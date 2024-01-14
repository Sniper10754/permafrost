use alloc::{string::ToString, vec, vec::Vec};
use frostbite_bytecode::{
    ConstantValue, Function, FunctionKey, Globals, Instruction, Manifest, Module,
};
use frostbite_parser::ast::{tokens::BinaryOperatorKind, Spanned};
use frostbite_reports::sourcemap::SourceId;
use slotmap::SecondaryMap;

use crate::{
    context::CompilerContext,
    tir::{
        self, FunctionType, ImportDirectiveKind, Type, TypedAst, TypedExpression,
        TypedExpressionKind, TypedFunction, TypesArena,
    },
};

use super::CodegenBackend;

#[derive(Debug, Default)]
pub struct BytecodeCodegenBackend
{
    functions: SecondaryMap<tir::TypeKey, FunctionKey>,
}

impl BytecodeCodegenBackend
{
    fn compile_program(
        &mut self,
        t_ast: &TypedAst,
        types_arena: &TypesArena,
        module: &mut Module,
    )
    {
        let body = &mut module.body;
        let globals = &mut module.globals;

        for node in &t_ast.nodes {
            self.compile_node(t_ast, types_arena, body, globals, node);
        }
    }

    fn compile_node(
        &mut self,
        t_ast: &TypedAst,
        types_arena: &TypesArena,
        instructions: &mut Vec<Instruction>,
        globals: &mut Globals,
        t_expr: &TypedExpression,
    )
    {
        match &t_expr.typed_expression_kind {
            TypedExpressionKind::Int(..)
            | TypedExpressionKind::Float(..)
            | TypedExpressionKind::Bool(..)
            | TypedExpressionKind::String(..) => {
                self.compile_constant(instructions, globals, t_expr)
            }

            TypedExpressionKind::ImportDirective(Spanned(_, import_directive)) => {
                self.compile_import_directive(instructions, import_directive)
            }

            TypedExpressionKind::Ident {
                refers_to: _,
                str_value: Spanned(_, name),
            } => {
                instructions.push(Instruction::LoadName(name.into()));
            }

            TypedExpressionKind::BinaryOperation { lhs, operator, rhs } => self
                .compile_binary_operation(
                    t_ast,
                    types_arena,
                    instructions,
                    globals,
                    lhs,
                    operator.kind,
                    rhs,
                ),

            TypedExpressionKind::Assign { lhs, value } => {
                self.compile_assignment(t_ast, types_arena, globals, instructions, lhs, value);
            }

            TypedExpressionKind::Function(function_expr) => self.compile_function_node(
                t_ast,
                types_arena,
                globals,
                instructions,
                t_expr.type_key,
                function_expr,
            ),

            TypedExpressionKind::Call {
                callee, arguments, ..
            } => self.compile_function_call(
                t_ast,
                types_arena,
                globals,
                instructions,
                callee,
                arguments,
            ),

            TypedExpressionKind::Return(_, _, return_value) => {
                if let Some(value) = return_value {
                    self.compile_node(t_ast, types_arena, instructions, globals, value);
                } else {
                    instructions.push(Instruction::LoadConstant(
                        globals.constants_pool.insert(ConstantValue::Unit),
                    ));
                }

                instructions.push(Instruction::Return);
            }

            TypedExpressionKind::Block { expressions, .. } => expressions.iter().for_each(|expr| {
                self.compile_node(t_ast, types_arena, instructions, globals, expr)
            }),
        }
    }

    fn compile_constant(
        &mut self,
        instructions: &mut Vec<Instruction>,
        globals: &mut Globals,
        node: &TypedExpression,
    )
    {
        let constant_index = globals
            .constants_pool
            .insert(match &node.typed_expression_kind {
                TypedExpressionKind::Int(Spanned(_, constant)) => (*constant).into(),
                TypedExpressionKind::Float(Spanned(_, constant)) => (*constant).into(),
                TypedExpressionKind::Bool(Spanned(_, bool)) => (*bool).into(),
                TypedExpressionKind::String(Spanned(_, constant)) => constant.clone().into(),

                _ => unreachable!(),
            });

        instructions.push(Instruction::LoadConstant(constant_index));
    }

    fn compile_import_directive(
        &mut self,
        instructions: &mut Vec<Instruction>,
        import_directive: &ImportDirectiveKind,
    )
    {
        match import_directive {
            ImportDirectiveKind::FromModuleImportSymbol { module, symbol } => {
                instructions.push(Instruction::ImportFromModule {
                    module: module.to_string(),
                    symbol: symbol.clone(),
                })
            }
            ImportDirectiveKind::ImportModule { module } => {
                instructions.push(Instruction::Import {
                    module: module.to_string(),
                })
            }
        }
    }

    fn compile_binary_operation(
        &mut self,
        t_ast: &TypedAst,
        types_arena: &TypesArena,
        instructions: &mut Vec<Instruction>,
        globals: &mut Globals,
        lhs: &TypedExpression,
        operator: BinaryOperatorKind,
        rhs: &TypedExpression,
    )
    {
        self.compile_node(t_ast, types_arena, instructions, globals, rhs);
        self.compile_node(t_ast, types_arena, instructions, globals, lhs);

        match operator {
            BinaryOperatorKind::Add => instructions.push(Instruction::Add),
            BinaryOperatorKind::Sub => instructions.push(Instruction::Subtract),
            BinaryOperatorKind::Mul => instructions.push(Instruction::Multiply),
            BinaryOperatorKind::Div => instructions.push(Instruction::Divide),
            BinaryOperatorKind::Equal => {
                instructions.push(Instruction::Cmp);

                // binary operations must produce the result on the stack
                // we can achieve this using function calls pushing a result on the stack

                {
                    let const_val_idx = globals.constants_pool.insert(ConstantValue::Bool(true));

                    let tmp_fn_idx = globals.functions.insert(Self::compile_function_with_body(
                        [Instruction::LoadConstant(const_val_idx)].into(),
                    ));

                    instructions.push(Instruction::LoadFunction(tmp_fn_idx));

                    instructions.push(Instruction::CallEq)
                }

                {
                    let const_val_idx = globals.constants_pool.insert(ConstantValue::Bool(false));

                    let tmp_fn_idx = globals.functions.insert(Self::compile_function_with_body(
                        [Instruction::LoadConstant(const_val_idx)].into(),
                    ));

                    instructions.push(Instruction::LoadFunction(tmp_fn_idx));

                    instructions.push(Instruction::CallNe)
                }
            }
        };
    }

    fn compile_assignment(
        &mut self,
        t_ast: &TypedAst,
        types_arena: &TypesArena,
        globals: &mut Globals,
        instructions: &mut Vec<Instruction>,
        lhs: &tir::Assignable,
        value: &TypedExpression,
    )
    {
        self.compile_node(t_ast, types_arena, instructions, globals, value);

        match lhs {
            tir::Assignable::Ident(_, Spanned(_, name)) => {
                instructions.push(Instruction::StoreName(name.into()));
            }
        };
    }

    fn compile_function_node(
        &mut self,
        t_ast: &TypedAst,
        types_arena: &TypesArena,
        globals: &mut Globals,
        instructions: &mut Vec<Instruction>,
        type_key: tir::TypeKey,
        function: &TypedFunction,
    )
    {
        let dummy_function_index = globals.functions.insert(Function { body: vec![] });

        self.functions.insert(type_key, dummy_function_index);

        globals.functions[dummy_function_index] =
            self.compile_function(t_ast, types_arena, globals, &function.body);

        instructions.push(Instruction::LoadFunction(dummy_function_index));

        if let Some(Spanned(_, name)) = &function.name {
            instructions.push(Instruction::StoreName(name.clone()));
        }
    }

    fn compile_function(
        &mut self,
        t_ast: &TypedAst,
        types_arena: &TypesArena,
        globals: &mut Globals,
        function_body: &TypedExpression,
    ) -> frostbite_bytecode::Function
    {
        let mut bytecode_function_body = Vec::new();

        self.compile_node(
            t_ast,
            types_arena,
            &mut bytecode_function_body,
            globals,
            function_body,
        );

        BytecodeCodegenBackend::compile_function_with_body(bytecode_function_body)
    }

    /// Caller must guarantee that the return type is on the stack before calling this function
    fn compile_function_with_body(mut body: Vec<Instruction>) -> frostbite_bytecode::Function
    {
        body.push(Instruction::Return);

        Function { body }
    }

    fn compile_function_call(
        &mut self,
        t_ast: &TypedAst,
        types_arena: &TypesArena,
        globals: &mut Globals,
        instructions: &mut Vec<Instruction>,
        callee: &tir::Callable,
        arguments_exprs: &[TypedExpression],
    )
    {
        match callee {
            tir::Callable::Function(refers_to, _) => {
                let function_type_key = refers_to.into_type(t_ast);

                let Type::Function(FunctionType {
                    arguments,
                    return_type: _,
                }) = &types_arena[function_type_key]
                else {
                    unreachable!()
                };

                Iterator::zip(arguments.keys(), arguments_exprs.iter()).for_each(
                    |(argument_name, argument_expr)| {
                        self.compile_node(t_ast, types_arena, instructions, globals, argument_expr);

                        instructions.push(Instruction::StoreName(argument_name.into()));
                    },
                );

                let function_index = self.functions[function_type_key];

                instructions.push(Instruction::LoadFunction(function_index));

                instructions.push(Instruction::Call);
            }
        }
    }
}

impl CodegenBackend for BytecodeCodegenBackend
{
    type Output = Module;

    fn codegen(
        mut self,
        source_id: SourceId,
        compiler_ctx: &mut CompilerContext,
    ) -> Self::Output
    {
        compiler_ctx
            .intrinsic_ctx
            .symbols
            .iter()
            .for_each(
                |(_name, type_key)| match &compiler_ctx.types_arena[*type_key] {
                    Type::Int => (),
                    Type::Float => (),
                    Type::String => (),
                    Type::Bool => (),
                    Type::Function(_) => todo!(),
                    Type::Unit => (),
                    Type::Object(_) => (),
                    Type::Any => (),
                },
            );

        let mut module = Module {
            manifest: Manifest {
                bytecode_version: option_env!("PROJECT_VERSION")
                    .unwrap_or(env!("CARGO_PKG_VERSION"))
                    .into(),
            },
            globals: Globals::default(),
            body: Vec::new(),
        };

        let t_ast = &compiler_ctx.t_asts[source_id];

        self.compile_program(t_ast, &compiler_ctx.types_arena, &mut module);

        module
    }
}
