#![allow(unsafe_code)]

use alloc::{vec, vec::Vec};
use permafrost_ast::{tokens::BinaryOperatorKind, Spanned};
use permafrost_bytecode::{
    ConstantValue, Function, FunctionKey, Globals, Instruction, Manifest, Module,
};
use permafrost_reports::sourcemap::SourceKey;
use slotmap::SecondaryMap;

use crate::{
    context::{CompilerContext, TypeContext},
    ir::typed::{
        Assignable, Callable, FunctionType, Type, TypeKey, TypedAst, TypedExpression,
        TypedExpressionKind, TypedFunction,
    },
};

use super::{CodegenBackend, CodegenOutput, PrintableCodegenOutput, SerializationError};

impl CodegenBackend for BytecodeCodegenBackend
{
    type Output = Module;

    fn codegen(
        &mut self,
        source_key: SourceKey,
        compiler_ctx: &mut CompilerContext,
    ) -> Self::Output
    {
        let mut module = Module {
            manifest: Manifest {
                emitted_by_compiler_version: option_env!("PROJECT_VERSION")
                    .unwrap_or(env!("CARGO_PKG_VERSION"))
                    .parse()
                    .unwrap(),
            },
            globals: Globals::default(),
            body: Vec::new(),
        };

        let t_ast = compiler_ctx.type_ctx.get_ast(source_key);

        self.compile_program(t_ast, &compiler_ctx.type_ctx, &mut module);

        module
    }
}

#[derive(Debug, Default)]
pub struct BytecodeCodegenBackend
{
    functions: SecondaryMap<TypeKey, FunctionKey>,
}

impl BytecodeCodegenBackend
{
    pub fn new() -> Self
    {
        Self {
            functions: SecondaryMap::new(),
        }
    }

    /// Compile a program
    fn compile_program(
        &mut self,
        t_ast: &TypedAst,
        type_ctx: &TypeContext,
        module: &mut Module,
    )
    {
        let body = &mut module.body;
        let globals = &mut module.globals;

        // compile each node in the program
        t_ast.nodes.iter().for_each(|node| {
            self.compile_node(t_ast, type_ctx, body, globals, node);
        });
    }

    /// compiles a node
    ///
    /// # ASSUMPTIONS
    ///
    /// Every node compiled has to perform some kind of operation/load onto the stack something
    fn compile_node(
        &mut self,
        t_ast: &TypedAst,
        type_ctx: &TypeContext,
        instructions: &mut Vec<Instruction>,
        globals: &mut Globals,
        t_expr: &TypedExpression,
    )
    {
        match t_expr.kind {
            TypedExpressionKind::Int(..)
            | TypedExpressionKind::Float(..)
            | TypedExpressionKind::Bool(..)
            | TypedExpressionKind::String(..) => {
                self.compile_constant(instructions, globals, t_expr)
            }

            TypedExpressionKind::ModuleStatement(..) => {}

            TypedExpressionKind::Ident {
                str_value: Spanned(_, ref name),
            } => {
                instructions.push(Instruction::LoadName(name.into()));
            }

            TypedExpressionKind::BinaryOperation {
                ref lhs,
                ref operator,
                ref rhs,
            } => self.compile_binary_operation(
                t_ast,
                type_ctx,
                instructions,
                globals,
                lhs,
                operator.kind,
                rhs,
            ),

            TypedExpressionKind::Assign { ref lhs, ref value } => {
                self.compile_assignment(t_ast, type_ctx, globals, instructions, lhs, value);
            }

            TypedExpressionKind::Function(ref function_expr) => self.compile_function_node(
                t_ast,
                type_ctx,
                globals,
                instructions,
                t_expr.type_key,
                function_expr,
            ),

            TypedExpressionKind::Call {
                ref callee,
                ref arguments,
                ..
            } => self.compile_function_call(
                t_ast,
                type_ctx,
                globals,
                instructions,
                callee,
                arguments,
            ),

            TypedExpressionKind::Return(_, _, ref return_value) => {
                if let Some(value) = return_value {
                    self.compile_node(t_ast, type_ctx, instructions, globals, value);
                } else {
                    instructions.push(Instruction::LoadConstant(
                        globals.constants_pool.insert(ConstantValue::Unit),
                    ));
                }

                instructions.push(Instruction::Return);
            }

            TypedExpressionKind::Block {
                ref expressions, ..
            } => expressions
                .iter()
                .for_each(|expr| self.compile_node(t_ast, type_ctx, instructions, globals, expr)),
        }
    }

    /// Converts a literal into bytecode
    fn compile_constant(
        &mut self,
        instructions: &mut Vec<Instruction>,
        globals: &mut Globals,
        node: &TypedExpression,
    )
    {
        let constant_index = globals.constants_pool.insert(match &node.kind {
            TypedExpressionKind::Int(Spanned(_, constant)) => (*constant).into(),
            TypedExpressionKind::Float(Spanned(_, constant)) => (*constant).into(),
            TypedExpressionKind::Bool(Spanned(_, bool)) => (*bool).into(),
            TypedExpressionKind::String(Spanned(_, constant)) => constant.clone().into(),

            _ => unreachable!(),
        });

        instructions.push(Instruction::LoadConstant(constant_index));
    }

    fn compile_binary_operation(
        &mut self,
        t_ast: &TypedAst,
        type_ctx: &TypeContext,
        instructions: &mut Vec<Instruction>,
        globals: &mut Globals,
        lhs: &TypedExpression,
        operator: BinaryOperatorKind,
        rhs: &TypedExpression,
    )
    {
        self.compile_node(t_ast, type_ctx, instructions, globals, rhs);
        self.compile_node(t_ast, type_ctx, instructions, globals, lhs);

        match operator {
            BinaryOperatorKind::Add => instructions.push(Instruction::Add),
            BinaryOperatorKind::Sub => instructions.push(Instruction::Subtract),
            BinaryOperatorKind::Mul => instructions.push(Instruction::Multiply),
            BinaryOperatorKind::Div => instructions.push(Instruction::Divide),
            BinaryOperatorKind::Equal => {
                self.compile_equal_binary_op(globals, instructions);
            }
        };
    }

    fn compile_equal_binary_op(
        &mut self,
        globals: &mut Globals,
        instructions: &mut Vec<Instruction>,
    )
    {
        instructions.push(Instruction::Cmp);

        // binary operations must produce the result on the stack
        // we can achieve this using function calls pushing a result on the stack

        {
            let const_val_idx = globals.constants_pool.insert(ConstantValue::Bool(true));

            let tmp_fn_idx = globals.functions.insert(unsafe {
                Self::compile_function_with_body([Instruction::LoadConstant(const_val_idx)].into())
            });

            instructions.push(Instruction::LoadFunction(tmp_fn_idx));

            instructions.push(Instruction::CallEq)
        }

        {
            let const_val_idx = globals.constants_pool.insert(ConstantValue::Bool(false));

            let tmp_fn_idx = globals.functions.insert(unsafe {
                Self::compile_function_with_body([Instruction::LoadConstant(const_val_idx)].into())
            });

            instructions.push(Instruction::LoadFunction(tmp_fn_idx));

            instructions.push(Instruction::CallNe)
        }
    }

    fn compile_assignment(
        &mut self,
        t_ast: &TypedAst,
        type_ctx: &TypeContext,
        globals: &mut Globals,
        instructions: &mut Vec<Instruction>,
        lhs: &Assignable,
        value: &TypedExpression,
    )
    {
        self.compile_node(t_ast, type_ctx, instructions, globals, value);

        match lhs {
            Assignable::Ident(_, Spanned(_, name)) => {
                instructions.push(Instruction::StoreName(name.into()));
            }
        };
    }

    fn compile_function_node(
        &mut self,
        t_ast: &TypedAst,
        type_ctx: &TypeContext,
        globals: &mut Globals,
        instructions: &mut Vec<Instruction>,
        type_key: TypeKey,
        function: &TypedFunction,
    )
    {
        let dummy_function_index = globals.functions.insert(Function { body: vec![] });

        self.functions.insert(type_key, dummy_function_index);

        globals.functions[dummy_function_index] =
            self.compile_function(t_ast, type_ctx, globals, function);

        instructions.push(Instruction::LoadFunction(dummy_function_index));

        if let Some(Spanned(_, ref name)) = function.name {
            instructions.push(Instruction::StoreName(name.clone()));
        }
    }

    fn compile_function(
        &mut self,
        t_ast: &TypedAst,
        type_ctx: &TypeContext,
        globals: &mut Globals,
        function: &TypedFunction,
    ) -> permafrost_bytecode::Function
    {
        let mut bytecode_function_body = Vec::new();

        function.arguments.keys().rev().for_each(|argument_name| {
            bytecode_function_body.push(Instruction::PopAndStoreName(argument_name.into()))
        });

        self.compile_node(
            t_ast,
            type_ctx,
            &mut bytecode_function_body,
            globals,
            &function.body,
        );

        unsafe { BytecodeCodegenBackend::compile_function_with_body(bytecode_function_body) }
    }

    /// Caller must guarantee that the return type is on the stack before calling this function
    unsafe fn compile_function_with_body(
        mut body: Vec<Instruction>
    ) -> permafrost_bytecode::Function
    {
        body.push(Instruction::Return);

        Function { body }
    }

    fn compile_function_call(
        &mut self,
        t_ast: &TypedAst,
        type_ctx: &TypeContext,
        globals: &mut Globals,
        instructions: &mut Vec<Instruction>,
        callee: &Callable,
        arguments_exprs: &[TypedExpression],
    )
    {
        match callee {
            Callable::Function(type_key, _) => {
                let Type::Function(FunctionType {
                    arguments: _,
                    return_type: _,
                }) = type_ctx.get_type(*type_key)
                else {
                    unreachable!()
                };

                arguments_exprs.iter().for_each(|argument_expr| {
                    self.compile_node(t_ast, type_ctx, instructions, globals, argument_expr);
                });

                let function_index = self.functions[*type_key];

                instructions.push(Instruction::LoadFunction(function_index));

                instructions.push(Instruction::Call);
            }
        }
    }
}

impl CodegenOutput for Module
{
    fn serialize(
        &self,
        buf: &mut Vec<u8>,
    )
    {
        permafrost_bytecode::encode(self, buf);
    }

    fn deserialize(buf: &[u8]) -> Result<Self, SerializationError>
    {
        permafrost_bytecode::decode(buf).map_err(|_| SerializationError)
    }

    fn as_printable(&self) -> Option<&dyn super::PrintableCodegenOutput>
    {
        Some(self)
    }
}

impl PrintableCodegenOutput for Module
{
    fn print(
        &self,
        buf: &mut alloc::string::String,
    ) -> alloc::fmt::Result
    {
        permafrost_bytecode::text_repr::print_bytecode(buf, self)
    }
}
