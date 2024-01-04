use alloc::{vec, vec::Vec};
use frostbite_bytecode::{
    BytecodeVersion, ConstantValue, Function, FunctionIndex, Globals, Instruction, Manifest, Module,
};
use frostbite_parser::ast::{tokens::BinaryOperatorKind, Spanned};
use frostbite_reports::ReportContext;
use slotmap::SecondaryMap;

use crate::tir::{self, FunctionType, Type, TypedAst, TypedExpression, TypedFunctionExpr};

use super::CodegenBackend;

#[derive(Debug, Default)]
pub struct BytecodeCodegenBackend {
    functions: SecondaryMap<tir::TypeIndex, FunctionIndex>,
}

impl BytecodeCodegenBackend {
    fn compile_program(&mut self, t_ast: &TypedAst, module: &mut Module) {
        let body = &mut module.body;
        let globals = &mut module.globals;

        for node in &t_ast.nodes {
            self.compile_node(t_ast, body, globals, node);
        }
    }

    fn compile_node(
        &mut self,
        t_ast: &TypedAst,
        instructions: &mut Vec<Instruction>,
        globals: &mut Globals,
        t_expr: &TypedExpression,
    ) {
        match t_expr {
            TypedExpression::Int(..)
            | TypedExpression::Float(..)
            | TypedExpression::Bool(..)
            | TypedExpression::String(..) => self.compile_constant(instructions, globals, t_expr),

            TypedExpression::Ident {
                type_index: _,
                refers_to: _,
                str_value: Spanned(_, name),
            } => {
                instructions.push(Instruction::LoadName(name.into()));
            }

            TypedExpression::BinaryOperation { lhs, operator, rhs } => {
                self.compile_binary_operation(t_ast, instructions, globals, lhs, operator.kind, rhs)
            }

            TypedExpression::Assign {
                local_index: _,
                lhs,
                value,
            } => {
                self.compile_assignment(t_ast, globals, instructions, lhs, value);
            }

            TypedExpression::Function(function_expr) => {
                self.compile_function_node(t_ast, globals, function_expr)
            }

            TypedExpression::Call {
                callee, arguments, ..
            } => self.compile_function_call(t_ast, globals, instructions, callee, arguments),

            TypedExpression::Return(_, _, return_value) => {
                if let Some(value) = return_value {
                    self.compile_node(t_ast, instructions, globals, value);
                } else {
                    instructions.push(Instruction::LoadConstant(
                        globals.constants_pool.insert(ConstantValue::Unit),
                    ));
                }

                instructions.push(Instruction::Return);
            }

            TypedExpression::Block { expressions, .. } => expressions
                .iter()
                .for_each(|expr| self.compile_node(t_ast, instructions, globals, expr)),

            TypedExpression::Poisoned | TypedExpression::Uninitialized => unreachable!(),
        }
    }

    fn compile_constant(
        &mut self,
        instructions: &mut Vec<Instruction>,
        globals: &mut Globals,
        node: &TypedExpression,
    ) {
        let constant_index = globals.constants_pool.insert(match node {
            TypedExpression::Int(Spanned(_, constant)) => (*constant).into(),
            TypedExpression::Float(Spanned(_, constant)) => (*constant).into(),
            TypedExpression::Bool(Spanned(_, bool)) => (*bool).into(),
            TypedExpression::String(Spanned(_, constant)) => constant.clone().into(),

            _ => unreachable!(),
        });

        instructions.push(Instruction::LoadConstant(constant_index));
    }

    fn compile_binary_operation(
        &mut self,
        t_ast: &TypedAst,
        instructions: &mut Vec<Instruction>,
        globals: &mut Globals,
        lhs: &TypedExpression,
        operator: BinaryOperatorKind,
        rhs: &TypedExpression,
    ) {
        self.compile_node(t_ast, instructions, globals, rhs);
        self.compile_node(t_ast, instructions, globals, lhs);

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

                    instructions.push(Instruction::CallEq(tmp_fn_idx))
                }

                {
                    let const_val_idx = globals.constants_pool.insert(ConstantValue::Bool(false));

                    let tmp_fn_idx = globals.functions.insert(Self::compile_function_with_body(
                        [Instruction::LoadConstant(const_val_idx)].into(),
                    ));

                    instructions.push(Instruction::CallNe(tmp_fn_idx))
                }
            }
        };
    }

    fn compile_assignment(
        &mut self,
        t_ast: &TypedAst,
        globals: &mut Globals,
        instructions: &mut Vec<Instruction>,
        lhs: &tir::Assignable,
        value: &TypedExpression,
    ) {
        self.compile_node(t_ast, instructions, globals, value);

        match lhs {
            tir::Assignable::Ident(_, Spanned(_, name)) => {
                instructions.push(Instruction::StoreName(name.into()));
            }
        };
    }

    fn compile_function_node(
        &mut self,
        t_ast: &TypedAst,
        globals: &mut Globals,
        function: &TypedFunctionExpr,
    ) {
        let dummy_function_index = globals.functions.insert(Function { body: vec![] });

        self.functions
            .insert(function.type_index, dummy_function_index);

        globals.functions[dummy_function_index] =
            self.compile_function(t_ast, globals, &function.body);
    }

    fn compile_function(
        &mut self,
        t_ast: &TypedAst,
        globals: &mut Globals,
        function_body: &TypedExpression,
    ) -> frostbite_bytecode::Function {
        let mut bytecode_function_body = Vec::new();

        self.compile_node(t_ast, &mut bytecode_function_body, globals, function_body);

        BytecodeCodegenBackend::compile_function_with_body(bytecode_function_body)
    }

    /// Caller must guarantee that the return type is on the stack before calling this function
    fn compile_function_with_body(mut body: Vec<Instruction>) -> frostbite_bytecode::Function {
        body.push(Instruction::Return);

        Function { body }
    }

    fn compile_function_call(
        &mut self,
        t_ast: &TypedAst,
        globals: &mut Globals,
        instructions: &mut Vec<Instruction>,
        callee: &tir::Callable,
        arguments_exprs: &[TypedExpression],
    ) {
        let function_type = callee.calling_function_type();

        let Type::Function(FunctionType {
            arguments,
            return_type: _,
        }) = &t_ast.types_arena[function_type]
        else {
            unreachable!()
        };

        Iterator::zip(arguments.keys(), arguments_exprs.iter()).for_each(
            |(argument_name, argument_expr)| {
                self.compile_node(t_ast, instructions, globals, argument_expr);
                instructions.push(Instruction::StoreName(argument_name.into()));
            },
        );

        match callee {
            tir::Callable::Ident(type_index, _) => {
                let function_index = self.functions[*type_index];

                instructions.push(Instruction::Call(function_index));
            }
        };
    }
}

impl CodegenBackend for BytecodeCodegenBackend {
    type Output = Module;

    fn codegen(mut self, _report_ctx: &mut ReportContext, t_ast: &TypedAst) -> Self::Output {
        let mut module = Module {
            manifest: Manifest {
                bytecode_version: BytecodeVersion::Experimental(),
            },
            globals: Globals::default(),
            body: Vec::new(),
        };

        self.compile_program(t_ast, &mut module);

        module
    }
}
