use alloc::fmt;
use owo_colors::OwoColorize;

use crate::{Instruction, Module};

const INSTRUCTION_PADDING: usize = 15;
const CONSTANT_PADDING: usize = 20;

pub fn print_bytecode<W>(
    w: &mut W,
    module: &Module,
) -> fmt::Result
where
    W: fmt::Write,
{
    writeln!(
        w,
        "compiled by frostbite-compiler@{}",
        module.manifest.emitted_by_compiler_version.yellow()
    )?;

    writeln!(w)?;
    writeln!(w, "{}", "Global Constants".bold().underline())?;

    module
        .globals
        .constants_pool
        .iter()
        .try_for_each(|(index, constant)| {
            writeln!(
                w,
                "{:>CONSTANT_PADDING$} -> {}",
                index.cyan(),
                constant.yellow()
            )
        })?;

    writeln!(w)?;

    module
        .globals
        .functions
        .iter()
        .try_for_each(|(index, function)| {
            writeln!(
                w,
                "{} {}",
                "Function at pool index".bold().underline(),
                index.bold().cyan()
            )?;

            function
                .body
                .iter()
                .try_for_each(|instruction| print_instruction(w, instruction, module))?;

            writeln!(w)?;

            Ok(())
        })?;

    writeln!(w, "{}", "Program body".bold().underline())?;

    module
        .body
        .iter()
        .try_for_each(|instruction| print_instruction(w, instruction, module))?;

    Ok(())
}

fn print_instruction<W>(
    w: &mut W,
    instruction: &Instruction,
    module: &Module,
) -> fmt::Result
where
    W: fmt::Write,
{
    // writeln!(
    //     w,
    //     "# {:>INSTRUCTION_PADDING$}",
    //     instruction.description().bright_white().underline()
    // )?;

    write!(
        w,
        "{:>INSTRUCTION_PADDING$} ",
        serde_variant::to_variant_name(instruction)
            .unwrap()
            .bright_red()
    )?;

    match instruction {
        Instruction::LoadConstant(index) => write!(
            w,
            "{} // {}",
            index.cyan(),
            module.globals.constants_pool[*index]
        )?,
        Instruction::Import { module } => {
            write!(w, "{module}")?;
        }
        Instruction::ImportFromModule { module, symbol } => {
            write!(w, "{module}")?;

            write!(w, "{symbol}")?;
        }
        Instruction::LoadFunction(index) => {
            write!(w, "{}", index.cyan())?;
        }
        Instruction::LoadBuiltin(name) => {
            write!(w, "builtin {}", name.cyan())?;
        }
        Instruction::StoreName(name) => write!(w, "{:?}", name.bright_yellow())?,
        Instruction::LoadName(name) => write!(w, "{:?}", name.bright_yellow())?,
        Instruction::PopAndStoreName(name) => {
            write!(w, "{0} // pops and stores onto {0}", name.cyan())?
        }

        Instruction::Pop
        | Instruction::Call
        | Instruction::CallEq
        | Instruction::CallNe
        | Instruction::Return
        | Instruction::Add
        | Instruction::Subtract
        | Instruction::Multiply
        | Instruction::Divide
        | Instruction::Cmp
        | Instruction::Nop => (),
    }

    writeln!(w)?;

    Ok(())
}
