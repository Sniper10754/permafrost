use alloc::fmt;
use owo_colors::OwoColorize;
use variant_name::VariantName;

use crate::{Instruction, Module};

const INSTRUCTION_PADDING: usize = 15;
const NUM_PADDING: usize = usize::ilog10(usize::MAX) as _;

pub fn print_bytecode<W>(w: &mut W, module: &Module) -> fmt::Result
where
    W: fmt::Write,
{
    writeln!(w, "Bytecode version {}", module.manifest.bytecode_version)?;

    writeln!(w)?;

    module
        .globals
        .constants_pool
        .iter()
        .try_for_each(|(index, constant)| {
            writeln!(
                w,
                "{:>INSTRUCTION_PADDING$} -> {:>NUM_PADDING$}",
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
                .try_for_each(|instruction| print_instruction(w, instruction))?;

            Ok(())
        })?;

    writeln!(w)?;
    writeln!(w, "{}", "Program body".bold().underline())?;

    module
        .body
        .iter()
        .try_for_each(|instruction| print_instruction(w, instruction))?;

    Ok(())
}

fn print_instruction<W>(w: &mut W, instruction: &Instruction) -> fmt::Result
where
    W: fmt::Write,
{
    writeln!(
        w,
        "# {:>INSTRUCTION_PADDING$}",
        instruction.description().bright_white().underline()
    )?;

    write!(
        w,
        "{:>INSTRUCTION_PADDING$} ",
        instruction.variant_name().bright_red()
    )?;

    match instruction {
        Instruction::LoadConstant(index) => write!(w, "{}", index.cyan())?,
        Instruction::Call(index) => write!(w, "{}", index.cyan())?,
        Instruction::StoreName(name) => write!(w, "{:?}", name.bright_yellow())?,
        Instruction::LoadName(name) => write!(w, "{:?}", name.bright_yellow())?,

        Instruction::Pop
        | Instruction::Return
        | Instruction::Add
        | Instruction::Subtract
        | Instruction::Multiply
        | Instruction::Divide
        | Instruction::Nop => (),
    }

    writeln!(w)?;

    Ok(())
}
