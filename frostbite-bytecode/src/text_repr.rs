use alloc::fmt;
use variant_name::VariantName;

use crate::{Instruction, Module};

pub fn print_bytecode<W>(w: &mut W, module: &Module) -> fmt::Result
where
    W: fmt::Write,
{
    writeln!(w, "Bytecode version {}", module.manifest.bytecode_version)?;

    module
        .globals
        .constants_pool
        .iter()
        .try_for_each(|(index, constant)| writeln!(w, "{:?}: {}", index, constant))?;

    module
        .globals
        .functions
        .iter()
        .try_for_each(|(index, function)| {
            writeln!(w, "function at pool index {:?}", index)?;

            function
                .body
                .iter()
                .try_for_each(|instruction| print_instruction(w, instruction))?;

            Ok(())
        })?;

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
    write!(w, "{:>8}", instruction.variant_name())?;

    if let Instruction::Load(index) | Instruction::Call(index) = instruction {
        write!(w, "{index:?}")?;
    }

    Ok(())
}
