use core::fmt::Display;

#[derive(Debug, Default, dbg_pls::DebugPls)]
pub struct Registers
{
    pub pc: usize,
    pub eq: bool,
}

impl Display for Registers
{
    fn fmt(
        &self,
        f: &mut core::fmt::Formatter<'_>,
    ) -> core::fmt::Result
    {
        dbg_pls::pretty(self).fmt(f)
    }
}
