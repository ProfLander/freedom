use crate::scheme::steel::{compiler::program::RawProgramWithSymbols, rvals::Custom};

#[derive(Clone)]
pub struct Program(RawProgramWithSymbols);

impl Program {
    pub fn new(prog: RawProgramWithSymbols) -> Self {
        Program(prog)
    }

    pub fn unwrap(self) -> RawProgramWithSymbols {
        self.0
    }
}

impl Custom for Program {}
