use std::ffi::OsStr;

use libloading::Library as LibraryBase;
use steel::{
    SteelVal,
    rvals::Custom,
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
    steelerr,
};

use crate::Result;

pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/loading");
    module.register_fn("#%library-filename", |name: String| {
        libloading::library_filename(name)
            .to_string_lossy()
            .to_string()
    });
    module.register_fn("Library", Library::new::<String>);
    module.register_fn("Library-get", Library::get);
    module
}

pub struct Library(LibraryBase);

impl Custom for Library {}

impl Library {
    pub fn new<P: AsRef<OsStr>>(filename: P) -> Result<Self> {
        Ok(Library(unsafe {
            LibraryBase::new(filename).or_else(|e| steelerr!(Generic => e))?
        }))
    }

    pub fn get(&self, symbol: String) -> Result<SteelVal> {
        unsafe {
            let func = self
                .0
                .get::<fn() -> Result<SteelVal>>(symbol.as_bytes())
                .or_else(|e| steelerr!(Generic => e))?;
            func()
        }
    }
}
