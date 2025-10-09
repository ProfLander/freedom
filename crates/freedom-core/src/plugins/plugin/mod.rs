pub mod dylib;

use std::path::{Path, PathBuf};

use libloading::Symbol;
use log::info;
use steel::{
    SteelVal,
    rvals::{Custom, IntoSteelVal},
    steel_vm::{builtin::BuiltInModule, engine::Engine},
    stop, throw,
};

use crate::Result;
use dylib::Dylib;

// Interface to a reloadable rust dylib exposing a Steel module
pub struct Plugin {
    pub path: PathBuf,
}

impl Custom for Plugin {}

impl Plugin {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self> {
        Ok(Plugin {
            path: path.as_ref().to_path_buf(),
        })
    }

    pub fn reload(&self, engine: &mut Engine) -> Result<()> {
        info!("Plugin::module");
        let lib = Dylib::new(&self.path)?;

        let name = self
            .path
            .file_stem()
            .ok_or_else(throw!(Generic => "Path has no file stem: {:?}", self.path))?
            .to_str()
            .ok_or_else(
                throw!(Generic => "Failed to convert path to string slice: {:?}", self.path),
            )?;

        info!("Dropping existing dylib...");
        let _ = engine.update_value(&format!("%-plugin-dylib-{}", name), SteelVal::Void);

        info!("Extracting module symbol...");
        let sym: Symbol<fn() -> BuiltInModule> =
            unsafe { lib.get(b"module").or_else(|e| stop!(Io => e))? };

        info!("Registering module...");
        engine.register_module(sym());

        // Inject dylib into module for lifetime control
        info!("Emplacing new dylib...");
        engine.register_value(&format!("%-plugin-dylib-{}", name), lib.into_steelval()?);

        Ok(())
    }
}
