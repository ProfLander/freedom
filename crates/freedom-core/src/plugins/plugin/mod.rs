pub mod dylib;

use std::path::{Path, PathBuf};

use libloading::Symbol;
use log::info;
use steel::{rvals::Custom, steel_vm::builtin::BuiltInModule, steelerr};

use crate::Result;
use dylib::Dylib;

// Interface to a reloadable rust dylib exposing a Steel module
pub struct Plugin {
    path: PathBuf,
    loaded: Option<(BuiltInModule, Dylib)>,
}

impl Custom for Plugin {}

impl Plugin {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self> {
        let mut plugin = Plugin {
            path: path.as_ref().to_path_buf(),
            loaded: None,
        };
        plugin.load()?;
        Ok(plugin)
    }

    pub fn path(&self) -> &Path {
        self.path.as_path()
    }

    pub fn module(&self) -> Option<&BuiltInModule> {
        self.loaded.as_ref().map(|(module, _)| module)
    }

    pub fn load(&mut self) -> Result<()> {
        info!("Plugin::module");
        let lib = Dylib::new(&self.path)?;

        info!("Dropping existing dylib...");
        drop(self.loaded.take());

        info!("Extracting init symbol...");
        if let Ok(init) = unsafe { lib.get::<fn()>(b"init") } {
            init();
        }

        info!("Extracting module symbol...");
        let module: Symbol<fn() -> BuiltInModule> =
            unsafe { lib.get(b"module").or_else(|e| steelerr!(Io => e))? };

        info!("Constructing module...");
        let module = module();

        // Inject dylib into module for lifetime control
        info!("Emplacing new dylib...");
        drop(self.loaded.replace((module, lib)));

        Ok(())
    }
}
