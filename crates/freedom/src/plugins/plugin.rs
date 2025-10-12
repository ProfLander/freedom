use std::path::{Path, PathBuf};

use log::debug;

use crate::{
    plugins::dylib::Dylib,
    scheme::{
        Result,
        steel::{rvals::Custom, steel_vm::builtin::BuiltInModule, steelerr},
    },
};

pub struct PluginInterface {
    pub init: fn(),
    pub module: fn() -> BuiltInModule,
}

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
        debug!("Plugin::load");
        let lib = Dylib::new(&self.path)?;

        debug!("Dropping existing dylib...");
        drop(self.loaded.take());

        debug!("Extracting plugin interface...");
        let plugin = unsafe { lib.get::<fn() -> PluginInterface>(b"plugin") };
        let plugin = plugin.or_else(|e| steelerr!(Generic => e))?;
        let PluginInterface { init, module } = plugin();

        debug!("Initializing...");
        init();

        debug!("Constructing module...");
        let module = module();

        // Inject dylib into module for lifetime control
        drop(self.loaded.replace((module, lib)));

        Ok(())
    }
}
