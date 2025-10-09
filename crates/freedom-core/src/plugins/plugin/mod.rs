pub mod dylib;

use std::path::{Path, PathBuf};

use libloading::Symbol;
use log::info;
use steel::{
    SteelVal,
    rvals::{Custom, IntoSteelVal},
    steel_vm::{
        builtin::{Arity, BuiltInModule},
        engine::Engine,
    },
    stop,
};

use crate::Result;
use dylib::Dylib;

#[derive(Debug)]
pub struct FunctionSpec {
    pub symbol: &'static str,
    pub name: &'static str,
    pub arity: Arity,
}

#[derive(Debug)]
pub struct PluginSpec {
    pub name: &'static str,
    pub values: &'static [&'static str],
    pub functions: &'static [FunctionSpec],
}

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

        info!("Extracting plugin spec...");
        let sym: Symbol<fn() -> PluginSpec> =
            unsafe { lib.get(b"spec").or_else(|e| stop!(Io => e))? };

        let spec @ PluginSpec {
            name,
            values,
            functions,
        } = sym();

        info!("{spec:?}");

        info!("Building module...");
        let mut module = BuiltInModule::new(name);

        for symbol in values {
            let sym: Symbol<fn() -> SteelVal> =
                unsafe { lib.get(symbol.as_bytes()).or_else(|e| stop!(Io => e))? };
            let val = sym();
            info!("Registering {symbol} = {val}");
            module.register_value(symbol, val);
        }

        for spec @ FunctionSpec {
            symbol,
            name,
            arity,
        } in functions
        {
            info!("Registering function...");
            info!("{spec:?}");
            let sym: Symbol<fn(&[SteelVal]) -> Result<SteelVal>> =
                unsafe { lib.get(symbol.as_bytes()).or_else(|e| stop!(Io => e))? };
            module.register_native_fn(name, *sym, *arity);
        }

        info!("Checking for existing dylib...");
        let existing = engine.update_value(&format!("%-plugin-dylib-{}", name), SteelVal::Void);
        if existing.is_some() {
            info!("Dropped");
        }
        else {
            info!("Not loaded");
        }

        info!("Registering module...");
        engine.register_module(module);

        // Inject dylib into module for lifetime control
        info!("Emplacing new dylib...");
        engine.register_value(&format!("%-plugin-dylib-{}", name), lib.into_steelval()?);

        Ok(())
    }
}
