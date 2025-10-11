pub mod dylib;
pub mod plugin;
pub mod plugins;

use std::{cell::OnceCell, ffi::OsStr, path::Path};

use crate::scheme::{
    Result,
    steel::steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
};

use crate::{plugins::plugin::Plugin, plugins::plugins::Plugins};

thread_local! {
    static PLUGINS: OnceCell<Plugins> = OnceCell::new();
}

pub fn init<P: AsRef<OsStr>>(dir: &P) -> Result<()> {
    // Construct plugins
    let plugins = Plugins::new(Path::new(dir))?;

    PLUGINS.with(|cell| {
        cell.set(plugins)
            .ok()
            .expect("Failed to initialize plugins")
    });

    Ok(())
}

pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/plugins");
    module.register_fn("#%get-plugin", get_plugin);
    module
}

pub fn get_plugin(name: String) -> Result<BuiltInModule> {
    PLUGINS.with(|plugins| {
        let plugins = plugins.get().unwrap();
        let mut path = plugins.search_path().to_path_buf();
        let fname = libloading::library_filename(&name);
        path.push(fname);

        let mut plugins = plugins.plugins.borrow_mut();
        let plugin = plugins
            .entry(name.clone())
            .or_insert_with(|| Plugin::new(path).unwrap());

        Ok(plugin.module().unwrap().clone())
    })
}
