pub mod plugin;

use std::{
    cell::{OnceCell, RefCell},
    collections::BTreeMap,
    path::{Path, PathBuf},
    time::Duration,
};

use log::info;
use notify_debouncer_full::notify::{EventKind, RecursiveMode};
use steel::{
    SteelVal,
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
    steelerr, throw,
};

use crate::{ENGINE, Result, fs::Watcher, handle_error, plugins::plugin::Plugin};

thread_local! {
    static PLUGINS: OnceCell<Plugins> = OnceCell::new();
}

pub fn init(dir: &str) -> Result<()> {
    // Construct plugins
    let plugins = Plugins::new(&Path::new(dir))?;

    ENGINE.with(|engine| {
        let engine = &mut engine.borrow_mut();
        engine.register_fn("#%get-plugin", get_plugin);
        Ok(()) as Result<()>
    })?;

    PLUGINS.with(|cell| {
        cell.set(plugins)
            .ok()
            .expect("Failed to initialize plugins")
    });

    Ok(())
}

pub fn get_plugin(name: String) -> Result<BuiltInModule> {
    PLUGINS.with(|plugins| {
        let plugins = plugins.get().unwrap();
        let mut path = plugins.search_path.clone();
        let fname = libloading::library_filename(&name);
        path.push(fname);

        let mut plugins = plugins.plugins.borrow_mut();
        let plugin = plugins
            .entry(name.clone())
            .or_insert_with(|| Plugin::new(path).unwrap());

        Ok(plugin.module().unwrap().clone())
    })
}

// Thread-local collection of plugins
pub struct Plugins {
    search_path: PathBuf,
    pub(crate) plugins: RefCell<BTreeMap<String, Plugin>>,
    _watcher: Watcher,
}

impl Plugins {
    pub fn new(search_path: &Path) -> Result<Self> {
        let search_path = search_path.to_path_buf();
        let watcher = Self::watch(search_path.clone())?;

        Ok(Plugins {
            search_path,
            plugins: RefCell::new(Default::default()),
            _watcher: watcher,
        })
    }

    pub fn watch<P>(path: P) -> Result<Watcher>
    where
        P: AsRef<Path> + 'static,
    {
        let path = path.as_ref().to_path_buf();
        let debouncer = crate::fs::watch(
            path.clone(),
            Duration::from_secs_f32(0.1),
            None,
            RecursiveMode::NonRecursive,
            move |event| {
                handle_error(match event.event.kind {
                    EventKind::Modify(_) => PLUGINS.with(move |plugins| {
                        for key in &event.paths {
                            for (_, plugin) in plugins
                                .get()
                                .ok_or_else(throw!(Generic => "Plugins have not been initialized"))?
                                .plugins
                                .borrow_mut()
                                .iter_mut()
                            {
                                if key.ends_with(&plugin.path()) {
                                    info!("Reloading {:?}", plugin.path());
                                    plugin.load()?;
                                }
                            }
                        }

                        Ok(())
                    }),
                    _ => Ok(()),
                })
            },
        )
        .or_else(|e| steelerr!(Io => e))?;

        Ok(debouncer)
    }
}
