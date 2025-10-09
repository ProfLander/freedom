pub mod plugin;

use std::{
    cell::{OnceCell, RefCell},
    collections::BTreeMap,
    path::Path,
    time::Duration,
};

use log::info;
use notify_debouncer_full::notify::{EventKind, RecursiveMode};
use steel::{steelerr, throw};

use crate::{ENGINE, Result, fs::Watcher, handle_error, plugins::plugin::Plugin, with_engine_mut};

thread_local! {
    static PLUGINS: OnceCell<Plugins> = OnceCell::new();
}

pub fn init(dir: &str, names: Vec<String>) -> Result<()> {
    // Construct plugins
    let plugins = Plugins::new(&Path::new(dir), names)?;

    ENGINE.with(|engine| {
        let engine = &mut engine.borrow_mut();
        for (_, v) in plugins.plugins.borrow().iter() {
            v.reload(engine)?;
        }
        Ok(()) as Result<()>
    })?;

    PLUGINS.with(|cell| {
        cell.set(plugins)
            .ok()
            .expect("Failed to initialize plugins")
    });

    Ok(())
}

// Thread-local collection of plugins
pub struct Plugins {
    pub(crate) plugins: RefCell<BTreeMap<String, Plugin>>,
    _watcher: Watcher,
}

impl Plugins {
    pub fn new(search_path: &Path, names: Vec<String>) -> Result<Self> {
        let plugins = names
            .into_iter()
            .try_fold(BTreeMap::default(), |mut acc, next| {
                let mut path = search_path.to_path_buf();
                let fname = libloading::library_filename(&next);
                path.push(fname);
                let path = path
                    .to_str()
                    .ok_or_else(
                        throw!(Generic => "Failed to convert path to string slice: {:?}", path),
                    )?
                    .to_string();

                let plugin = Plugin::new(&path)?;
                acc.insert(path, plugin);
                Ok(acc) as Result<_>
            })?;

        let watcher = Self::watch(search_path.to_path_buf())?;

        Ok(Plugins {
            plugins: RefCell::new(plugins),
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
                                .borrow()
                                .iter()
                            {
                                if key.ends_with(&plugin.path) {
                                    info!("Reloading {:?}", plugin.path);
                                    with_engine_mut(|engine| plugin.reload(engine))?;
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
