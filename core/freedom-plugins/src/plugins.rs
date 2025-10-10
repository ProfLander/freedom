use std::{
    cell::RefCell,
    collections::BTreeMap,
    path::{Path, PathBuf},
    time::Duration,
};

use freedom_fs::{
    Watcher,
    notify_debouncer_full::notify::{EventKind, RecursiveMode},
};
use freedom_log::{handle_error, info};
use freedom_scheme::{
    Result,
    steel::{steelerr, throw},
};

use crate::{PLUGINS, plugin::Plugin};

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

    pub fn search_path(&self) -> &Path {
        self.search_path.as_path()
    }

    pub fn watch<P>(path: P) -> Result<Watcher>
    where
        P: AsRef<Path> + 'static,
    {
        let path = path.as_ref().to_path_buf();
        let debouncer = freedom_fs::watch(
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
