use std::{
    borrow::Cow,
    cell::{OnceCell, RefCell},
    collections::BTreeMap,
    path::{Path, PathBuf},
    time::Duration,
};

use log::info;
use notify_debouncer_full::{
    Debouncer, FileIdMap,
    notify::{EventKind, ReadDirectoryChangesWatcher, RecursiveMode},
};
use steel::{SteelVal, compiler::program::RawProgramWithSymbols, steelerr, throw};

use crate::{ENGINE, Result, handle_error_with};

thread_local! {
    static SCRIPTS: Scripts = Scripts::new();
}

pub fn init(path: &Path) -> Result<()> {
    SCRIPTS.with(|scripts| scripts.watch(path))
}

pub fn run(name: &str) -> Result<Vec<SteelVal>> {
    SCRIPTS.with(|scripts| scripts.run(name))
}

pub fn source(name: &str) -> Result<Cow<'static, str>> {
    SCRIPTS.with(|scripts| scripts.source(name))
}

pub struct Scripts {
    scripts: RefCell<BTreeMap<String, (Cow<'static, str>, RawProgramWithSymbols)>>,
    watcher: OnceCell<Debouncer<ReadDirectoryChangesWatcher, FileIdMap>>,
}

impl Scripts {
    pub fn new() -> Self {
        Scripts {
            scripts: RefCell::new(BTreeMap::new()),
            watcher: OnceCell::new(),
        }
    }

    pub fn watch(&self, path: &Path) -> Result<()> {
        let debouncer = crate::fs::watch(
            path,
            Duration::from_secs_f32(0.1),
            None,
            RecursiveMode::NonRecursive,
            |event| {
                handle_error_with(|| {
                    match event.event.kind {
                    EventKind::Modify(_) => SCRIPTS.with(|scripts| {
                        for path in &event.paths {
                        let key = path
                            .file_stem()
                            .ok_or_else(throw!(Generic => "Path has no file stem: {:?}", path))?
                            .to_str()
                            .ok_or_else(throw!(Generic => "Failed to convert path to string slice: {:?}", path))?;
                        info!("Reloading {key:}");
                        scripts
                            .scripts
                            .borrow_mut()
                            .insert(key.to_string(), scripts.load_script(&Path::new(key))?);
                        }
                        Ok(())
                    }),
                    EventKind::Remove(_) => {
                        SCRIPTS.with(|scripts| {
                            for path in &event.paths {
                                let key = path
                                    .file_stem()
                                    .ok_or_else(throw!(Generic => "Path has no file stem: {:?}", path))?
                                    .to_str()
                                    .ok_or_else(throw!(Generic => "Failed to convert path to string slice: {:?}", path))?
                                    .to_string();
                                info!("Unloading {key:}");
                                scripts.scripts.borrow_mut().remove(&key);
                            }
                            Ok(())
                        })
                    }
                    _ => Ok(()),
                }
                })
            },
        ).or_else(|e| steelerr!(Generic => e))?;

        self.watcher
            .set(debouncer)
            .or_else(|_| steelerr!(Generic => "Watcher has already been set"))?;

        Ok(())
    }

    pub fn load_script(&self, name: &Path) -> Result<(Cow<'static, str>, RawProgramWithSymbols)> {
        let mut path = PathBuf::new();
        path.push("scheme");
        path.push(name);
        path.set_extension("scm");

        let src: Cow<'static, str> =
            Cow::Owned(std::fs::read_to_string(&path).or_else(|e| steelerr!(Generic => e))?);
        let prog = ENGINE.with(|engine| engine.borrow_mut().emit_raw_program(src.clone(), path))?;
        Ok((src, prog))
    }

    pub fn source<T: AsRef<str>>(&self, name: T) -> Result<Cow<'static, str>> {
        let name = name.as_ref();
        self.scripts
            .borrow()
            .get(name)
            .map(|(src, _)| src.clone())
            .ok_or_else(throw!(Generic => "No script with name: {}", name))
    }

    pub fn run<T: AsRef<str>>(&self, name: T) -> Result<Vec<SteelVal>> {
        let name = name.as_ref();
        let (_, prog) = if let Some(entry) = self.scripts.borrow().get(name) {
            entry.clone()
        } else {
            let script = self.load_script(&Path::new(name))?;

            self.scripts
                .borrow_mut()
                .insert(name.to_string(), script.clone());
            script
        };

        ENGINE.with(|engine| engine.borrow_mut().run_raw_program(prog))
    }
}
