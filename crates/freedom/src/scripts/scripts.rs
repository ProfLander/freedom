use std::{
    borrow::Cow,
    cell::{OnceCell, RefCell},
    collections::BTreeMap,
    ffi::OsStr,
    path::{Path, PathBuf},
    time::Duration,
};

use crate::{
    fs::notify_debouncer_full::{
        Debouncer, FileIdMap,
        notify::{EventKind, ReadDirectoryChangesWatcher, RecursiveMode},
    },
    log::{handle_error_with, info},
    scheme::{
        Result,
        steel::{compiler::program::RawProgramWithSymbols, steelerr, throw},
    },
    scripts::SCRIPTS,
};

pub struct Scripts {
    scripts: RefCell<BTreeMap<Cow<'static, OsStr>, (Cow<'static, str>, RawProgramWithSymbols)>>,
    watcher: OnceCell<(PathBuf, Debouncer<ReadDirectoryChangesWatcher, FileIdMap>)>,
}

impl Scripts {
    pub fn new() -> Self {
        Scripts {
            scripts: RefCell::new(BTreeMap::new()),
            watcher: OnceCell::new(),
        }
    }

    pub fn get<P: AsRef<OsStr>>(&self, name: &P) -> Option<RawProgramWithSymbols> {
        self.scripts
            .borrow()
            .get(name.as_ref())
            .map(|(_, prog)| prog.clone())
    }

    pub fn insert<P: Into<Cow<'static, OsStr>>>(
        &self,
        name: P,
        script: (Cow<'static, str>, RawProgramWithSymbols),
    ) {
        self.scripts.borrow_mut().insert(name.into(), script);
    }

    pub fn watch<P: AsRef<OsStr>>(&self, path: &P) -> Result<()> {
        let path = path.as_ref();
        let debouncer = crate::fs::watch(
            path,
            Duration::from_secs_f32(0.1),
            None,
            RecursiveMode::NonRecursive,
            |event| {
                handle_error_with(|| match event.event.kind {
                    EventKind::Modify(_) => SCRIPTS.with(|scripts| {
                        for path in &event.paths {
                            let key = path.file_stem().ok_or_else(
                                throw!(Generic => "Path has no file stem: {:?}", path),
                            )?;
                            info!("Reloading {key:?}");
                            scripts
                                .scripts
                                .borrow_mut()
                                .insert(key.to_owned().into(), scripts.load_script(&key)?);
                        }
                        Ok(())
                    }),
                    EventKind::Remove(_) => SCRIPTS.with(|scripts| {
                        for path in &event.paths {
                            let key = path.file_stem().ok_or_else(
                                throw!(Generic => "Path has no file stem: {:?}", path),
                            )?;
                            info!("Unloading {key:?}");
                            scripts.scripts.borrow_mut().remove(key);
                        }
                        Ok(())
                    }),
                    _ => Ok(()),
                })
            },
        )
        .or_else(|e| steelerr!(Generic => e))?;

        self.watcher
            .set((PathBuf::from(path), debouncer))
            .or_else(|_| steelerr!(Generic => "Watcher has already been set"))?;

        Ok(())
    }

    fn path(&self) -> &Path {
        self.watcher.get().map(|(path, _)| path.as_path()).unwrap()
    }

    pub fn load_script<P: AsRef<OsStr>>(
        &self,
        name: &P,
    ) -> Result<(Cow<'static, str>, RawProgramWithSymbols)> {
        let mut path = PathBuf::new();
        path.push(self.path());
        path.push(Path::new(name));
        path.set_extension("scm");

        let src: Cow<'static, str> =
            Cow::Owned(std::fs::read_to_string(&path).or_else(|e| steelerr!(Generic => e))?);
        let prog =
            crate::scheme::with_engine_mut(|engine| engine.emit_raw_program(src.clone(), path))?;
        Ok((src, prog))
    }

    pub fn source<T: AsRef<OsStr>>(&self, name: T) -> Result<Cow<'static, str>> {
        let name = name.as_ref();
        self.scripts
            .borrow()
            .get(name)
            .map(|(src, _)| src.clone())
            .ok_or_else(throw!(Generic => "No script with name: {:?}", name))
    }
}
