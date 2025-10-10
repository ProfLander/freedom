use std::{
    borrow::Cow,
    cell::{OnceCell, RefCell},
    collections::BTreeMap,
    path::{Path, PathBuf},
    time::Duration,
};

use freedom_scheme::{
    Program, Result,
    steel::{
        compiler::program::RawProgramWithSymbols, steel_vm::register_fn::RegisterFn, steelerr,
        throw,
    },
};
use freedom_fs::notify_debouncer_full::{
    Debouncer, FileIdMap,
    notify::{EventKind, ReadDirectoryChangesWatcher, RecursiveMode},
};
use freedom_log::{handle_error_with, info};

thread_local! {
    static SCRIPTS: Scripts = Scripts::new();
}

pub fn init(path: &Path) -> Result<()> {
    SCRIPTS.with(|scripts| scripts.watch(path))?;
    freedom_scheme::with_engine_mut(|engine| {
        engine.register_fn("#%get-script", get_script::<String>);
    });
    Ok(())
}

pub async fn get_script<P: AsRef<str>>(name: P) -> Result<Program> {
    let name = name.as_ref();
    let prog = SCRIPTS.with(|scripts| {
        let (_, prog) = if let Some(entry) = scripts.scripts.borrow().get(name) {
            entry.clone()
        } else {
            let script = scripts.load_script(&Path::new(name))?;

            scripts
                .scripts
                .borrow_mut()
                .insert(name.to_string(), script.clone());
            script
        };
        Ok(Program::new(prog)) as Result<_>
    })?;

    Ok(prog)
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
        let debouncer = freedom_fs::watch(
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
        let prog =
            freedom_scheme::with_engine_mut(|engine| engine.emit_raw_program(src.clone(), path))?;
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
}
