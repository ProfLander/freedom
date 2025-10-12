pub mod into_steelval;

use log::debug;
pub use notify_debouncer_full;
use smol::channel::Receiver;
use steel::{
    SteelVal, list,
    rvals::{Custom, FromSteelVal, IntoSteelVal},
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
    stop,
};

use std::{cell::RefCell, path::Path, rc::Rc, time::Duration};

use crate::{
    Result,
    r#async::smol::channel::unbounded,
    fs::into_steelval::FsIntoSteelVal,
    log::{handle_error},
    scheme::{steel::steelerr, steel_future},
};
use notify_debouncer_full::{
    DebounceEventResult, DebouncedEvent, FileIdMap, new_debouncer,
    notify::{Error, ReadDirectoryChangesWatcher},
};

pub fn watch<P, F>(
    path: P,
    timeout: Duration,
    tick_rate: Option<Duration>,
    recursive_mode: notify_debouncer_full::notify::RecursiveMode,
    mut f: F,
) -> std::result::Result<Debouncer, Error>
where
    P: AsRef<Path>,
    F: FnMut(DebouncedEvent) + 'static,
{
    let path = path.as_ref();
    debug!("watch: {path:?}");

    let (tx, rx) = unbounded();
    let mut debouncer = new_debouncer(timeout, tick_rate, move |result: DebounceEventResult| {
        handle_error(
            tx.send_blocking(result)
                .or_else(|e| steelerr!(Generic => e)),
        )
    })?;

    crate::r#async::executor().spawn::<_, ()>(async move {
        loop {
            let events = rx.recv().await;
            let events = events.or_else(|e| steelerr!(Generic => e))?;
            let events = events.or_else(|e| steelerr!(Generic => "{:?}", e))?;
            for event in events {
                f(event);
            }
        }
    });

    // Add a path to be watched. All files and directories at that path and
    // below will be monitored for changes.
    debouncer.watch(path, recursive_mode)?;
    Ok(debouncer)
}

pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/fs");
    Watcher::register_type(&mut module);
    module
}

pub type Debouncer = notify_debouncer_full::Debouncer<ReadDirectoryChangesWatcher, FileIdMap>;

#[derive(Clone)]
pub struct Watcher {
    debouncer: Rc<RefCell<Debouncer>>,
    rx: Receiver<DebounceEventResult>,
}

impl Watcher {
    fn register_type(module: &mut BuiltInModule) {
        module
            .register_fn("Watcher", Watcher::new)
            .register_fn("Watcher-watch", Watcher::watch)
            .register_fn("Watcher-unwatch", Watcher::unwatch)
            .register_fn("Watcher-events", |watcher: Watcher| {
                steel_future(watcher.events())
            });
    }
}

#[derive(Clone)]
pub struct RecursiveMode(notify_debouncer_full::notify::RecursiveMode);

impl RecursiveMode {
    pub fn recursive() -> Self {
        RecursiveMode(notify_debouncer_full::notify::RecursiveMode::Recursive)
    }

    pub fn non_recursive() -> Self {
        RecursiveMode(notify_debouncer_full::notify::RecursiveMode::NonRecursive)
    }

    pub fn unwrap(self) -> notify_debouncer_full::notify::RecursiveMode {
        self.0
    }
}

impl IntoSteelVal for RecursiveMode {
    fn into_steelval(self) -> steel::rvals::Result<SteelVal> {
        Ok(list![
            "RecursiveMode",
            match self.0 {
                notify_debouncer_full::notify::RecursiveMode::Recursive => list!["Recursive"],
                notify_debouncer_full::notify::RecursiveMode::NonRecursive => list!["NonRecursive"],
            }
        ])
    }
}

impl FromSteelVal for RecursiveMode {
    fn from_steelval(val: &SteelVal) -> Result<RecursiveMode> {
        let SteelVal::ListV(list) = val else {
            stop!(TypeMismatch => "Expected list, got: {}", val)
        };

        let car = list.car();
        let Some(SteelVal::SymbolV(sym)) = list.car() else {
            stop!(UnexpectedToken => "Expected a symbol, got: {:?}", car)
        };

        if !matches!(sym.as_str(), "RecursiveMode") {
            stop!(UnexpectedToken => "Expected 'RecursiveMode, got: {}", sym)
        }

        let cdr = list.cdr().and_then(|cdr| cdr.car());
        let Some(SteelVal::ListV(list)) = cdr else {
            stop!(UnexpectedToken => "Expected a list, got: {:?}", cdr);
        };

        let Some(SteelVal::SymbolV(sym)) = list.car() else {
            stop!(TypeMismatch => "Expected a symbol, got: {:?}", car)
        };

        let res = match sym.as_str() {
            "Recursive" => RecursiveMode::recursive(),
            "NonRecursive" => RecursiveMode::non_recursive(),
            _ => stop!(UnexpectedToken => "Expected 'Recursive or 'NonRecursive, got: {}", sym),
        };

        Ok(res)
    }
}

impl Watcher {
    pub fn new(timeout: f64, tick_rate: Option<f64>) -> Result<Self> {
        let (tx, rx) = unbounded::<DebounceEventResult>();

        let debouncer = new_debouncer(
            Duration::from_secs_f64(timeout),
            tick_rate.map(Duration::from_secs_f64),
            move |result: DebounceEventResult| {
                handle_error(tx.send_blocking(result).or_else(|e| stop!(Generic => e)))
            },
        )
        .or_else(|e| stop!(Generic => e))?;

        Ok(Watcher {
            debouncer: Rc::new(RefCell::new(debouncer)),
            rx: rx,
        })
    }

    pub fn watch(&self, path: String, recursive_mode: RecursiveMode) -> Result<SteelVal> {
        self.debouncer
            .borrow_mut()
            .watch(path, recursive_mode.unwrap())
            .or_else(|e| stop!(Generic => e))
            .into_steelval()
    }

    pub fn unwatch(&self, path: String) -> Result<SteelVal> {
        self.debouncer
            .borrow_mut()
            .unwatch(path)
            .or_else(|e| stop!(Generic => e))
            .into_steelval()
    }

    pub async fn events(self) -> Result<SteelVal> {
        self.rx
            .recv()
            .await
            .or_else(|e| stop!(Io => e))?
            .or_else(|e| stop!(Generic => "{:?}", e))?
            .into_steelval()
    }
}

impl Custom for Watcher {}
