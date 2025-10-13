use freedom::{
    r#async::smol::channel::Receiver,
    scheme::steel::{
        SteelVal,
        rvals::{Custom, IntoSteelVal},
        steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
        stop,
    },
};

use std::{cell::RefCell, rc::Rc, time::Duration};

use freedom::{
    r#async::smol::channel::unbounded, log::handle_error, scheme::Result, scheme::steel_future,
};
use notify_debouncer_full::{
    DebounceEventResult, FileIdMap, new_debouncer, notify::ReadDirectoryChangesWatcher,
};

use crate::{into_steelval::FsIntoSteelVal, recursive_mode::RecursiveMode};

pub type Debouncer = notify_debouncer_full::Debouncer<ReadDirectoryChangesWatcher, FileIdMap>;

#[derive(Clone)]
pub struct Watcher {
    debouncer: Rc<RefCell<Debouncer>>,
    rx: Receiver<DebounceEventResult>,
}

impl Watcher {
    pub fn register_type(module: &mut BuiltInModule) {
        module
            .register_fn("Watcher", Watcher::new)
            .register_fn("Watcher-watch", Watcher::watch)
            .register_fn("Watcher-unwatch", Watcher::unwatch)
            .register_fn("Watcher-events", |watcher: Watcher| {
                steel_future(watcher.events())
            });
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
