use notify_debouncer_full::{
    DebounceEventResult, FileIdMap, new_debouncer, notify::ReadDirectoryChangesWatcher,
};
use std::{cell::RefCell, rc::Rc, time::Duration};

use freedom::{
    log::{handle_error, warn},
    scheme::{
        Result,
        r#async::smol::channel::{Receiver, unbounded},
        steel::{
            SteelVal,
            rvals::{Custom, FromSteelVal, IntoSteelVal},
            steel_vm::builtin::{Arity, BuiltInModule},
            stop, throw,
        },
        steel_future,
    },
};

use crate::{into_steelval::FsIntoSteelVal, recursive_mode::RecursiveMode};

pub type Debouncer = notify_debouncer_full::Debouncer<ReadDirectoryChangesWatcher, FileIdMap>;

#[derive(Clone)]
pub struct Watcher {
    debouncer: Rc<RefCell<Option<Debouncer>>>,
    rx: Receiver<DebounceEventResult>,
}

impl Watcher {
    pub fn register_type(module: &mut BuiltInModule) {
        module
            .register_native_fn(
                "Watcher",
                |args| {
                    Watcher::new(
                        args.get(0)
                            .and_then(|t| match t {
                                SteelVal::NumV(num) => Some(*num),
                                _ => None,
                            })
                            .ok_or_else(throw!(ArityMismatch => "Expected a number"))?,
                        args.get(1).and_then(|t| match t {
                            SteelVal::NumV(num) => Some(*num),
                            _ => None,
                        }),
                    )?
                    .into_steelval()
                },
                Arity::AtLeast(1),
            )
            .register_native_fn(
                "Watcher-watch",
                |args| {
                    Watcher::watch(
                        &args
                            .get(0)
                            .ok_or_else(throw!(ArityMismatch => "Expected 3 arguments"))
                            .and_then(|t| Watcher::from_steelval(t))?,
                        args.get(1)
                            .ok_or_else(throw!(ArityMismatch => "Expected 3 arguments"))
                            .and_then(|t| String::from_steelval(t))?,
                        args.get(2)
                            .ok_or_else(throw!(ArityMismatch => "Expected 3 arguments"))
                            .and_then(|t| RecursiveMode::from_steelval(t))?,
                    )
                },
                Arity::AtLeast(3),
            )
            .register_native_fn(
                "Watcher-unwatch",
                |args| {
                    Watcher::unwatch(
                        &args
                            .get(0)
                            .ok_or_else(throw!(ArityMismatch => "Expected 2 arguments"))
                            .and_then(Watcher::from_steelval)?,
                        args.get(1)
                            .ok_or_else(throw!(ArityMismatch => "Expected 2 arguments"))
                            .and_then(String::from_steelval)?,
                    )
                },
                Arity::Exact(2),
            )
            .register_native_fn(
                "Watcher-events",
                |args| {
                    Ok(steel_future(
                        Watcher::from_steelval(
                            args.get(0)
                                .ok_or_else(throw!(ArityMismatch => "Expected 1 argument"))?,
                        )?
                        .events(),
                    ))
                },
                Arity::Exact(1),
            )
            .register_native_fn(
                "Watcher-stop",
                |args| {
                    Watcher::from_steelval(
                        args.get(0)
                            .ok_or_else(throw!(ArityMismatch => "Expected 1 argument"))?,
                    )?
                    .stop()
                },
                Arity::Exact(1),
            );
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
            debouncer: Rc::new(RefCell::new(Some(debouncer))),
            rx: rx,
        })
    }

    pub fn watch(&self, path: String, recursive_mode: RecursiveMode) -> Result<SteelVal> {
        if let Some(debouncer) = self.debouncer.borrow_mut().as_mut() {
            debouncer
                .watch(path, recursive_mode.unwrap())
                .or_else(|e| stop!(Generic => e))?;
            Ok(SteelVal::BoolV(true))
        } else {
            warn!("Debouncer has been dropped");
            Ok(SteelVal::BoolV(false))
        }
    }

    pub fn unwatch(&self, path: String) -> Result<SteelVal> {
        if let Some(debouncer) = self.debouncer.borrow_mut().as_mut() {
            debouncer.unwatch(path).or_else(|e| stop!(Generic => e))?;
            Ok(SteelVal::BoolV(true))
        } else {
            warn!("Debouncer has been dropped");
            Ok(SteelVal::BoolV(false))
        }
    }

    pub fn stop(self) -> Result<SteelVal> {
        self.debouncer
            .borrow_mut()
            .take()
            .expect("Debouncer already stopped")
            .stop();

        Ok(SteelVal::Void)
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
