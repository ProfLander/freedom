use async_executor::StaticLocalExecutor;
use log::debug;
pub use smol;

use smol::LocalExecutor;
use steel::{steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn}, stop};

use crate::{
    log::handle_error,
    scheme::{
        Result,
        program::Program,
        steel::{
            SteelVal,
            rvals::{Custom, FromSteelVal},
        },
    },
};

#[derive(Clone)]
pub struct Executor(&'static StaticLocalExecutor);

impl Custom for Executor {}

impl Executor {
    pub fn register_type(module: &mut BuiltInModule) {
        module
            .register_fn("#%spawn", Self::spawn_value)
            .register_fn("#%await", Self::await_future);
    }
    pub fn new() -> Self {
        Executor(LocalExecutor::new().leak())
    }

    pub fn unwrap(&self) -> &'static StaticLocalExecutor {
        self.0
    }

    pub async fn tick(&self) {
        self.0.tick().await
    }

    pub fn spawn<F, T>(&self, fut: F)
    where
        F: Future<Output = Result<T>> + 'static,
        T: 'static,
    {
        self.0
            .spawn(async move { handle_error(fut.await) })
            .detach()
    }

    pub fn spawn_value(&self, task: SteelVal) {
        match task {
            SteelVal::FutureV(fut) => {
                debug!("Spawning future");
                self.spawn(fut.unwrap().into_shared())
            }
            SteelVal::ListV(_) => {
                debug!("Spawning form {task}");
                self.spawn(async move {
                    crate::scheme::with_engine_mut(|engine| {
                        debug!("Running form {task}");
                        engine.run(format!("({task})"))
                    })
                })
            }
            _ => match Program::from_steelval(&task) {
                Ok(prog) => {
                    debug!("Spawning program {task}");
                    self.spawn(async move {
                        crate::scheme::with_engine_mut(|engine| {
                            debug!("Running program {task:?}");
                            engine.run_raw_program(prog.unwrap())
                        })
                    })
                }
                Err(_) => {
                    debug!("Spawning task {task}");
                    self.spawn(async move {
                        crate::scheme::with_engine_mut(|engine| {
                            debug!("Running task {task:?}");
                            engine.call_function_with_args(task, vec![])
                        })
                    })
                }
            },
        }
    }

    pub fn await_future(&self, task: SteelVal, cont: SteelVal) {
        debug!("#%await: {task} {cont}");
        self.spawn(async move {
            let SteelVal::FutureV(fut) = task else {
                stop!(TypeMismatch => "Expected a future")
            };

            debug!("Awaiting future...");
            let res = fut.unwrap().into_shared().await;
            let res = res?;

            debug!("Calling continuation with result: {res:?}");
            crate::scheme::with_engine_mut(|engine| {
                engine.call_function_with_args(cont, vec![res])
            })?;

            Ok(())
        });
    }
}
