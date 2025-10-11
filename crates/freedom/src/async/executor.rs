use async_executor::StaticLocalExecutor;
pub use smol;

use smol::LocalExecutor;

use crate::{
    log::{handle_error, info},
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
                info!("Spawning future");
                self.spawn(fut.unwrap().into_shared())
            }
            SteelVal::ListV(_) => {
                info!("Spawning form {task}");
                self.spawn(async move {
                    crate::scheme::with_engine_mut(|engine| {
                        info!("Running form {task}");
                        engine.run(format!("({task})"))
                    })
                })
            }
            _ => match Program::from_steelval(&task) {
                Ok(prog) => {
                    info!("Spawning program {task}");
                    self.spawn(async move {
                        crate::scheme::with_engine_mut(|engine| {
                            info!("Running program {task:?}");
                            engine.run_raw_program(prog.unwrap())
                        })
                    })
                }
                Err(_) => {
                    info!("Spawning task {task}");
                    self.spawn(async move {
                        crate::scheme::with_engine_mut(|engine| {
                            info!("Running task {task:?}");
                            engine.call_function_with_args(task, vec![])
                        })
                    })
                }
            },
        }
    }
}
