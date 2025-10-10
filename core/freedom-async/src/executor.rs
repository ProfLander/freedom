pub use smol;

use std::{cell::RefCell, rc::Rc};

use smol::LocalExecutor;

use freedom_log::{handle_error, info};
use freedom_scheme::{
    program::Program, Result,
    steel::{
        SteelVal,
        rvals::{Custom, FromSteelVal},
    },
};

#[derive(Clone)]
pub struct Executor(Rc<RefCell<LocalExecutor<'static>>>);

impl Custom for Executor {}

impl Executor {
    pub fn new() -> Self {
        Executor(Rc::new(RefCell::new(LocalExecutor::new())))
    }

    pub fn unwrap(self) -> Rc<RefCell<LocalExecutor<'static>>> {
        self.0
    }

    pub async fn tick(&self) {
        self.0.borrow().tick().await
    }

    pub fn spawn<F, T>(&self, fut: F)
    where
        F: Future<Output = Result<T>> + 'static,
        T: 'static,
    {
        self.0
            .borrow()
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
                    freedom_scheme::with_engine_mut(|engine| {
                        info!("Running form {task}");
                        engine.run(format!("({task})"))
                    })
                })
            }
            _ => match Program::from_steelval(&task) {
                Ok(prog) => {
                    info!("Spawning program {task}");
                    self.spawn(async move {
                        freedom_scheme::with_engine_mut(|engine| {
                            info!("Running program {task:?}");
                            engine.run_raw_program(prog.unwrap())
                        })
                    })
                }
                Err(_) => {
                    info!("Spawning task {task}");
                    self.spawn(async move {
                        freedom_scheme::with_engine_mut(|engine| {
                            info!("Running task {task:?}");
                            engine.call_function_with_args(task, vec![])
                        })
                    })
                }
            },
        }
    }
}
