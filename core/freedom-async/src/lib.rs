pub use smol;

use std::{cell::RefCell, rc::Rc};

use smol::{LocalExecutor, block_on};

use freedom_log::{handle_error, info};
use freedom_scheme::{
    Program, Result,
    steel::{
        SteelVal,
        rvals::{Custom, FromSteelVal, IntoSteelVal},
        steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
        stop,
    },
};

thread_local! {
    pub(crate) static EXECUTOR: Executor = {
        info!("Constructing new Executor on {:?}", std::thread::current().id());
        Executor::new()
    };
}

pub fn executor() -> Executor {
    EXECUTOR.with(Clone::clone)
}

pub fn init() -> Result<()> {
    freedom_scheme::with_engine_mut(|engine| {
        let mut module = BuiltInModule::new("freedom/async");

        module
            .register_value("#%executor", executor().into_steelval()?)
            .register_fn("#%spawn", |task: SteelVal| {
                info!("#%spawn: {task}");
                executor().spawn_value(task);
            })
            .register_fn("#%await", |task: SteelVal, cont: SteelVal| {
                info!("#%await: {task} {cont}");
                executor().spawn(async move {
                    let SteelVal::FutureV(fut) = task else {
                        stop!(TypeMismatch => "Expected a future")
                    };

                    info!("Awaiting future...");
                    let res = fut.unwrap().into_shared().await;
                    let res = res?;

                    info!("Calling continuation with result: {res:?}");
                    freedom_scheme::with_engine_mut(|engine| {
                        engine.call_function_with_args(cont, vec![res])
                    })?;

                    Ok(())
                });
            });

        engine.register_module(module);

        Ok(())
    })
}

pub fn run() {
    let exe = executor();
    let exe = exe.unwrap();
    let exe = exe.borrow();
    while !exe.is_empty() {
        block_on(exe.tick());
    }
}

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
