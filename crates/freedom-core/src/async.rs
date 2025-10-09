use std::{cell::RefCell, ops::Deref, rc::Rc};

use log::{error, info};
use smol::{LocalExecutor, block_on, channel::bounded};
use steel::{
    SteelVal,
    rvals::{Custom, IntoSteelVal},
    steel_vm::{
        builtin::{Arity, BuiltInModule},
        register_fn::RegisterFn,
    },
    stop,
};

use crate::{handle_error, handle_error_with};

thread_local! {
    pub(crate) static EXECUTOR: Executor = {
        info!("Constructing Executor for {:?}", std::thread::current().id());
        Executor::new()
    };
}

pub fn init() -> crate::Result<()> {
    crate::with_engine_mut(|engine| {
        let mut module = BuiltInModule::new("freedom/async");

        module
            .register_value("#%executor", executor().into_steelval()?)
            .register_native_fn(
                "#%spawn",
                |args: &[SteelVal]| {
                    let mut args = args.to_vec();
                    let task = args.remove(0);
                    match task {
                        SteelVal::FutureV(fut) => {
                            info!("Spawning future");
                            spawn(async move {
                                info!("Running future");
                                handle_error(fut.unwrap().into_shared().await);
                            });
                        }
                        SteelVal::SymbolV(sym) | SteelVal::StringV(sym) => {
                            info!("Spawning script: {sym}");
                            spawn(async move {
                                let name = sym.as_str();
                                info!("Running script: {name}");
                                handle_error_with(|| {
                                    let res = crate::scripts::run(name);
                                    if let Err(e) = &res {
                                        let src = crate::scripts::source(name)?;
                                        error!("{}", e.emit_result_to_string(sym.as_str(), &src));
                                    }
                                    Ok(())
                                });
                            });
                        }
                        _ => {
                            info!("Spawning task {task:?} with args {args:?}");
                            spawn(async move {
                                handle_error(crate::with_engine_mut(|engine| {
                                    info!("Running task {task:?} with args {args:?}");
                                    engine.call_function_with_args(task, args)
                                }));
                            });
                        }
                    };

                    Ok(SteelVal::Void)
                },
                Arity::AtLeast(1),
            );

        engine.register_module(module);

        Ok(())
    })
}

#[derive(Clone)]
pub struct Executor(Rc<RefCell<LocalExecutor<'static>>>);

impl Deref for Executor {
    type Target = Rc<RefCell<LocalExecutor<'static>>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Custom for Executor {}

impl Executor {
    pub fn new() -> Self {
        Executor(Rc::new(RefCell::new(LocalExecutor::new())))
    }
}

pub fn executor() -> Executor {
    EXECUTOR.with(Clone::clone)
}

pub fn spawn(fut: impl Future<Output = ()> + 'static) {
    executor().borrow().spawn(fut).detach();
}

pub fn run() -> crate::Result<()> {
    let exe = executor();
    let exe = exe.borrow();
    let (_tx, rx) = bounded::<()>(1);
    Ok(block_on(exe.run(rx.recv())).or_else(|e| stop!(Generic => e))?)
}
