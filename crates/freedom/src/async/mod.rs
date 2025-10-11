pub mod executor;

pub use smol;

use smol::block_on;

use crate::{
    log::info,
    scheme::{
        Result,
        steel::{
            SteelVal,
            rvals::IntoSteelVal,
            steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
            stop,
        },
        steel_future,
    },
};

use crate::r#async::executor::Executor;

thread_local! {
    pub(crate) static EXECUTOR: Executor = {
        info!("Constructing new Executor on {:?}", std::thread::current().id());
        Executor::new()
    };
}

pub fn executor() -> Executor {
    EXECUTOR.with(Clone::clone)
}

pub fn module() -> Result<BuiltInModule> {
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
                crate::scheme::with_engine_mut(|engine| {
                    engine.call_function_with_args(cont, vec![res])
                })?;

                Ok(())
            });
        });

    Ok(module)
}

pub fn run() {
    info!("Running executor");
    let exe = executor();
    let exe = exe.unwrap();
    while exe.try_tick() {
        block_on(exe.tick());
    }
}
