pub mod r#async;
pub mod engine;
pub mod fs;
pub mod loading;
pub mod log;
pub mod program;

use std::{cell::OnceCell, path::PathBuf};

pub use steel;

use steel::{
    SteelErr, SteelVal,
    gc::Gc,
    rvals::{Custom, FutureResult, IntoSteelVal},
    steel_vm::{builtin::BuiltInModule, engine::Engine as SteelEngine, register_fn::RegisterFn},
    steelerr,
};
use {r#async::Executor, log::info};

use crate::scheme::{engine::Engine, program::Program};

pub type Result<T> = std::result::Result<T, SteelErr>;

thread_local! {
    static ENGINE: OnceCell<Engine> = OnceCell::new();
}

#[derive(Clone)]
pub struct SchemeConfig {
    /// Path to a scheme file which should run to initialize engine instances
    pub kernel: String,
}

impl Custom for SchemeConfig {}

fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/scheme");
    module.register_fn("#%compile", |src: String, path: String| {
        SteelVal::FutureV(Gc::new(FutureResult::new(Box::pin(async move {
            crate::scheme::with_engine_mut(|engine| {
                Program::new(engine.emit_raw_program(src, PathBuf::from(path))?).into_steelval()
            })
        }))))
    });
    module
}

pub fn init(config: SchemeConfig, executor: Executor) -> Result<SteelVal> {
    info!("Initializing scheme on {:?}", std::thread::current().id());

    // Construct engine
    let engine = Engine::new();

    log::init();

    // Perform infallible registration
    engine
        .borrow_mut()
        .register_value("#%scheme-config", config.clone().into_steelval()?)
        .register_value("#%executor", executor.into_steelval()?)
        .register_module(module())
        .register_module(log::module())
        .register_module(r#async::module().unwrap())
        .register_module(loading::module())
        .register_module(fs::module());

    // Emplace the engine
    ENGINE.with(|cell| {
        cell.set(engine)
            .or_else(|_| steelerr!(Generic => "Engine has already been initialized"))
    })?;

    // Run kernel
    ENGINE.with(|cell| {
        cell.get()
            .unwrap()
            .borrow_mut()
            .run(format!("(load \"{}\")", config.kernel))
    })?;

    Ok(SteelVal::Void)
}

pub fn with_engine<T>(f: impl FnOnce(&SteelEngine) -> T) -> T {
    ENGINE.with(|engine| {
        f(&engine
            .get()
            .expect("Engine has not been initialized")
            .borrow())
    })
}

pub fn with_engine_mut<T>(f: impl FnOnce(&mut SteelEngine) -> T) -> T {
    ENGINE.with(|engine| {
        f(&mut engine
            .get()
            .expect("Engine has not been initialized")
            .borrow_mut())
    })
}

pub fn steel_future<F>(fut: F) -> SteelVal
where
    F: Future<Output = Result<SteelVal>> + 'static,
{
    // Return a future to run the winit event loop
    SteelVal::FutureV(Gc::new(FutureResult::new(Box::pin(fut))))
}
