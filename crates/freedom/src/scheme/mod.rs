pub mod engine;
pub mod program;

use std::path::PathBuf;

pub use steel;

use crate::log::info;
use steel::{
    SteelErr, SteelVal,
    gc::Gc,
    rvals::{FutureResult, IntoSteelVal},
    steel_vm::{builtin::BuiltInModule, engine::Engine as SteelEngine, register_fn::RegisterFn},
};

use crate::scheme::{engine::Engine, program::Program};

pub type Result<T> = std::result::Result<T, SteelErr>;

thread_local! {
    static ENGINE: Engine = {
        info!("Constructing new Engine on {:?}", std::thread::current().id());
        let engine = Engine::new();
        engine.borrow_mut().register_module(module())
            .register_module(crate::log::module())
            .register_module(crate::r#async::module().unwrap())
            .register_module(crate::loading::module())
            .register_module(crate::fs::module())
            .register_module(crate::tempfile::module());
        engine
    };
}

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

pub fn with_engine<T>(f: impl FnOnce(&SteelEngine) -> T) -> T {
    ENGINE.with(|engine| f(&engine.borrow()))
}

pub fn with_engine_mut<T>(f: impl FnOnce(&mut SteelEngine) -> T) -> T {
    ENGINE.with(|engine| f(&mut engine.borrow_mut()))
}

pub fn steel_future<F>(fut: F) -> SteelVal
where
    F: Future<Output = Result<SteelVal>> + 'static,
{
    // Return a future to run the winit event loop
    SteelVal::FutureV(Gc::new(FutureResult::new(Box::pin(fut))))
}
