pub mod engine;
pub mod program;

pub use steel;

use log::info;
use steel::{
    gc::Gc, rvals::{FutureResult, IntoSteelVal}, steel_vm::{builtin::BuiltInModule, engine::Engine as SteelEngine, register_fn::RegisterFn}, SteelErr, SteelVal
};

use crate::{engine::Engine, program::Program};

pub type Result<T> = std::result::Result<T, SteelErr>;

thread_local! {
    static ENGINE: Engine = {
        info!("Constructing new Engine on {:?}", std::thread::current().id());
        let engine = Engine::new();
        engine.borrow_mut().register_module(module());
        engine
    };
}

fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/scheme");
    module.register_fn("#%compile", |src: SteelVal| {
        SteelVal::FutureV(Gc::new(FutureResult::new(Box::pin(async move {
            crate::with_engine_mut(|engine| {
                Program::new(engine.emit_raw_program_no_path(src.to_string())?).into_steelval()
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

#[macro_export]
macro_rules! err {
    ($type:ident => $fmt:expr, $($arg:tt)+) => {
        $crate::steel::rerrs::SteelErr::new($crate::steel::rerrs::ErrorKind::$type, format!($fmt, $($arg)+))
    };
    ($type:ident => $thing:expr) => {
        $crate::steel::rerrs::SteelErr::new($crate::steel::rerrs::ErrorKind::$type, ($thing).to_string())
    };
    ($type:ident => $thing:expr; $span:expr) => {
        $crate::steel::rerrs::SteelErr::new($crate::steel::rerrs::ErrorKind::$type, ($thing).to_string()).with_span($span)
    };
    ($type:ident => $thing:expr; $span:expr; $source:expr) => {
        $crate::steel::rerrs::SteelErr::new($crate::steel::rerrs::ErrorKind::$type, ($thing).to_string()).with_span($span).with_source($source)
    };
}
