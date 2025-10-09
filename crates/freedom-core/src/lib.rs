pub mod r#async;
pub mod fs;
pub mod logging;
pub mod plugins;
pub mod scripts;

pub use smol;
pub use steel;

use std::{cell::RefCell, future::Future, ops::Deref, rc::Rc};

use log::{error, info};
use steel::{
    SteelErr, SteelVal,
    compiler::program::RawProgramWithSymbols,
    gc::Gc,
    rvals::{Custom, FutureResult, IntoSteelVal},
    steel_vm::{engine::Engine as SteelEngine, register_fn::RegisterFn},
};

pub type Result<T> = std::result::Result<T, SteelErr>;

#[derive(Clone)]
pub struct Engine(Rc<RefCell<SteelEngine>>);

impl Deref for Engine {
    type Target = RefCell<SteelEngine>;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl Custom for Engine {}

#[derive(Clone)]
pub struct Program(RawProgramWithSymbols);

impl Program {
    pub fn unwrap(self) -> RawProgramWithSymbols {
        self.0
    }
}

impl Custom for Program {}

thread_local! {
    static ENGINE: Engine = {
        info!("Constructing new Engine on {:?}", std::thread::current().id());
        let engine = Engine(Rc::new(RefCell::new(SteelEngine::new())));
        engine.borrow_mut()
            .register_fn(
                "#%compile",
                |src: SteelVal| SteelVal::FutureV(Gc::new(FutureResult::new(Box::pin(async move {
                    crate::with_engine_mut(|engine| {
                        Program(engine.emit_raw_program_no_path(src.to_string())?).into_steelval()
                    })
                }))))
        );
        engine
    };
}

pub fn with_engine<T>(f: impl FnOnce(&SteelEngine) -> T) -> T {
    ENGINE.with(|engine| f(&engine.borrow()))
}

pub fn with_engine_mut<T>(f: impl FnOnce(&mut SteelEngine) -> T) -> T {
    ENGINE.with(|engine| f(&mut engine.borrow_mut()))
}

fn handle_error_impl(e: SteelErr) {
    let default = || error!("{e}");

    if let Some(span) = e.span() {
        if let Some(source_id) = span.source_id() {
            with_engine(|engine| {
                if let Some(source_path) = engine.get_path_for_source_id(&source_id) {
                    let Some(source_path) = source_path.to_str() else {
                        error!("Failed to convert path into a string slice: {source_path:?}");
                        return;
                    };

                    if let Some(source) = engine.get_source(&source_id) {
                        error!("{}", e.emit_result_to_string(source_path, &source));
                    } else {
                        default()
                    }
                } else {
                    default()
                }
            })
        } else {
            default()
        }
    } else {
        default()
    }
}

pub fn handle_error<T>(res: Result<T>) {
    if let Err(e) = res {
        handle_error_impl(e);
    }
}

pub fn handle_error_with<F, T>(f: F)
where
    F: FnOnce() -> Result<T>,
{
    if let Err(e) = f() {
        handle_error_impl(e);
    }
}

pub async fn handle_error_async<F, T>(res: F)
where
    F: Future<Output = Result<T>>,
{
    if let Err(e) = res.await {
        handle_error_impl(e);
    }
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
