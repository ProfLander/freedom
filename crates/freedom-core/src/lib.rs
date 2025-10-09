pub mod r#async;
pub mod fs;
pub mod logging;
pub mod plugins;
pub mod scripts;

pub use smol;
pub use steel;

use std::cell::RefCell;

use log::info;
use steel::{SteelErr, steel_vm::engine::Engine};

pub type Result<T> = std::result::Result<T, SteelErr>;

thread_local! {
    static ENGINE: RefCell<Engine> = {
        info!("Constructing Engine for {:?}", std::thread::current().id());
        RefCell::new(Engine::new())
    };
}

pub fn with_engine<T>(f: impl FnOnce(&Engine) -> T) -> T {
    ENGINE.with_borrow(|engine| f(engine))
}

pub fn with_engine_mut<T>(f: impl FnOnce(&mut Engine) -> T) -> T {
    ENGINE.with_borrow_mut(|engine| f(engine))
}

pub fn handle_error<T>(res: Result<T>) {
    if let Err(e) = res {
        log::error!("{}", e);
    }
}

pub fn handle_error_with<F, T>(f: F)
where
    F: FnOnce() -> Result<T>,
{
    if let Err(e) = f() {
        log::error!("{}", e);
    }
}

pub async fn handle_error_async<F, T>(res: F)
where
    F: Future<Output = Result<T>>,
{
    if let Err(e) = res.await {
        log::error!("{}", e);
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
