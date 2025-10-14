pub mod log;
pub mod scheme;

use std::ffi::OsStr;

use log::handle_error_with;
use scheme::{Result, SchemeConfig, r#async::Executor};
use steel::steel_vm::engine::Engine;

pub fn run<P: AsRef<OsStr>>(config: SchemeConfig, main: P) {
    log::init();

    handle_error_with(|| {
        worker_thread_run(config, main);
        Ok(())
    })
}

pub fn worker_thread_run<P: AsRef<OsStr>>(config: SchemeConfig, entrypoint: P) {
    let entrypoint = entrypoint.as_ref().to_string_lossy().to_string();
    worker_thread_run_with(config, |engine| engine.run(entrypoint));
}

pub fn worker_thread_run_with<F, T>(config: SchemeConfig, f: F)
where
    F: FnOnce(&mut Engine) -> Result<T>,
{
    let executor = Executor::new();

    handle_error_with(|| {
        scheme::init(config, executor)?;

        // Run main script
        scheme::with_engine_mut(f)?;

        // Run async engine to completion
        executor.run();

        // Invoke exit callback
        scheme::with_engine_mut(|engine| {
            engine.run(format!("(when (function? *on-exit*) (*on-exit*))"))
        })?;

        // Done
        Ok(())
    })
}

/// Create a Steel symbol from a Rust identifier or string literal
#[macro_export]
macro_rules! sym {
    ($ident:ident) => {
        SteelVal::SymbolV(stringify!($ident).into())
    };
    ($str:expr) => {
        SteelVal::SymbolV($str.into())
    };
}
