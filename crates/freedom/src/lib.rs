pub mod log;
pub mod scheme;

use easy_parallel::Parallel;
use std::ffi::OsStr;
use steel::throw;

use log::handle_error_with;
use scheme::{Result, SchemeConfig, r#async::Executor};

pub fn run<P: AsRef<OsStr>, Q: AsRef<OsStr>>(config: SchemeConfig, main: P, worker: Q) {
    log::init();

    let thread_count = 1;

    let main = main.as_ref();
    let worker = worker.as_ref();
    let mut par = Parallel::new();
    for i in 1..thread_count {
        par = par.add({
            let config = config.clone();
            move || worker_thread(i, config, worker)
        });
    }
    let _ = par.finish(|| worker_thread(0, config, main));
}

pub fn worker_thread<P: AsRef<OsStr>>(worker_id: usize, config: SchemeConfig, entrypoint: P) {
    handle_error_with(|| {
        let executor = Executor::new();

        scheme::init(worker_id, config, executor)?;

        // Run main script
        scheme::with_engine_mut(|engine| {
            engine.run(format!(
                "(require \"{}\")",
                entrypoint.as_ref().to_str().ok_or_else(
                    throw!(Generic => "Failed to convert entrypoint to string slice")
                )?
            ))
        })?;

        // Run async engine to completion
        executor.run();
        
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
