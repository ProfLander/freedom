pub mod scheme;

use std::ffi::OsStr;
use steel::throw;

use scheme::{Result, SchemeConfig, r#async::Executor, log::handle_error_with};

pub fn run<R: AsRef<OsStr>>(config: SchemeConfig, entrypoint: R) {
    handle_error_with(|| {
        let executor = Executor::new();

        scheme::init(config, executor)?;

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
