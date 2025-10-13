pub mod r#async;
pub mod fs;
pub mod loading;
pub mod log;
pub mod scheme;

use std::ffi::OsStr;

use scheme::Result;
use steel::throw;

use crate::{log::handle_error_with, scheme::SchemeConfig};

pub fn run<R: AsRef<OsStr>>(config: SchemeConfig, entrypoint: R) {
    handle_error_with(|| {
        scheme::init(config)?;
        
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
        r#async::run();

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
