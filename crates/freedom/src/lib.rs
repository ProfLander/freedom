pub mod r#async;
pub mod fs;
pub mod loading;
pub mod log;
pub mod scheme;
pub mod tempfile;

use std::ffi::OsStr;

use log::handle_error;
use scheme::Result;
use steel::throw;

pub fn run<R: AsRef<OsStr>>(entrypoint: R) {
    handle_error(run_result(entrypoint));
}

pub fn run_result<R: AsRef<OsStr>>(entrypoint: R) -> Result<()> {
    // Setup logging
    log::init();

    // Run main script
    scheme::with_engine_mut(|engine| {
        engine.run(format!(
            "(require \"{}\")",
            entrypoint
                .as_ref()
                .to_str()
                .ok_or_else(throw!(Generic => "Failed to convert entrypoint to string slice"))?
        ))
    })?;

    // Run async engine to completion
    r#async::run();

    // Done
    Ok(())
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
