pub mod r#async;
pub mod fs;
pub mod log;
pub mod plugins;
pub mod scheme;

use std::ffi::OsStr;

use log::handle_error;
use scheme::Result;
use steel::throw;

pub fn run<P: AsRef<OsStr>, Q: AsRef<OsStr>, R: AsRef<OsStr>>(
    plugin_dir: P,
    script_dir: Q,
    entrypoint: R,
) {
    handle_error(run_result(plugin_dir, script_dir, entrypoint));
}

pub fn run_result<P: AsRef<OsStr>, Q: AsRef<OsStr>, R: AsRef<OsStr>>(
    plugin_dir: P,
    script_dir: Q,
    entrypoint: R,
) -> Result<()> {
    // Setup logging
    log::init();

    // Setup plugins
    plugins::init(&plugin_dir)?;

    // Run main script
    scheme::with_engine_mut(|engine| {
        engine.run(format!(
            "(require \"{}/{}\")",
            script_dir
                .as_ref()
                .to_str()
                .ok_or_else(throw!(Generic => "Failed to convert script dir to string slice"))?,
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
