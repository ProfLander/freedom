pub mod r#async;
pub mod log;
pub mod plugins;
pub mod scheme;
pub mod scripts;
pub mod fs;

use std::ffi::OsStr;

use r#async::smol::block_on;
use log::handle_error;
use scheme::Result;

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

    // Setup scripts
    scripts::init(&script_dir)?;

    // Run main script
    block_on(scripts::run(&entrypoint))?;

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
