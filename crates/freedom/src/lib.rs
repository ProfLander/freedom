use std::ffi::OsStr;

use freedom_async::smol::block_on;
use freedom_log::handle_error;
use freedom_scheme::Result;

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
    freedom_log::init();

    // Setup plugins
    freedom_plugins::init(&plugin_dir)?;

    // Setup scripts
    freedom_scripts::init(&script_dir)?;

    // Setup async
    freedom_async::init()?;

    // Run main script
    block_on(freedom_scripts::run(&entrypoint))?;

    // Run async engine to completion
    freedom_async::run();

    // Done
    Ok(())
}
