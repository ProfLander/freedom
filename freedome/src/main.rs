use std::path::Path;

use freedom_core::{handle_error, logging::info};

static PLUGIN_DIR: &str = "target/debug/deps";

// Entrypoint
fn main() {
    handle_error(main_impl());
}

fn main_impl() -> freedom_core::Result<()> {
    // Setup logging
    freedom_core::logging::init();

    // Setup plugins
    freedom_core::plugins::init(PLUGIN_DIR, vec!["library".into(), "freedom_winit".into()])?;

    // Setup scripts
    freedom_core::scripts::init(&Path::new("scheme"))?;

    // Setup scripts
    freedom_core::r#async::init()?;

    // Run main script
    info!("Running main");
    if let Err(_) = freedom_core::scripts::run(&"main") {
        return Ok(());
    }

    // Run async engine to completion
    info!("Running executor");
    freedom_core::r#async::run()?;

    Ok(())
}
