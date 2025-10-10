use freedom_async::smol::block_on;
use freedom_log::{handle_error_with, info};

static PLUGIN_DIR: &str = "target/debug/deps";

// Entrypoint
fn main() {
    handle_error_with(|| {
        // Setup logging
        freedom_log::init();

        // Setup plugins
        freedom_plugins::init(&PLUGIN_DIR)?;

        // Setup scripts
        freedom_scripts::init(&"scheme")?;

        // Setup async
        freedom_async::init()?;

        // Run main script
        info!("Running main");
        let main = block_on(freedom_scripts::get_script("main"))?;
        freedom_scheme::with_engine_mut(|engine| engine.run_raw_program(main.unwrap()))?;

        // Run async engine to completion
        info!("Running executor");
        freedom_async::run();

        Ok(())
    });
}
