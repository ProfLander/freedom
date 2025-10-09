pub use log::{Level, debug, error, info, log, trace, warn};

pub fn init() {
    // Setup logging
    env_logger::init()
}
