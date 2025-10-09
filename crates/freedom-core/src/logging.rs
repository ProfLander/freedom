pub use log::{Level, debug, error, info, log, trace, warn};
use steel::steelerr;

use crate::handle_error;

pub fn init() {
    // Setup logging
    let logger = env_logger::builder()
        .parse_default_env()
        .format_timestamp(None)
        .build();

    let max_level = logger.filter();
    handle_error(log::set_boxed_logger(Box::new(logger)).or_else(|e| steelerr!(Generic => e)));
    log::set_max_level(max_level);
}
