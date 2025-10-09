pub use log::{Level, debug, error, info, log, trace, warn};
use steel::{
    SteelVal,
    parser::ast::IteratorExtensions,
    steel_vm::builtin::{Arity, BuiltInModule},
    steelerr,
};

use crate::handle_error;

pub fn init() {
    init_local();
    crate::with_engine_mut(|engine| {
        let mut module = BuiltInModule::new("freedom/log");
        module.register_native_fn(
            "trace!",
            |args: &[SteelVal]| {
                trace!("{}", args.iter().map(ToString::to_string).join(" "));
                Ok(SteelVal::Void)
            },
            Arity::AtLeast(1),
        );
        module.register_native_fn(
            "info!",
            |args: &[SteelVal]| {
                info!("{}", args.iter().map(ToString::to_string).join(" "));
                Ok(SteelVal::Void)
            },
            Arity::AtLeast(1),
        );
        module.register_native_fn(
            "debug!",
            |args: &[SteelVal]| {
                debug!("{}", args.iter().map(ToString::to_string).join(" "));
                Ok(SteelVal::Void)
            },
            Arity::AtLeast(1),
        );
        module.register_native_fn(
            "warn!",
            |args: &[SteelVal]| {
                warn!("{}", args.iter().map(ToString::to_string).join(" "));
                Ok(SteelVal::Void)
            },
            Arity::AtLeast(1),
        );
        module.register_native_fn(
            "error!",
            |args: &[SteelVal]| {
                error!("{}", args.iter().map(ToString::to_string).join(" "));
                Ok(SteelVal::Void)
            },
            Arity::AtLeast(1),
        );
        engine.register_module(module);
    });
}

pub fn init_local() {
    // Setup logging
    let logger = env_logger::builder()
        .parse_default_env()
        .format_timestamp(None)
        .build();

    let max_level = logger.filter();
    handle_error(log::set_boxed_logger(Box::new(logger)).or_else(|e| steelerr!(Generic => e)));
    log::set_max_level(max_level);
}
