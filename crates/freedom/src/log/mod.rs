pub use log::{Level, debug, error, info, log, trace, warn};
use steel::steelerr;

use crate::scheme::{
    Result,
    steel::{
        SteelErr, SteelVal,
        parser::ast::IteratorExtensions,
        steel_vm::builtin::{Arity, BuiltInModule},
    },
    with_engine,
};

pub fn steelval_to_string(val: &SteelVal) -> String {
    match val {
        SteelVal::StringV(str) => str.to_string(),
        _ => format!("{val:?}"),
    }
}

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

pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/log");
    module.register_native_fn(
        "trace!",
        |args: &[SteelVal]| {
            trace!("{}", args.iter().map(steelval_to_string).join(" "));
            Ok(SteelVal::Void)
        },
        Arity::AtLeast(1),
    );
    module.register_native_fn(
        "info!",
        |args: &[SteelVal]| {
            info!("{}", args.iter().map(steelval_to_string).join(" "));
            Ok(SteelVal::Void)
        },
        Arity::AtLeast(1),
    );
    module.register_native_fn(
        "debug!",
        |args: &[SteelVal]| {
            debug!("{}", args.iter().map(steelval_to_string).join(" "));
            Ok(SteelVal::Void)
        },
        Arity::AtLeast(1),
    );
    module.register_native_fn(
        "warn!",
        |args: &[SteelVal]| {
            warn!("{}", args.iter().map(steelval_to_string).join(" "));
            Ok(SteelVal::Void)
        },
        Arity::AtLeast(1),
    );
    module.register_native_fn(
        "error!",
        |args: &[SteelVal]| {
            error!("{}", args.iter().map(steelval_to_string).join(" "));
            Ok(SteelVal::Void)
        },
        Arity::AtLeast(1),
    );
    module
}

fn handle_error_impl(e: SteelErr) {
    let default = || error!("{e}");

    if let Some(span) = e.span() {
        if let Some(source_id) = span.source_id() {
            with_engine(|engine| {
                if let Some(source_path) = engine.get_path_for_source_id(&source_id) {
                    let Some(source_path) = source_path.to_str() else {
                        error!("Failed to convert path into a string slice: {source_path:?}");
                        return;
                    };

                    if let Some(source) = engine.get_source(&source_id) {
                        error!("{}", e.emit_result_to_string(source_path, &source));
                    } else {
                        default()
                    }
                } else {
                    default()
                }
            })
        } else {
            default()
        }
    } else {
        default()
    }
}

pub fn handle_error<T>(res: Result<T>) {
    if let Err(e) = res {
        handle_error_impl(e);
    }
}

pub fn handle_error_with<F, T>(f: F)
where
    F: FnOnce() -> Result<T>,
{
    if let Err(e) = f() {
        handle_error_impl(e);
    }
}

pub async fn handle_error_async<F, T>(res: F)
where
    F: Future<Output = Result<T>>,
{
    if let Err(e) = res.await {
        handle_error_impl(e);
    }
}
