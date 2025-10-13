pub mod executor;

use log::debug;
pub use smol;

use smol::block_on;

use crate::{
    log::info,
    scheme::{
        Result,
        steel::{rvals::IntoSteelVal, steel_vm::builtin::BuiltInModule},
    },
};

use crate::r#async::executor::Executor;

thread_local! {
    pub(crate) static EXECUTOR: Executor = {
        info!("Constructing new Executor on {:?}", std::thread::current().id());
        Executor::new()
    };
}

pub fn executor() -> Executor {
    EXECUTOR.with(Clone::clone)
}

pub fn module() -> Result<BuiltInModule> {
    let mut module = BuiltInModule::new("freedom/async");

    Executor::register_type(&mut module);
    module.register_value("#%executor", executor().into_steelval()?);

    Ok(module)
}

pub fn run() {
    debug!("Running executor");
    let exe = executor();
    let exe = exe.unwrap();
    while exe.try_tick() {
        block_on(exe.tick());
    }
}
