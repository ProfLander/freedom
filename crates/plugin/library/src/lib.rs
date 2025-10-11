use freedom::log::info;
use freedom::scheme::steel::steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn};

fn print_hello() {
    info!("Hello, gubbin!");
}

#[unsafe(no_mangle)]
pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("library");
    module.register_fn("print-hello", print_hello);
    module
}

#[unsafe(no_mangle)]
pub fn init() {
    freedom::log::init_local();
}
