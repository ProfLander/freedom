use freedom::log::info;
use freedom::plugins::plugin::PluginInterface;
use freedom::scheme::steel::steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn};

fn print_hello() {
    info!("Hello, gubbin!");
}

fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("library");
    module.register_fn("print-hello", print_hello);
    module
}

fn init() {
    freedom::log::init_local();
}

#[unsafe(no_mangle)]
pub fn plugin() -> PluginInterface {
    PluginInterface { init, module }
}
