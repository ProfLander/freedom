use freedom::{
    plugins::plugin::PluginInterface,
    scheme::steel::steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
};

#[unsafe(no_mangle)]
pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/stdio");
    module.register_fn("#%write", stdout_write);
    module.register_fn("#%read", stdin_read);
    module
}

pub async fn stdout_write() {
    todo!()
}

pub async fn stdin_read() -> String {
    todo!()
}

#[unsafe(no_mangle)]
pub fn plugin() -> PluginInterface {
    PluginInterface {
        init: || (),
        module,
    }
}
