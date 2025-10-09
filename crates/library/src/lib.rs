use steel::steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn};

fn print_hello() {
    println!("Hello, wow!");
}

#[unsafe(no_mangle)]
pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("library");
    module.register_fn("print-hello", print_hello);
    module
}
