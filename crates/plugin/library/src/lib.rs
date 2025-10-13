use freedom::{
    log::info,
    scheme::{
        Result,
        steel::{
            SteelVal, list,
            rvals::IntoSteelVal,
            steel_vm::builtin::{Arity, BuiltInModule},
        },
    },
    sym,
};

fn print_hello(_: &[SteelVal]) -> Result<SteelVal> {
    info!("Hello, gubbins!");
    Ok(SteelVal::Void)
}

fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("library");
    module.register_native_fn("print-hello", print_hello, Arity::Exact(0));
    module
}

fn init() {
    freedom::log::init_local();
    info!("Initialized library");
}

#[unsafe(no_mangle)]
pub fn plugin() -> SteelVal {
    list![
        sym!(Plugin),
        list![
            list![
                sym!(init),
                SteelVal::FuncV(|_| { Ok(init().into_steelval()?) })
            ],
            list![
                sym!(module),
                SteelVal::FuncV(|_| { Ok(module().into_steelval()?) })
            ]
        ]
    ]
}
