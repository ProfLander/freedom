use freedom::{
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
    println!("Hello, gubbins!");
    Ok(SteelVal::Void)
}

fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("library");
    module.register_native_fn("print-hello", print_hello, Arity::Exact(0));
    module
}

#[unsafe(no_mangle)]
pub fn plugin() -> SteelVal {
    list![
        sym!(Plugin),
        list![
            list![
                sym!(init),
                SteelVal::FuncV(|_| { Ok(SteelVal::Void) })
            ],
            list![
                sym!(module),
                SteelVal::FuncV(|_| { Ok(module().into_steelval()?) })
            ]
        ]
    ]
}
