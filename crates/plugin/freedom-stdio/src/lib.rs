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

#[unsafe(no_mangle)]
pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/stdio");
    module.register_native_fn("#%write", stdout_write, Arity::AtLeast(1));
    module.register_native_fn("#%read", stdin_read, Arity::Exact(0));
    module
}

pub fn stdout_write(_args: &[SteelVal]) -> Result<SteelVal> {
    todo!()
}

pub fn stdin_read(_args: &[SteelVal]) -> Result<SteelVal> {
    todo!()
}

#[unsafe(no_mangle)]
pub fn plugin() -> SteelVal {
    list![
        sym!(Plugin),
        list![
            list![sym!(init), SteelVal::FuncV(|_| Ok(SteelVal::Void))],
            list![
                sym!(module),
                SteelVal::FuncV(|_| { Ok(module().into_steelval()?) })
            ]
        ]
    ]
}
