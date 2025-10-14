pub mod app;
pub mod callback;
pub mod event_loop;
pub mod from_steelval;
pub mod into_steelval;
pub mod window;

use freedom::{
    scheme::{
        SchemeConfig,
        r#async::Executor,
        steel::{
            SteelVal, list,
            rvals::{FromSteelVal, IntoSteelVal},
            steel_vm::builtin::{Arity, BuiltInModule},
        },
    },
    sym,
};

use crate::app::App;

pub type UserEvent = SteelVal;

fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/winit");
    module.register_native_fn(
        "#%winit-run",
        |args| {
            freedom::log::init();
            App::run(
                SchemeConfig::from_steelval(&args[0])?,
                Executor::from_steelval(&args[1])?,
                args[2].clone(),
            )
        },
        Arity::Exact(3),
    );
    module.register_native_fn(
        "#%winit-send",
        |args| App::send(args[0].clone()),
        Arity::Exact(1),
    );
    module
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
