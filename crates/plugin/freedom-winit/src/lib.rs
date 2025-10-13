pub mod app;
pub mod callback;
pub mod event_loop;
pub mod from_steelval;
pub mod into_steelval;
pub mod window;

use freedom::{
    r#async::executor::Executor,
    scheme::steel::{
        SteelVal, list,
        rvals::{FromSteelVal, IntoSteelVal},
        steel_vm::builtin::{Arity, BuiltInModule},
    },
    sym,
};

use crate::{app::App, event_loop::EventLoop, window::Window};

pub type UserEvent = SteelVal;

fn init() {
    freedom::log::init_local();
    freedom::scheme::with_engine_mut(|engine| {
        EventLoop::register_type(engine);
        Window::register_type(engine);
    })
}

fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/winit");
    module.register_native_fn(
        "#%winit-run",
        |args| App::run(Executor::from_steelval(&args[0])?, args[1].clone()),
        Arity::Exact(2),
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
            list![sym!(init), SteelVal::FuncV(|_| Ok(init().into_steelval()?))],
            list![
                sym!(module),
                SteelVal::FuncV(|_| { Ok(module().into_steelval()?) })
            ]
        ]
    ]
}
