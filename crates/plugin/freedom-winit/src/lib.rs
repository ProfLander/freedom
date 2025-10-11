pub mod app;
pub mod callback;
pub mod event_loop;
pub mod from_steelval;
pub mod into_steelval;
pub mod window;

use freedom::scheme::steel::{
    SteelVal,
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
};

use crate::{app::App, event_loop::EventLoop, window::Window};

pub type UserEvent = SteelVal;

#[unsafe(no_mangle)]
pub fn init() {
    freedom::log::init_local();
    freedom::scheme::with_engine_mut(|engine| {
        EventLoop::register_type(engine);
        Window::register_type(engine);
    })
}

#[unsafe(no_mangle)]
pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/winit");
    module.register_fn("#%winit-run", App::run);
    module.register_fn("#%winit-send", App::send);
    module
}
