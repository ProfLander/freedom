pub mod app;
pub mod callback;
pub mod event_loop;
pub mod from_steelval;
pub mod into_steelval;
pub mod window;

use std::cell::OnceCell;

use freedom_log::info;
use freedom_scheme::{
    Result,
    steel::{
        SteelVal,
        rvals::FromSteelVal,
        steel_vm::{
            builtin::{Arity, BuiltInModule},
            register_fn::RegisterFn,
        },
        steelerr, throw,
    },
    steel_future,
};

use winit::event_loop::{ControlFlow, EventLoop, EventLoopProxy};

use crate::app::App;

thread_local! {
    static PROXY: OnceCell<EventLoopProxy<UserEvent>> = OnceCell::new();
}

pub type UserEvent = SteelVal;

#[unsafe(no_mangle)]
pub fn init() {
    freedom_log::init_local();
}

#[unsafe(no_mangle)]
pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/winit");
    module.register_native_fn("#%winit", winit, Arity::AtLeast(1));
    module.register_fn("#%winit-send", winit_send);
    module
}

fn winit_send(event: UserEvent) -> Result<SteelVal> {
    PROXY.with(|proxy| {
        proxy
            .get()
            .ok_or_else(throw!(Generic => "EventLoopProxy accessed before initialization"))?
            .send_event(event)
            .or_else(|e| steelerr!(Generic => e))
    })?;
    Ok(SteelVal::Void)
}

fn winit(args: &[SteelVal]) -> Result<SteelVal> {
    let mut app = App::from_steelval(&SteelVal::ListV(args.into_iter().collect()))?;

    // Return a future to run the winit event loop
    Ok(steel_future(async move {
        let el = EventLoop::<UserEvent>::with_user_event()
            .build()
            .or_else(|e| steelerr!(Generic => e))?;
        el.set_control_flow(ControlFlow::Poll);

        PROXY
            .with(|proxy| proxy.set(el.create_proxy()))
            .or_else(|_| steelerr!(Infallible => "Event loop proxy already set"))?;

        info!("Entering event loop");
        el.run_app(&mut app).or_else(|e| steelerr!(Generic => e))?;
        info!("Exiting event loop");

        Ok(SteelVal::Void)
    }))
}
