pub mod app;

use freedom_log::info;
use freedom_scheme::{
    Result,
    steel::{
        SteelVal,
        rvals::FromSteelVal,
        steel_vm::builtin::{Arity, BuiltInModule},
        steelerr,
    },
    steel_future,
};

use winit::event_loop::{ControlFlow, EventLoop};

use crate::app::App;

#[unsafe(no_mangle)]
pub fn init() {
    freedom_log::init_local();
}

#[unsafe(no_mangle)]
pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/winit");
    module.register_native_fn("#%winit", winit, Arity::AtLeast(1));
    module
}

fn winit(args: &[SteelVal]) -> Result<SteelVal> {
    let mut app = App::from_steelval(&SteelVal::ListV(args.into_iter().collect()))?;

    // Return a future to run the winit event loop
    Ok(steel_future(async move {
        let el = EventLoop::new().or_else(|e| steelerr!(Generic => e))?;
        el.set_control_flow(ControlFlow::Poll);
        info!("Entering event loop");
        el.run_app(&mut app).or_else(|e| steelerr!(Generic => e))?;
        info!("Exiting event loop");
        Ok(SteelVal::Void)
    }))
}
