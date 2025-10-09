use freedom_core::{
    r#async::Executor,
    plugins::plugin::{FunctionSpec, PluginSpec},
    smol::block_on,
};
use steel::{
    SteelErr, SteelVal,
    gc::Gc,
    rvals::{FromSteelVal, FutureResult},
    steel_vm::{
        builtin::{Arity, BuiltInModule},
        register_fn::RegisterFn,
    },
    stop, throw,
};
use winit::{
    application::ApplicationHandler,
    event::{DeviceId, WindowEvent},
    event_loop::{ActiveEventLoop, ControlFlow, EventLoop},
    window::WindowId,
};

struct App(Executor);

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        println!("resumed");
    }

    fn device_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        device_id: DeviceId,
        event: winit::event::DeviceEvent,
    ) {
        println!("device event")
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        window_id: WindowId,
        event: WindowEvent,
    ) {
        println!("window event")
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
        println!("winit about to wait");
        block_on(self.0.borrow().tick());
        println!("winit done waiting");
    }
}

#[unsafe(no_mangle)]
pub fn run_winit(args: &[SteelVal]) -> Result<SteelVal, SteelErr> {
    let exe = args
        .get(0)
        .ok_or_else(throw!(ArityMismatch => "Expected 1 argument"))?;

    let exe: Executor = Executor::from_steelval(exe)?;

    Ok(SteelVal::FutureV(Gc::new(FutureResult::new(Box::pin(
        async move {
            let el = EventLoop::new().or_else(|e| stop!(Generic => e))?;
            el.set_control_flow(ControlFlow::Poll);
            println!("winit entering event loop");
            el.run_app(&mut App(exe)).or_else(|e| stop!(Generic => e))?;
            println!("winit exiting event loop");
            Ok(SteelVal::Void)
        },
    )))))
}

#[unsafe(no_mangle)]
pub fn spec() -> PluginSpec {
    PluginSpec {
        name: "freedom/winit",
        values: &[],
        functions: &[FunctionSpec {
            symbol: "run_winit",
            name: "run-winit",
            arity: Arity::Exact(1),
        }],
    }
}
