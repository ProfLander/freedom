use freedom_core::{r#async::Executor, handle_error, smol::block_on, steel::{
    gc::Gc, primitives::lists::plist_get, rvals::{FromSteelVal, FutureResult}, steel_vm::builtin::{Arity, BuiltInModule}, steelerr, SteelVal
}, Result};

use winit::{
    application::ApplicationHandler,
    event::{DeviceId, WindowEvent},
    event_loop::{ActiveEventLoop, ControlFlow, EventLoop},
    window::WindowId,
};

struct App {
    executor: Executor,
    resumed: SteelVal,
    suspended: SteelVal,
    new_events: SteelVal,
    device_event: SteelVal,
    window_event: SteelVal,
    about_to_wait: SteelVal,
    exiting: SteelVal,
    memory_warning: SteelVal,
}

impl App {
    fn callback(&self, callback: SteelVal, args: Vec<SteelVal>) -> Result<()> {
        if !matches!(callback, SteelVal::Void) {
            self.executor.spawn(callback)
        }
        Ok(())
    }
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        handle_error(self.callback(self.resumed.clone(), vec![]));
    }

    fn suspended(&mut self, event_loop: &ActiveEventLoop) {
        handle_error(self.callback(self.suspended.clone(), vec![]));
    }

    fn new_events(&mut self, event_loop: &ActiveEventLoop, cause: winit::event::StartCause) {
        handle_error(self.callback(self.new_events.clone(), vec![]));
    }

    fn device_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        device_id: DeviceId,
        event: winit::event::DeviceEvent,
    ) {
        handle_error(self.callback(self.device_event.clone(), vec![]));
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        window_id: WindowId,
        event: WindowEvent,
    ) {
        handle_error(self.callback(self.window_event.clone(), vec![]));
    }

    fn user_event(&mut self, event_loop: &ActiveEventLoop, event: ()) {
        let _ = (event_loop, event);
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
        handle_error(self.callback(self.about_to_wait.clone(), vec![]));
        block_on(self.executor.borrow().tick());
    }

    fn exiting(&mut self, event_loop: &ActiveEventLoop) {
        handle_error(self.callback(self.exiting.clone(), vec![]));
    }

    fn memory_warning(&mut self, event_loop: &ActiveEventLoop) {
        handle_error(self.callback(self.memory_warning.clone(), vec![]));
    }
}

fn run_winit(args: &[SteelVal]) -> Result<SteelVal> {
    let mut args = args.iter().collect::<Vec<_>>();

    if args.len() < 2 {
        return steelerr!(ArityMismatch => "Expected at least 2 arguments");
    }

    let executor = Executor::from_steelval(args.remove(0))?;
    let args = args.into_iter().collect();

    let default = |_| Ok(SteelVal::Void) as Result<SteelVal>;

    let resumed = plist_get(&args, &SteelVal::SymbolV("#:resumed".into())).or_else(default)?;
    println!("resumed: {resumed:?}");

    let suspended = plist_get(&args, &SteelVal::SymbolV("#:suspended".into())).or_else(default)?;
    println!("suspended: {suspended:?}");

    let new_events =
        plist_get(&args, &SteelVal::SymbolV("#:new-events".into())).or_else(default)?;
    println!("new_events: {new_events:?}");

    let device_event =
        plist_get(&args, &SteelVal::SymbolV("#:device-event".into())).or_else(default)?;
    println!("device_event: {device_event:?}");

    let window_event =
        plist_get(&args, &SteelVal::SymbolV("#:window-event".into())).or_else(default)?;
    println!("window_event: {window_event:?}");

    let about_to_wait =
        plist_get(&args, &SteelVal::SymbolV("#:about-to-wait".into())).or_else(default)?;
    println!("about_to_wait: {about_to_wait:?}");

    let exiting = plist_get(&args, &SteelVal::SymbolV("#:exiting".into())).or_else(default)?;
    println!("exiting: {exiting:?}");

    let memory_warning =
        plist_get(&args, &SteelVal::SymbolV("#:memory-warning".into())).or_else(default)?;
    println!("memory_warning: {memory_warning:?}");

    Ok(SteelVal::FutureV(Gc::new(FutureResult::new(Box::pin(
        async move {
            let el = EventLoop::new().or_else(|e| steelerr!(Generic => e))?;
            el.set_control_flow(ControlFlow::Poll);
            println!("winit entering event loop");
            el.run_app(&mut App {
                executor,
                resumed,
                suspended,
                new_events,
                device_event,
                window_event,
                about_to_wait,
                exiting,
                memory_warning,
            })
            .or_else(|e| steelerr!(Generic => e))?;
            println!("winit exiting event loop");
            Ok(SteelVal::Void)
        },
    )))))
}

#[unsafe(no_mangle)]
pub fn init() {
    freedom_core::logging::init_local();
}

#[unsafe(no_mangle)]
pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/winit");
    module.register_native_fn("#%winit", run_winit, Arity::AtLeast(1));
    module
}
