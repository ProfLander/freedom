use freedom_async::{executor::Executor, smol::block_on};
use freedom_log::handle_error;
use freedom_scheme::{
    Result,
    steel::{SteelVal, primitives::lists::plist_get, rvals::FromSteelVal, steelerr, stop},
};

use winit::{
    application::ApplicationHandler,
    event::{DeviceId, WindowEvent},
    event_loop::ActiveEventLoop,
    window::WindowId,
};

pub struct App {
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
            self.executor.spawn_value(callback);
        }
        Ok(())
    }
}

impl FromSteelVal for App {
    fn from_steelval(val: &SteelVal) -> freedom_scheme::steel::rvals::Result<Self> {
        let SteelVal::ListV(args) = val else {
            stop!(TypeMismatch => "Expected a vector, got: {val}");
        };

        let mut args = args.iter().collect::<Vec<_>>();

        if args.len() < 1 {
            return steelerr!(ArityMismatch => "Expected at least 1 argument");
        }

        // Pop off first argument and cast to executor
        let executor = Executor::from_steelval(args.remove(0))?;

        // Extract keyword arguments
        let args = args.into_iter().collect();

        let default = |_| Ok(SteelVal::Void) as Result<SteelVal>;

        let resumed = plist_get(&args, &SteelVal::SymbolV("#:resumed".into())).or_else(default)?;

        let suspended =
            plist_get(&args, &SteelVal::SymbolV("#:suspended".into())).or_else(default)?;

        let new_events =
            plist_get(&args, &SteelVal::SymbolV("#:new-events".into())).or_else(default)?;

        let device_event =
            plist_get(&args, &SteelVal::SymbolV("#:device-event".into())).or_else(default)?;

        let window_event =
            plist_get(&args, &SteelVal::SymbolV("#:window-event".into())).or_else(default)?;

        let about_to_wait =
            plist_get(&args, &SteelVal::SymbolV("#:about-to-wait".into())).or_else(default)?;

        let exiting = plist_get(&args, &SteelVal::SymbolV("#:exiting".into())).or_else(default)?;

        let memory_warning =
            plist_get(&args, &SteelVal::SymbolV("#:memory-warning".into())).or_else(default)?;

        Ok(App {
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
        block_on(self.executor.tick());
    }

    fn exiting(&mut self, event_loop: &ActiveEventLoop) {
        handle_error(self.callback(self.exiting.clone(), vec![]));
    }

    fn memory_warning(&mut self, event_loop: &ActiveEventLoop) {
        handle_error(self.callback(self.memory_warning.clone(), vec![]));
    }
}
