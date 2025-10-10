use freedom_async::{executor::Executor, smol::block_on};
use freedom_log::{handle_error, handle_error_with};
use freedom_scheme::{
    Result,
    steel::{
        SteelVal,
        primitives::lists::plist_get,
        rvals::{FromSteelVal, IntoSteelVal},
        steelerr, stop,
    },
};

use winit::{
    application::ApplicationHandler,
    event::{DeviceId, WindowEvent},
    event_loop::ActiveEventLoop,
    window::WindowId,
};

use crate::{
    UserEvent, callback::Callback, event_loop::EventLoop, into_steelval::WinitIntoSteelVal,
};

pub struct App {
    executor: Executor,
    resumed: Callback,
    suspended: Callback,
    new_events: Callback,
    device_event: Callback,
    window_event: Callback,
    about_to_wait: Callback,
    exiting: Callback,
    memory_warning: Callback,
}

impl App {
    fn callback(
        &self,
        event_loop: &ActiveEventLoop,
        callback: Callback,
        mut args: Vec<SteelVal>,
    ) -> Result<()> {
        let callback = callback.into_steelval()?;
        if !matches!(callback, SteelVal::Void) {
            let el = EventLoop::new();
            args.insert(0, el.clone().into_steelval()?);
            freedom_scheme::with_engine_mut(|engine| {
                engine.call_function_with_args(callback, args)
            })?;
            el.apply(event_loop);
        }
        Ok(())
    }
}

impl FromSteelVal for App {
    fn from_steelval(val: &SteelVal) -> freedom_scheme::steel::rvals::Result<Self> {
        let SteelVal::ListV(args) = val else {
            stop!(TypeMismatch => "Expected a vector, got: {}", val);
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
            resumed: Callback::from_steelval(&resumed)?,
            suspended: Callback::from_steelval(&suspended)?,
            new_events: Callback::from_steelval(&new_events)?,
            device_event: Callback::from_steelval(&device_event)?,
            window_event: Callback::from_steelval(&window_event)?,
            about_to_wait: Callback::from_steelval(&about_to_wait)?,
            exiting: Callback::from_steelval(&exiting)?,
            memory_warning: Callback::from_steelval(&memory_warning)?,
        })
    }
}

impl ApplicationHandler<UserEvent> for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        handle_error(self.callback(event_loop, self.resumed.clone(), vec![]));
    }

    fn suspended(&mut self, event_loop: &ActiveEventLoop) {
        handle_error(self.callback(event_loop, self.suspended.clone(), vec![]));
    }

    fn new_events(&mut self, event_loop: &ActiveEventLoop, cause: winit::event::StartCause) {
        handle_error_with(|| {
            self.callback(
                event_loop,
                self.new_events.clone(),
                vec![cause.into_steelval()?],
            )
        });
    }

    fn device_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        device_id: DeviceId,
        event: winit::event::DeviceEvent,
    ) {
        handle_error_with(|| {
            self.callback(
                event_loop,
                self.device_event.clone(),
                vec![device_id.into_steelval()?, event.into_steelval()?],
            )
        });
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        window_id: WindowId,
        event: WindowEvent,
    ) {
        handle_error_with(|| {
            self.callback(
                event_loop,
                self.window_event.clone(),
                vec![window_id.into_steelval()?, event.into_steelval()?],
            )
        });
    }

    fn user_event(&mut self, event_loop: &ActiveEventLoop, event: UserEvent) {
        handle_error_with(|| {
            let callback = Callback::from_steelval(&event)?;
            self.callback(event_loop, callback, vec![])
        })
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
        handle_error(self.callback(event_loop, self.about_to_wait.clone(), vec![]));
        block_on(self.executor.tick());
    }

    fn exiting(&mut self, event_loop: &ActiveEventLoop) {
        handle_error(self.callback(event_loop, self.exiting.clone(), vec![]));
    }

    fn memory_warning(&mut self, event_loop: &ActiveEventLoop) {
        handle_error(self.callback(event_loop, self.memory_warning.clone(), vec![]));
    }
}
