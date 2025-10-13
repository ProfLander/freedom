use std::{
    cell::OnceCell,
    rc::Rc,
    sync::atomic::{AtomicBool, Ordering},
};

use freedom::scheme::{
    Result, SchemeConfig,
    r#async::Executor,
    log::{handle_error, handle_error_with, info},
    steel::{
        SteelVal,
        rvals::{FromSteelVal, IntoSteelVal},
        steelerr, stop, throw,
    },
    steel_future,
};

use winit::{
    application::ApplicationHandler,
    event::{DeviceId, WindowEvent},
    event_loop::{ActiveEventLoop, ControlFlow, EventLoopProxy},
    window::WindowId,
};

use crate::{
    UserEvent, callback::Callback, event_loop::EventLoop, into_steelval::WinitIntoSteelVal,
    window::Window,
};

static APP_RESUMED: &str = "*resumed*";
static APP_SUSPENDED: &str = "*suspended*";
static APP_NEW_EVENTS: &str = "*new-events*";
static APP_DEVICE_EVENT: &str = "*device-event*";
static APP_WINDOW_EVENT: &str = "*window-event*";
static APP_ABOUT_TO_WAIT: &str = "*about-to-wait*";
static APP_EXITING: &str = "*exiting*";
static APP_MEMORY_WARNING: &str = "*memory-warning*";

thread_local! {
    static PROXY: OnceCell<EventLoopProxy<UserEvent>> = OnceCell::new();
}

pub struct App {
    executor: Executor,
    polling: Rc<AtomicBool>,
}

impl App {
    pub fn run(config: SchemeConfig, executor: Executor, init: SteelVal) -> Result<SteelVal> {
        let SteelVal::ListV(init) = init else {
            stop!(TypeMismatch => "Expected a list, got: {init}");
        };

        let init = init.into_iter().fold(String::default(), |mut acc, next| {
            acc = acc + &next.to_string();
            acc
        });

        freedom::scheme::init(config, executor.clone())?;

        freedom::scheme::with_engine_mut(|engine| {
            EventLoop::register_type(engine);
            Window::register_type(engine);
            engine
                .register_value(APP_RESUMED, false.into())
                .register_value(APP_SUSPENDED, false.into())
                .register_value(APP_NEW_EVENTS, false.into())
                .register_value(APP_DEVICE_EVENT, false.into())
                .register_value(APP_WINDOW_EVENT, false.into())
                .register_value(APP_ABOUT_TO_WAIT, false.into())
                .register_value(APP_EXITING, false.into())
                .register_value(APP_MEMORY_WARNING, false.into())
                .run(init)
        })?;

        // Return a future to run the winit event loop
        Ok(steel_future(async move {
            let el = winit::event_loop::EventLoop::<UserEvent>::with_user_event()
                .build()
                .or_else(|e| steelerr!(Generic => e))?;
            el.set_control_flow(ControlFlow::Poll);

            PROXY
                .with(|proxy| proxy.set(el.create_proxy()))
                .or_else(|_| steelerr!(Infallible => "Event loop proxy already set"))?;

            info!("Entering event loop");
            el.run_app(&mut App {
                executor,
                polling: Rc::new(AtomicBool::new(false)),
            })
            .or_else(|e| steelerr!(Generic => e))?;
            info!("Exiting event loop");

            Ok(SteelVal::Void)
        }))
    }

    pub fn send(event: UserEvent) -> Result<SteelVal> {
        PROXY.with(|proxy| {
            proxy
                .get()
                .ok_or_else(throw!(Generic => "EventLoopProxy accessed before initialization"))?
                .send_event(event)
                .or_else(|e| steelerr!(Generic => e))
        })?;
        Ok(SteelVal::Void)
    }

    fn call(
        &self,
        event_loop: &ActiveEventLoop,
        callback: SteelVal,
        mut args: Vec<SteelVal>,
    ) -> Result<SteelVal> {
        if !matches!(callback, SteelVal::BoolV(false)) {
            let el = EventLoop::new();
            args.insert(0, el.clone().into_steelval()?);
            let res = freedom::scheme::with_engine_mut(|engine| {
                let res = engine.call_function_with_args(callback, args)?;
                Ok(res) as Result<SteelVal>
            })?;
            el.apply(event_loop);
            return Ok(res);
        }
        Ok(SteelVal::Void)
    }

    fn callback(
        &self,
        event_loop: &ActiveEventLoop,
        callback: &'static str,
        args: Vec<SteelVal>,
    ) -> Result<SteelVal> {
        let callback = freedom::scheme::with_engine(|engine| engine.extract(callback))?;
        self.call(event_loop, callback, args)
    }
}

impl ApplicationHandler<UserEvent> for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        handle_error(self.callback(event_loop, APP_RESUMED, vec![]));
    }

    fn suspended(&mut self, event_loop: &ActiveEventLoop) {
        handle_error(self.callback(event_loop, APP_SUSPENDED, vec![]));
    }

    fn new_events(&mut self, event_loop: &ActiveEventLoop, cause: winit::event::StartCause) {
        handle_error_with(|| {
            self.callback(event_loop, APP_NEW_EVENTS, vec![cause.into_steelval()?])
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
                APP_DEVICE_EVENT,
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
                APP_WINDOW_EVENT,
                vec![window_id.into_steelval()?, event.into_steelval()?],
            )
        });
    }

    fn user_event(&mut self, event_loop: &ActiveEventLoop, event: UserEvent) {
        handle_error_with(|| {
            let callback = Callback::from_steelval(&event)?.into_steelval()?;
            self.call(event_loop, callback, vec![])
        })
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
        // Run callback
        handle_error(self.callback(event_loop, APP_ABOUT_TO_WAIT, vec![]));

        // Fetch executor
        let exe = self.executor.unwrap();

        // Set polling flag
        self.polling.store(true, Ordering::Relaxed);

        // Spawn a future to unset the polling flag
        exe.spawn({
            let polling = self.polling.clone();
            async move {
                polling.store(false, Ordering::Relaxed);
            }
        })
        .detach();

        // Poll the executor until the flag is unset,
        // ensuring winit and other tasks get fair time share
        while self.polling.load(Ordering::Relaxed) {
            // The return value is meaningless here
            // As winit is an infinitely-blocking task, the executor will never finish
            exe.try_tick();
        }
    }

    fn exiting(&mut self, event_loop: &ActiveEventLoop) {
        handle_error(self.callback(event_loop, APP_EXITING, vec![]));
    }

    fn memory_warning(&mut self, event_loop: &ActiveEventLoop) {
        handle_error(self.callback(event_loop, APP_MEMORY_WARNING, vec![]));
    }
}
