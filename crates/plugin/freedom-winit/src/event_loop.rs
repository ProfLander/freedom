use std::sync::{Arc, Mutex};

use winit::{event_loop::ActiveEventLoop, window::WindowAttributes};

use freedom::scheme::{
    r#async::smol::channel::bounded,
    steel::{
        SteelVal,
        rvals::{Custom, IntoSteelVal},
        steel_vm::{engine::Engine as SteelEngine, register_fn::RegisterFn},
        steelerr,
    },
    steel_future,
};

use crate::{from_steelval::WinitFromSteelVal, window::Window};

#[derive(Clone)]
pub struct EventLoop(Arc<Mutex<Vec<Box<dyn FnOnce(&ActiveEventLoop) + Send>>>>);

impl Custom for EventLoop {}

impl EventLoop {
    pub fn new() -> Self {
        EventLoop(Arc::new(Mutex::new(vec![])))
    }

    pub fn len(&self) -> usize {
        self.0.lock().unwrap().len()
    }

    pub fn register_type(engine: &mut SteelEngine) {
        engine
            .register_fn(
                "EventLoop-create-window",
                |this: Self, attributes: SteelVal| {
                    let attributes = WindowAttributes::from_steelval(attributes).unwrap();
                    let (tx, rx) = bounded(1);
                    this.0.lock().unwrap().push(Box::new(move |event_loop| {
                        tx.send_blocking(
                            event_loop
                                .create_window(attributes)
                                .and_then(|t| Ok(Window(t)))
                                .unwrap(),
                        )
                        .unwrap();
                    }));
                    steel_future(async move {
                        rx.recv()
                            .await
                            .or_else(|e| steelerr!(Generic => e))?
                            .into_steelval()
                    })
                },
            )
            .register_fn("EventLoop-exit", |this: Self| async move {
                let (tx, rx) = bounded(1);
                this.0.lock().unwrap().push(Box::new(move |event_loop| {
                    event_loop.exit();
                    tx.send_blocking(()).unwrap();
                }));
                rx.recv().await.unwrap()
            });
    }

    pub fn apply(self, event_loop: &ActiveEventLoop) {
        for f in self.0.lock().unwrap().drain(..) {
            f(event_loop);
        }
    }
}
