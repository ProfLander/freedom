use std::sync::{Arc, Mutex};

use freedom_async::smol::channel::bounded;
use freedom_scheme::steel::{
    SteelVal,
    rvals::Custom,
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
};
use winit::{event_loop::ActiveEventLoop, window::WindowAttributes};

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

    pub fn register_type(module: &mut BuiltInModule) {
        module
            .register_fn(
                "ActiveEventLoop-create-window",
                |this: Self, attributes: SteelVal| {
                    let attributes = WindowAttributes::from_steelval(attributes).unwrap();
                    async move {
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
                        rx.recv().await.unwrap()
                    }
                },
            )
            .register_fn("ActiveEventLoop-exit", |this: Self| async move {
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
