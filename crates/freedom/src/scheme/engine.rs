use std::{cell::RefCell, ops::Deref, rc::Rc};

use steel::{rvals::Custom, steel_vm::engine::Engine as SteelEngine};

#[derive(Clone)]
pub struct Engine(Rc<RefCell<SteelEngine>>);

impl Deref for Engine {
    type Target = RefCell<SteelEngine>;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl Custom for Engine {}

impl Engine {
    pub fn new() -> Self {
        Engine(Rc::new(RefCell::new(SteelEngine::new())))
    }
}
