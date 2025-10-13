pub mod into_steelval;
pub mod recursive_mode;
pub mod watcher;

use crate::watcher::Watcher;
use freedom::{
    scheme::steel::{SteelVal, list, rvals::IntoSteelVal, steel_vm::builtin::BuiltInModule},
    sym,
};
pub use notify_debouncer_full;

pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/fs");
    Watcher::register_type(&mut module);
    module
}

#[unsafe(no_mangle)]
pub fn plugin() -> SteelVal {
    list![
        sym!(Plugin),
        list![
            list![sym!(init), SteelVal::FuncV(|_| { Ok(SteelVal::Void) })],
            list![
                sym!(module),
                SteelVal::FuncV(|_| { Ok(module().into_steelval()?) })
            ]
        ]
    ]
}
