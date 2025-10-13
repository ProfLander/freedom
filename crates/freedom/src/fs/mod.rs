pub mod into_steelval;
pub mod recursive_mode;
pub mod watcher;

pub use notify_debouncer_full;
use steel::steel_vm::builtin::BuiltInModule;

use crate::fs::watcher::Watcher;

pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/fs");
    Watcher::register_type(&mut module);
    module
}
