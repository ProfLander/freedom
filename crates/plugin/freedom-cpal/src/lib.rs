pub mod device;
pub mod host;
pub mod into_steelval;
pub mod stream;

use crate::host::Host;
use crate::device::Device;
use crate::stream::Stream;
use freedom::{
    scheme::steel::{SteelVal, list, rvals::IntoSteelVal, steel_vm::builtin::BuiltInModule},
    sym,
};
pub use cpal;

pub fn module() -> BuiltInModule {
    let mut module = BuiltInModule::new("freedom/cpal");
    Host::register_type(&mut module);
    Device::register_type(&mut module);
    Stream::register_type(&mut module);
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