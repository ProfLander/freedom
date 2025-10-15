use cpal::traits::HostTrait;
use std::{cell::RefCell, rc::Rc};

use freedom::scheme::{
    Result,
    steel::{
        SteelVal,
        rvals::{Custom, FromSteelVal, IntoSteelVal},
        steel_vm::builtin::{Arity, BuiltInModule},
        steelerr, throw,
    },
};

use crate::device::Device;

#[derive(Clone)]
pub struct Host {
    inner: Rc<RefCell<cpal::Host>>,
}

impl Host {
    pub fn register_type(module: &mut BuiltInModule) {
        module
            .register_native_fn(
                "Host-default",
                |_args| Host::default().into_steelval(),
                Arity::Exact(0),
            )
            .register_native_fn(
                "Host-default-output-device",
                |args| {
                    Host::from_steelval(&args[0])?
                        .default_output_device()?
                        .into_steelval()
                },
                Arity::Exact(1),
            )
            .register_native_fn(
                "Host-default-input-device",
                |args| {
                    Host::from_steelval(&args[0])?
                        .default_input_device()?
                        .into_steelval()
                },
                Arity::Exact(1),
            )
            .register_native_fn(
                "Host-output-devices",
                |args| {
                    let host = Host::from_steelval(&args[0])?;
                    let devices = host.output_devices()?;
                    Ok(SteelVal::ListV(
                        devices
                            .into_iter()
                            .map(|d| d.into_steelval())
                            .collect::<Result<Vec<_>>>()?
                            .into(),
                    ))
                },
                Arity::Exact(1),
            );
    }

    pub fn default() -> Self {
        Host {
            inner: Rc::new(RefCell::new(cpal::default_host())),
        }
    }

    pub fn default_output_device(&self) -> Result<Option<Device>> {
        Ok(self
            .inner
            .borrow()
            .default_output_device()
            .map(|d| Device::new(d)))
    }

    pub fn default_input_device(&self) -> Result<Option<Device>> {
        Ok(self
            .inner
            .borrow()
            .default_input_device()
            .map(|d| Device::new(d)))
    }

    pub fn output_devices(&self) -> Result<Vec<Device>> {
        let devices = self
            .inner
            .borrow()
            .output_devices()
            .or_else(|e| steelerr!(Generic => e))?
            .map(|d| Device::new(d))
            .collect();
        Ok(devices)
    }
}

impl Custom for Host {}
