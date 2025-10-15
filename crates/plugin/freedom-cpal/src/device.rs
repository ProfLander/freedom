use cpal::traits::DeviceTrait;
use std::{cell::RefCell, rc::Rc};

use freedom::scheme::{
    Result,
    steel::{
        SteelVal,
        rvals::{Custom, FromSteelVal, IntoSteelVal},
        steel_vm::builtin::{Arity, BuiltInModule},
        steelerr,
    },
};

use crate::into_steelval::AudioIntoSteelVal;

#[derive(Clone)]
pub struct Device {
    inner: Rc<RefCell<cpal::Device>>,
}

impl Device {
    pub fn register_type(module: &mut BuiltInModule) {
        module
            .register_native_fn(
                "Device-name",
                |args| {
                    let device = Device::from_steelval(&args[0])?;
                    device.name()?.into_steelval()
                },
                Arity::Exact(1),
            )
            .register_native_fn(
                "Device-default-output-config",
                |args| {
                    let device = Device::from_steelval(&args[0])?;
                    device.default_output_config()?.into_steelval()
                },
                Arity::Exact(1),
            )
            .register_native_fn(
                "Device-default-input-config",
                |args| {
                    let device = Device::from_steelval(&args[0])?;
                    device.default_input_config()?.into_steelval()
                },
                Arity::Exact(1),
            )
            .register_native_fn(
                "Device-supported-output-configs",
                |args| {
                    let device = Device::from_steelval(&args[0])?;
                    let configs = device.supported_output_configs()?;
                    Ok(SteelVal::ListV(
                        configs
                            .into_iter()
                            .map(|c| c.into_steelval())
                            .collect::<Result<Vec<_>>>()?
                            .into(),
                    ))
                },
                Arity::Exact(1),
            );
    }

    pub fn new(device: cpal::Device) -> Self {
        Device {
            inner: Rc::new(RefCell::new(device)),
        }
    }

    pub fn name(&self) -> Result<String> {
        self.inner
            .borrow()
            .name()
            .or_else(|e| steelerr!(Generic => e))
    }

    pub fn default_output_config(&self) -> Result<cpal::SupportedStreamConfig> {
        self.inner
            .borrow()
            .default_output_config()
            .or_else(|e| steelerr!(Generic => e))
    }

    pub fn default_input_config(&self) -> Result<cpal::SupportedStreamConfig> {
        self.inner
            .borrow()
            .default_input_config()
            .or_else(|e| steelerr!(Generic => e))
    }

    pub fn supported_output_configs(&self) -> Result<Vec<cpal::SupportedStreamConfigRange>> {
        self.inner
            .borrow()
            .supported_output_configs()
            .or_else(|e| steelerr!(Generic => e))
            .map(|configs| configs.collect())
    }

    pub fn inner(&self) -> Rc<RefCell<cpal::Device>> {
        self.inner.clone()
    }
}

impl Custom for Device {}
