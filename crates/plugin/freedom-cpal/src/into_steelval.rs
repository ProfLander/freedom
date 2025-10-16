use cpal::{SampleFormat, SupportedStreamConfig, SupportedStreamConfigRange};

use freedom::{
    scheme::{
        Result,
        steel::{
            SteelVal,
            rvals::{FromSteelVal, IntoSteelVal},
            steel_vm::builtin::{Arity, BuiltInModule},
        },
    },
    steel_bindings, sym,
};

use crate::{device::Device, host::Host};

pub trait AudioIntoSteelVal {
    fn into_steelval(&self) -> Result<SteelVal>;
}

impl<T> AudioIntoSteelVal for Option<T>
where
    T: AudioIntoSteelVal,
{
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(match self {
            Some(t) => t.into_steelval()?,
            None => SteelVal::BoolV(false),
        })
    }
}

impl<T> AudioIntoSteelVal for Vec<T>
where
    T: AudioIntoSteelVal,
{
    fn into_steelval(&self) -> Result<SteelVal> {
        self.iter()
            .try_fold(vec![], |mut acc, next| {
                acc.push(AudioIntoSteelVal::into_steelval(next)?);
                Ok(acc) as Result<_>
            })
            .into_steelval()
    }
}

// Usage example:
steel_bindings! {
    module: "freedom/cpal",

    types: {
        Host => {
            "Host-default" => default() -> Self,
            "Host-default-output-device" => default_output_device(&self) -> Option<Device>
        },
        Device => {
            "Device-name" => name(&self) -> String,
            "Device-supported-configs" => supported_output_configs(&self) -> Vec<SupportedStreamConfigRange>
        }
    },

    structs: {
        SupportedStreamConfig => SupportedStreamConfig {
            "channels": |s| s.channels(),
            "sample-rate": |s| s.sample_rate().0,
            "sample-format": |s| s.sample_format()
        },
        SupportedStreamConfigRange => SupportedStreamConfigRange {
            "channels": |s| s.channels(),
            "min-sample-rate": |s| s.min_sample_rate().0,
            "max-sample-rate": |s| s.max_sample_rate().0,
            "sample-format": |s| s.sample_format()
        }
    },

    enums: {
        SampleFormat => SampleFormat {
            SampleFormat::I8 => I8,
            SampleFormat::I16 => I16,
            SampleFormat::I32 => I32,
            SampleFormat::F32 => F32,
            SampleFormat::F64 => F64,
            @ _ => Unknown
        }
    }
}
