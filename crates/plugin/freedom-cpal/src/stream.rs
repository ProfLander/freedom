use cpal::{
    StreamConfig,
    traits::{DeviceTrait, StreamTrait},
};
use std::sync::{Arc, Mutex};

use freedom::{
    log::{error, warn},
    scheme::{
        Result,
        steel::{
            SteelVal,
            rvals::{Custom, FromSteelVal, IntoSteelVal},
            steel_vm::builtin::{Arity, BuiltInModule},
            steelerr,
        },
    },
};

use crate::device::Device;

#[derive(Clone)]
pub struct Stream {
    inner: Arc<Mutex<Option<cpal::Stream>>>,
}

impl Stream {
    pub fn register_type(module: &mut BuiltInModule) {
        module
            .register_native_fn(
                "Stream-build-output-sine",
                |args| {
                    let device = Device::from_steelval(&args[0])?;
                    let sample_rate = u32::from_steelval(&args[1])?;
                    let frequency = f32::from_steelval(&args[2])?;
                    Stream::build_output_sine(device, sample_rate, frequency)?.into_steelval()
                },
                Arity::Exact(3),
            )
            .register_native_fn(
                "Stream-play",
                |args| Stream::from_steelval(&args[0])?.play(),
                Arity::Exact(1),
            )
            .register_native_fn(
                "Stream-pause",
                |args| Stream::from_steelval(&args[0])?.pause(),
                Arity::Exact(1),
            );
    }

    pub fn build_output_sine(device: Device, sample_rate: u32, frequency: f32) -> Result<Self> {
        let config = StreamConfig {
            channels: 2,
            sample_rate: cpal::SampleRate(sample_rate),
            buffer_size: cpal::BufferSize::Default,
        };

        let mut sample_clock = 0f32;
        let sample_rate_f = sample_rate as f32;

        let stream = device
            .inner()
            .borrow()
            .build_output_stream(
                &config,
                move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                    for sample in data.chunks_mut(2) {
                        let value = (sample_clock * frequency * 2.0 * std::f32::consts::PI
                            / sample_rate_f)
                            .sin();
                        for channel in sample.iter_mut() {
                            *channel = value;
                        }
                        sample_clock += 1.0;
                    }
                },
                move |err| {
                    error!("Audio stream error: {}", err);
                },
                None,
            )
            .or_else(|e| steelerr!(Generic => e))?;

        Ok(Stream {
            inner: Arc::new(Mutex::new(Some(stream))),
        })
    }

    pub fn play(&self) -> Result<SteelVal> {
        if let Some(stream) = self.inner.lock().unwrap().as_ref() {
            stream.play().or_else(|e| steelerr!(Generic => e))?;
            Ok(SteelVal::BoolV(true))
        } else {
            warn!("Stream has been dropped");
            Ok(SteelVal::BoolV(false))
        }
    }

    pub fn pause(&self) -> Result<SteelVal> {
        if let Some(stream) = self.inner.lock().unwrap().as_ref() {
            stream.pause().or_else(|e| steelerr!(Generic => e))?;
            Ok(SteelVal::BoolV(true))
        } else {
            warn!("Stream has been dropped");
            Ok(SteelVal::BoolV(false))
        }
    }
}

impl Custom for Stream {}
