use cpal::{SampleFormat, SupportedStreamConfig, SupportedStreamConfigRange};

use freedom::{
    scheme::{
        Result,
        steel::{SteelVal, list, rvals::IntoSteelVal},
    },
    sym,
};

pub trait AudioIntoSteelVal {
    fn into_steelval(&self) -> Result<SteelVal>;
}

impl AudioIntoSteelVal for SupportedStreamConfig {
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(list![
            sym!(SupportedStreamConfig),
            list![
                list![sym!(channels), self.channels().into_steelval()?],
                list![sym!("sample-rate"), self.sample_rate().0.into_steelval()?],
                list![sym!("sample-format"), self.sample_format().into_steelval()?]
            ]
        ])
    }
}

impl AudioIntoSteelVal for SupportedStreamConfigRange {
    fn into_steelval(&self) -> Result<SteelVal> {
        Ok(list![
            sym!(SupportedStreamConfigRange),
            list![
                list![sym!(channels), self.channels().into_steelval()?],
                list![sym!("min-sample-rate"), self.min_sample_rate().0.into_steelval()?],
                list![sym!("max-sample-rate"), self.max_sample_rate().0.into_steelval()?],
                list![sym!("sample-format"), self.sample_format().into_steelval()?]
            ]
        ])
    }
}

impl AudioIntoSteelVal for SampleFormat {
    fn into_steelval(&self) ->Result<SteelVal> {
        Ok(list![
            sym!(SampleFormat),
            match self {
                SampleFormat::I8 => sym!(I8),
                SampleFormat::I16 => sym!(I16),
                SampleFormat::I32 => sym!(I32),
                SampleFormat::I64 => sym!(I64),
                SampleFormat::U8 => sym!(U8),
                SampleFormat::U16 => sym!(U16),
                SampleFormat::U32 => sym!(U32),
                SampleFormat::U64 => sym!(U64),
                SampleFormat::F32 => sym!(F32),
                SampleFormat::F64 => sym!(F64),
                _ => sym!(Unknown),
            }
        ])
    }
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