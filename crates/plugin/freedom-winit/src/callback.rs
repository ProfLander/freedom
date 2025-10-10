use freedom_scheme::{
    Result,
    steel::{
        SteelVal,
        rvals::{FromSteelVal, IntoSteelVal},
        stop,
    },
};

#[derive(Clone)]
pub struct Callback(SteelVal);

impl FromSteelVal for Callback {
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        if let SteelVal::Void = val {
            return Ok(Callback(SteelVal::Void));
        }
        let SteelVal::ListV(list) = val else {
            stop!(TypeMismatch => "Expected a list, got: {}", val)
        };

        let head = list.car();
        let Some(SteelVal::SymbolV(quote)) = &head else {
            stop!(TypeMismatch => "Expected a symbol, got: {:?}", head)
        };

        if quote.as_str() != "lambda" {
            stop!(UnexpectedToken => "Expected a quoted lambda, got: {}", val)
        }

        let mut exps = freedom_scheme::with_engine_mut(|engine| engine.run(format!("{}", val)))?;

        if exps.len() != 1 {
            stop!(ArityMismatch => "Expected an output of length 1, got: {:?}", exps)
        }

        let func = exps.remove(0);

        let SteelVal::Closure(_) = func else {
            stop!(TypeMismatch => "Expected a closure, got: {}", func)
        };

        Ok(Callback(func))
    }
}

impl IntoSteelVal for Callback {
    fn into_steelval(self) -> freedom_scheme::steel::rvals::Result<SteelVal> {
        Ok(self.0)
    }
}
