use freedom::scheme::{
    Result,
    steel::{
        SteelVal, list,
        rvals::{FromSteelVal, IntoSteelVal},
        stop,
    },
};

#[derive(Clone)]
pub struct RecursiveMode(notify_debouncer_full::notify::RecursiveMode);

impl RecursiveMode {
    pub fn recursive() -> Self {
        RecursiveMode(notify_debouncer_full::notify::RecursiveMode::Recursive)
    }

    pub fn non_recursive() -> Self {
        RecursiveMode(notify_debouncer_full::notify::RecursiveMode::NonRecursive)
    }

    pub fn unwrap(self) -> notify_debouncer_full::notify::RecursiveMode {
        self.0
    }
}

impl IntoSteelVal for RecursiveMode {
    fn into_steelval(self) -> freedom::scheme::Result<SteelVal> {
        Ok(list![
            "RecursiveMode",
            match self.0 {
                notify_debouncer_full::notify::RecursiveMode::Recursive => list!["Recursive"],
                notify_debouncer_full::notify::RecursiveMode::NonRecursive => list!["NonRecursive"],
            }
        ])
    }
}

impl FromSteelVal for RecursiveMode {
    fn from_steelval(val: &SteelVal) -> Result<RecursiveMode> {
        let SteelVal::ListV(list) = val else {
            stop!(TypeMismatch => "Expected list, got: {}", val)
        };

        let car = list.car();
        let Some(SteelVal::SymbolV(sym)) = list.car() else {
            stop!(UnexpectedToken => "Expected a symbol, got: {:?}", car)
        };

        if !matches!(sym.as_str(), "RecursiveMode") {
            stop!(UnexpectedToken => "Expected 'RecursiveMode, got: {}", sym)
        }

        let cdr = list.cdr().and_then(|cdr| cdr.car());
        let Some(SteelVal::ListV(list)) = cdr else {
            stop!(UnexpectedToken => "Expected a list, got: {:?}", cdr);
        };

        let Some(SteelVal::SymbolV(sym)) = list.car() else {
            stop!(TypeMismatch => "Expected a symbol, got: {:?}", car)
        };

        let res = match sym.as_str() {
            "Recursive" => RecursiveMode::recursive(),
            "NonRecursive" => RecursiveMode::non_recursive(),
            _ => stop!(UnexpectedToken => "Expected 'Recursive or 'NonRecursive, got: {}", sym),
        };

        Ok(res)
    }
}
