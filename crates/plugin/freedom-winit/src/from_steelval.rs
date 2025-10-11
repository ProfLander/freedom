use std::collections::BTreeMap;

use freedom::scheme::steel::{SteelErr, SteelVal, stop};
use winit::{
    dpi::{Position, Size},
    window::{Cursor, Fullscreen, Icon, Theme, WindowAttributes, WindowButtons, WindowLevel},
};

pub trait WinitFromSteelVal: Sized {
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr>;
}

impl<T> WinitFromSteelVal for Option<T>
where
    T: WinitFromSteelVal,
{
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr> {
        match val {
            SteelVal::BoolV(false) => Ok(None),
            _ => Ok(Some(T::from_steelval(val)?)),
        }
    }
}

impl WinitFromSteelVal for bool {
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr> {
        let SteelVal::BoolV(b) = val else {
            stop!(TypeMismatch => "Expected a boolean, got {}", val)
        };
        Ok(b)
    }
}

impl WinitFromSteelVal for String {
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr> {
        let SteelVal::StringV(str) = val else {
            stop!(TypeMismatch => "Expected a string, got {}", val)
        };
        Ok(str.to_string())
    }
}

impl WinitFromSteelVal for Position {
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr> {
        todo!()
    }
}

impl WinitFromSteelVal for Size {
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr> {
        todo!()
    }
}

impl WinitFromSteelVal for Icon {
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr> {
        todo!()
    }
}

impl WinitFromSteelVal for Theme {
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr> {
        todo!()
    }
}

impl WinitFromSteelVal for WindowButtons {
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr> {
        todo!()
    }
}

impl WinitFromSteelVal for WindowLevel {
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr> {
        todo!()
    }
}

impl WinitFromSteelVal for Fullscreen {
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr> {
        todo!()
    }
}

impl WinitFromSteelVal for Cursor {
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr> {
        todo!()
    }
}

fn remap<T>(
    att: WindowAttributes,
    f: impl FnOnce(WindowAttributes, T) -> WindowAttributes,
    val: SteelVal,
) -> WindowAttributes
where
    T: WinitFromSteelVal,
{
    f(att, T::from_steelval(val).unwrap())
}

impl WinitFromSteelVal for WindowAttributes {
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr> {
        let list = val.list().unwrap();
        let map = list
            .into_iter()
            .map(|val| {
                let list = val.list().unwrap();
                let head = list.car().unwrap().as_string().unwrap().to_string();
                let tail = list.cdr().unwrap().car().unwrap();
                (head, tail)
            })
            .collect::<BTreeMap<_, _>>();

        let mut att = WindowAttributes::default();
        for (k, v) in map {
            att = match k.as_str() {
                "inner-size" => remap(att, WindowAttributes::with_inner_size::<Size>, v),
                "min-inner-size" => remap(att, WindowAttributes::with_min_inner_size::<Size>, v),
                "max-inner-size" => remap(att, WindowAttributes::with_max_inner_size::<Size>, v),
                "position" => remap(att, WindowAttributes::with_position::<Position>, v),
                "resizable" => remap(att, WindowAttributes::with_resizable, v),
                "enabled-buttons" => remap(att, WindowAttributes::with_enabled_buttons, v),
                "title" => remap(att, WindowAttributes::with_title::<String>, v),
                "maximized" => remap(att, WindowAttributes::with_maximized, v),
                "visible" => remap(att, WindowAttributes::with_visible, v),
                "transparent" => remap(att, WindowAttributes::with_transparent, v),
                "blur" => remap(att, WindowAttributes::with_blur, v),
                "decorations" => remap(att, WindowAttributes::with_decorations, v),
                "window-icon" => remap(att, WindowAttributes::with_window_icon, v),
                "theme" => remap(att, WindowAttributes::with_theme, v),
                "resize-increments" => {
                    remap(att, WindowAttributes::with_resize_increments::<Size>, v)
                }
                "content-protected" => remap(att, WindowAttributes::with_content_protected, v),
                "window-level" => remap(att, WindowAttributes::with_window_level, v),
                "active" => remap(att, WindowAttributes::with_active, v),
                "cursor" => remap::<Cursor>(att, WindowAttributes::with_cursor, v),
                "fullscreen" => remap(att, WindowAttributes::with_fullscreen, v),
                _ => att,
            };
        }

        Ok(att)
    }
}
