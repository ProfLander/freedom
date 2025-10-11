use std::error::Error;

use freedom::scheme::steel::{SteelErr, SteelVal, list, rerrs::ErrorKind, rvals::IntoSteelVal};
use winit::{
    dpi::{PhysicalPosition, PhysicalSize},
    event::{
        DeviceEvent, ElementState, Force, Ime, KeyEvent, MouseButton, MouseScrollDelta,
        RawKeyEvent, StartCause, Touch, TouchPhase, WindowEvent,
    },
    keyboard::{Key, KeyCode, KeyLocation, NamedKey, NativeKey, NativeKeyCode, PhysicalKey},
    window::{Fullscreen, Theme, WindowButtons, WindowId},
};
pub trait WinitIntoSteelVal {
    fn into_steelval(self) -> Result<SteelVal, SteelErr>;
}

impl WinitIntoSteelVal for () {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(SteelVal::Void)
    }
}

impl WinitIntoSteelVal for bool {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        IntoSteelVal::into_steelval(self)
    }
}

impl WinitIntoSteelVal for f64 {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        IntoSteelVal::into_steelval(self)
    }
}

impl WinitIntoSteelVal for String {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(SteelVal::StringV(self.into()))
    }
}

impl<T> WinitIntoSteelVal for Option<T>
where
    T: WinitIntoSteelVal,
{
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        match self {
            Some(t) => t.into_steelval(),
            None => Ok(SteelVal::BoolV(false)),
        }
    }
}

impl<T: WinitIntoSteelVal, E: Error> WinitIntoSteelVal for Result<T, E> {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        match self {
            Ok(s) => s.into_steelval(),
            Err(e) => Err(SteelErr::new(ErrorKind::Generic, e.to_string())),
        }
    }
}

macro_rules! opaque_wrappers {
    ($($name:ident($inner:path);)+) => {
        $(
            #[derive(Clone, PartialEq)]
            pub struct $name($inner);

            impl freedom::scheme::steel::rvals::Custom for $name {
                fn fmt(&self) -> Option<Result<String, std::fmt::Error>> {
                    Some(Ok(format!("#%<{}>", stringify!($inner)).into()))
                }

                fn equality_hint(&self, other: &dyn freedom::scheme::steel::rvals::CustomType) -> bool {
                    if let Some(other) = other.as_any_ref().downcast_ref() {
                        self.eq(other)
                    } else {
                        false
                    }
                }

                fn equality_hint_general(&self, other: &SteelVal) -> bool {
                    if let Ok(other) = freedom::scheme::steel::rvals::FromSteelVal::from_steelval(other) {
                        self.eq(&other)
                    } else {
                        false
                    }
                }
            }

            impl WinitIntoSteelVal for $inner {
                fn into_steelval(self) -> Result<SteelVal, SteelErr> {
                    $name(self).into_steelval()
                }
            }
        )+
    };
}

opaque_wrappers! {
    DeviceId(winit::event::DeviceId);
    Modifiers(winit::event::Modifiers);
    ActivationToken(winit::window::ActivationToken);
    AsyncRequestSerial(winit::event_loop::AsyncRequestSerial);
    InnerSizeWriter(winit::event::InnerSizeWriter);
    VideoModeHandle(winit::monitor::VideoModeHandle);
    MonitorHandle(winit::monitor::MonitorHandle);
}

impl WinitIntoSteelVal for Fullscreen {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "Fullscreen",
            match self {
                Fullscreen::Exclusive(video_mode_handle) => {
                    list!["Exclusive", video_mode_handle.into_steelval()]
                }
                Fullscreen::Borderless(monitor_handle) =>
                    list!["Borderless", monitor_handle.into_steelval()],
            }
        ])
    }
}

impl WinitIntoSteelVal for DeviceEvent {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "DeviceEvent",
            match self {
                DeviceEvent::Added => list!["Added"],
                DeviceEvent::Removed => list!["Removed"],
                DeviceEvent::MouseMotion { delta } => {
                    list!["MouseMotion", list!["delta", delta]]
                }
                DeviceEvent::MouseWheel { delta } => {
                    list!["MouseWheel", list!["delta", delta.into_steelval()]]
                }
                DeviceEvent::Motion { axis, value } => {
                    list!["Motion", list!["axis", axis], list!["value", value]]
                }
                DeviceEvent::Button { button, state } => list![
                    "Button",
                    list!["button", button],
                    list!["state", state.into_steelval()]
                ],
                DeviceEvent::Key(raw_key_event) => {
                    list!["Key", raw_key_event.into_steelval()]
                }
            }
        ])
    }
}

impl WinitIntoSteelVal for MouseScrollDelta {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "MouseScrollDelta",
            match self {
                MouseScrollDelta::LineDelta(x, y) => {
                    list!["LineDelta", x, y]
                }
                MouseScrollDelta::PixelDelta(physical_position) => {
                    list!["PixelDelta", physical_position.into_steelval()]
                }
            }
        ])
    }
}

impl<T> WinitIntoSteelVal for PhysicalPosition<T>
where
    T: IntoSteelVal,
{
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "PhysicalPosition",
            list!["x", self.x],
            list!["y", self.y]
        ])
    }
}

impl<T> WinitIntoSteelVal for PhysicalSize<T>
where
    T: IntoSteelVal,
{
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "PhysicalSize",
            list!["width", self.width],
            list!["height", self.height]
        ])
    }
}

impl WinitIntoSteelVal for ElementState {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "ElementState",
            match self {
                ElementState::Pressed => list!["Pressed"],
                ElementState::Released => list!["Released"],
            }
        ])
    }
}

impl WinitIntoSteelVal for RawKeyEvent {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        let physical_key = list!["physical-key", self.physical_key.into_steelval()];
        let state = list!["state", self.state.into_steelval()];
        Ok(list!["RawKeyEvent", physical_key, state])
    }
}

impl WinitIntoSteelVal for PhysicalKey {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "PhysicalKey",
            match self {
                PhysicalKey::Code(key_code) => {
                    list!["Code", key_code.into_steelval()]
                }
                PhysicalKey::Unidentified(native_key_code) => {
                    list!["Unidentified", native_key_code.into_steelval()]
                }
            }
        ])
    }
}

impl WinitIntoSteelVal for KeyCode {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list!["KeyCode", format!("{self:?}")])
    }
}

impl WinitIntoSteelVal for NativeKeyCode {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "NativeKeyCode",
            match self {
                NativeKeyCode::Unidentified => list!["Unidentified"],
                NativeKeyCode::Android(code) => list!["Android", code],
                NativeKeyCode::MacOS(code) => list!["MacOS", code],
                NativeKeyCode::Windows(code) => list!["Windows", code],
                NativeKeyCode::Xkb(code) => list!["Xkb", code],
            }
        ])
    }
}

impl WinitIntoSteelVal for WindowId {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list!["WindowId", u64::from(self).into_steelval()])
    }
}

impl WinitIntoSteelVal for StartCause {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "StartCause",
            match self {
                StartCause::ResumeTimeReached {
                    start,
                    requested_resume,
                } => list![
                    "ResumeTimeReached",
                    list!["start", start],
                    list!["requested-resume", requested_resume]
                ],
                StartCause::WaitCancelled {
                    start,
                    requested_resume,
                } => list![
                    "WaitCancelled",
                    list!["start", start],
                    list!["requested-resume", requested_resume]
                ],
                StartCause::Poll => list!["Poll"],
                StartCause::Init => list!["Init"],
            }
        ])
    }
}

impl WinitIntoSteelVal for Ime {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "Ime",
            match self {
                Ime::Enabled => list!["Enabled"],
                Ime::Preedit(string, cursor) => list!["Preedit", string, cursor],
                Ime::Commit(string) => list!["Commit", string],
                Ime::Disabled => list!["Disabled"],
            }
        ])
    }
}

impl<T> WinitIntoSteelVal for Key<T>
where
    T: ToString,
{
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "Key",
            match self {
                Key::Named(named_key) => list!["Named", named_key.into_steelval()],
                Key::Character(char) => list!["Character", char.to_string()],
                Key::Unidentified(native_key) => list!["Unidentified", native_key.into_steelval()],
                Key::Dead(char) => list!["Dead", char],
            }
        ])
    }
}

impl WinitIntoSteelVal for NativeKey {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "NativeKey",
            match self {
                NativeKey::Unidentified => list!["Unidentified"],
                NativeKey::Android(code) => list!["Android", code],
                NativeKey::MacOS(code) => list!["MacOS", code],
                NativeKey::Windows(code) => list!["Windows", code],
                NativeKey::Xkb(code) => list!["Xkb", code],
                NativeKey::Web(smol_str) => list!["Web", smol_str.to_string()],
            }
        ])
    }
}

impl WinitIntoSteelVal for NamedKey {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list!["NamedKey", self.to_text()])
    }
}

impl WinitIntoSteelVal for KeyLocation {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "KeyLocation",
            match self {
                KeyLocation::Standard => list!["Standard"],
                KeyLocation::Left => list!["Left"],
                KeyLocation::Right => list!["Right"],
                KeyLocation::Numpad => list!["Numpad"],
            }
        ])
    }
}

impl WinitIntoSteelVal for KeyEvent {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "KeyEvent",
            list!["physical-key", self.physical_key.into_steelval()],
            list!["logical-key", self.logical_key.into_steelval()],
            list!["text", self.text.map(|str| str.to_string())],
            list!["location", self.location.into_steelval()],
            list!["state", self.state.into_steelval()],
            list!["repeat", self.repeat]
        ])
    }
}

impl WinitIntoSteelVal for MouseButton {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "MouseButton",
            match self {
                MouseButton::Left => list!["Left"],
                MouseButton::Right => list!["Right"],
                MouseButton::Middle => list!["Middle"],
                MouseButton::Back => list!["Back"],
                MouseButton::Forward => list!["Forward"],
                MouseButton::Other(code) => list!["Other", code],
            }
        ])
    }
}

impl WinitIntoSteelVal for Touch {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "Touch",
            list!["device-id", self.device_id.into_steelval()],
            list!["phase", self.phase.into_steelval()],
            list!["location", self.location.into_steelval()],
            list!["force", self.force.map(WinitIntoSteelVal::into_steelval)],
            list!["id", self.id]
        ])
    }
}

impl WinitIntoSteelVal for TouchPhase {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "TouchPhase",
            match self {
                TouchPhase::Started => list!["Started"],
                TouchPhase::Moved => list!["Moved"],
                TouchPhase::Ended => list!["Ended"],
                TouchPhase::Cancelled => list!["Cancelled"],
            }
        ])
    }
}

impl WinitIntoSteelVal for Force {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "Force",
            match self {
                Force::Calibrated {
                    force,
                    max_possible_force,
                    altitude_angle,
                } => list![
                    "Calibrated",
                    list!["force", force],
                    list!["max-possible-force", max_possible_force],
                    list!["altitude-angle", altitude_angle]
                ],
                Force::Normalized(force) => list!["Normalized", force],
            }
        ])
    }
}

impl WinitIntoSteelVal for Theme {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "Theme",
            match self {
                Theme::Light => list!["Light"],
                Theme::Dark => list!["Dark"],
            }
        ])
    }
}

impl WinitIntoSteelVal for WindowButtons {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list!["WindowButtons", self.bits()])
    }
}

impl WinitIntoSteelVal for WindowEvent {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "WindowEvent",
            match self {
                WindowEvent::ActivationTokenDone { serial, token } => list![
                    "ActivationTokenDone",
                    list!["serial", serial.into_steelval()],
                    list!["token", token.into_steelval()]
                ],
                WindowEvent::Resized(physical_size) =>
                    list!["Resized", physical_size.into_steelval()],
                WindowEvent::Moved(physical_position) => {
                    list!["Moved", physical_position.into_steelval()]
                }
                WindowEvent::CloseRequested => list!["CloseRequested"],
                WindowEvent::Destroyed => list!["Destroyed"],
                WindowEvent::DroppedFile(path_buf) => list!["DroppedFile", path_buf],
                WindowEvent::HoveredFile(path_buf) => list!["HoveredFile", path_buf],
                WindowEvent::HoveredFileCancelled => list!["HoveredFileCancelled"],
                WindowEvent::Focused(focused) => list!["Focused", focused],
                WindowEvent::KeyboardInput {
                    device_id,
                    event,
                    is_synthetic,
                } => list![
                    "KeyboardInput",
                    list!["device-id", device_id.into_steelval()],
                    list!["event", event.into_steelval()],
                    list!["is-synthetic", is_synthetic]
                ],
                WindowEvent::ModifiersChanged(modifiers) => {
                    list!["ModifiersChanged", modifiers.into_steelval()]
                }
                WindowEvent::Ime(ime) => list!["Ime", ime.into_steelval()],
                WindowEvent::CursorMoved {
                    device_id,
                    position,
                } => list![
                    "CursorMoved",
                    list!["device-id", device_id.into_steelval()],
                    list!["position", position.into_steelval()]
                ],
                WindowEvent::CursorEntered { device_id } => {
                    list![
                        "CursorEntered",
                        list!["device-id", device_id.into_steelval()]
                    ]
                }
                WindowEvent::CursorLeft { device_id } =>
                    list!["CursorLeft", list!["device-id", device_id.into_steelval()]],
                WindowEvent::MouseWheel {
                    device_id,
                    delta,
                    phase,
                } => list![
                    "MouseWheel",
                    list!["device-id", device_id.into_steelval()],
                    list!["delta", delta.into_steelval()],
                    list!["phase", phase.into_steelval()]
                ],
                WindowEvent::MouseInput {
                    device_id,
                    state,
                    button,
                } => list![
                    "MouseInput",
                    list!["device-id", device_id.into_steelval()],
                    list!["state", state.into_steelval()],
                    list!["button", button.into_steelval()]
                ],
                WindowEvent::PinchGesture {
                    device_id,
                    delta,
                    phase,
                } => list![
                    "PinchGesture",
                    list!["device-id", device_id.into_steelval()],
                    list!["delta", delta],
                    list!["phase", phase.into_steelval()]
                ],
                WindowEvent::PanGesture {
                    device_id,
                    delta,
                    phase,
                } => list![
                    "PanGesture",
                    list!["device-id", device_id.into_steelval()],
                    list!["delta", delta.into_steelval()],
                    list!["phase", phase.into_steelval()]
                ],
                WindowEvent::DoubleTapGesture { device_id } => {
                    list![
                        "DoubleTapGesture",
                        list!["device-id", device_id.into_steelval()]
                    ]
                }
                WindowEvent::RotationGesture {
                    device_id,
                    delta,
                    phase,
                } => list![
                    "RotationGesture",
                    list!["device-id", device_id.into_steelval()],
                    list!["delta", delta],
                    list!["phase", phase.into_steelval()]
                ],
                WindowEvent::TouchpadPressure {
                    device_id,
                    pressure,
                    stage,
                } => list![
                    "TouchpadPressure",
                    list!["device-id", device_id.into_steelval()],
                    list!["pressure", pressure],
                    list!["stage", stage]
                ],
                WindowEvent::AxisMotion {
                    device_id,
                    axis,
                    value,
                } => list![
                    "AxisMotion",
                    list!["device-id", device_id.into_steelval()],
                    list!["axis", axis],
                    list!["value", value]
                ],
                WindowEvent::Touch(touch) => list!["Touch", touch.into_steelval()],
                WindowEvent::ScaleFactorChanged {
                    scale_factor,
                    inner_size_writer,
                } => list![
                    "ScaleFactorChanged",
                    list!["scale-factor", scale_factor],
                    list!["inner-size-writer", inner_size_writer.into_steelval()]
                ],
                WindowEvent::ThemeChanged(theme) => list!["ThemeChanged", theme.into_steelval()],
                WindowEvent::Occluded(occluded) => list!["Occluded", occluded],
                WindowEvent::RedrawRequested => list!["RedrawRequested"],
            }
        ])
    }
}
