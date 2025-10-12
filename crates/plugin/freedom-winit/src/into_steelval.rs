use std::error::Error;

use freedom::{
    scheme::steel::{SteelErr, SteelVal, list, rerrs::ErrorKind, rvals::IntoSteelVal},
    sym,
};
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
                    Some(Ok(format!("#%<{:?}>", self.0).into()))
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
                    list![sym!(Exclusive), video_mode_handle.into_steelval()]
                }
                Fullscreen::Borderless(monitor_handle) =>
                    list![sym!(Borderless), monitor_handle.into_steelval()],
            }
        ])
    }
}

impl WinitIntoSteelVal for DeviceEvent {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            sym!(DeviceEvent),
            match self {
                DeviceEvent::Added => list![sym!(Added)],
                DeviceEvent::Removed => list![sym!(Removed)],
                DeviceEvent::MouseMotion { delta } => {
                    list![sym!(MouseMotion), list![sym!(delta), delta]]
                }
                DeviceEvent::MouseWheel { delta } => {
                    list![sym!(MouseWheel), list![sym!(delta), delta.into_steelval()]]
                }
                DeviceEvent::Motion { axis, value } => {
                    list![
                        sym!(Motion),
                        list![sym!(axis), axis],
                        list![sym!(value), value]
                    ]
                }
                DeviceEvent::Button { button, state } => list![
                    sym!(Button),
                    list![sym!(button), button],
                    list![sym!(state), state.into_steelval()]
                ],
                DeviceEvent::Key(raw_key_event) => {
                    list![sym!(Key), raw_key_event.into_steelval()]
                }
            }
        ])
    }
}

impl WinitIntoSteelVal for MouseScrollDelta {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            sym!(MouseScrollDelta),
            match self {
                MouseScrollDelta::LineDelta(x, y) => {
                    list![sym!(LineDelta), x, y]
                }
                MouseScrollDelta::PixelDelta(physical_position) => {
                    list![sym!(PixelDelta), physical_position.into_steelval()]
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
            sym!(PhysicalPosition),
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
            sym!(PhysicalSize),
            list![sym!(width), self.width],
            list![sym!(height), self.height]
        ])
    }
}

impl WinitIntoSteelVal for ElementState {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            sym!(ElementState),
            match self {
                ElementState::Pressed => list![sym!(Pressed)],
                ElementState::Released => list![sym!(Released)],
            }
        ])
    }
}

impl WinitIntoSteelVal for RawKeyEvent {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        let physical_key = list![sym!("physical-key"), self.physical_key.into_steelval()];
        let state = list![sym!(state), self.state.into_steelval()];
        Ok(list![sym!(RawKeyEvent), physical_key, state])
    }
}

impl WinitIntoSteelVal for PhysicalKey {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            sym!(PhysicalKey),
            match self {
                PhysicalKey::Code(key_code) => {
                    list![sym!(Code), key_code.into_steelval()]
                }
                PhysicalKey::Unidentified(native_key_code) => {
                    list![sym!(Unidentified), native_key_code.into_steelval()]
                }
            }
        ])
    }
}

impl WinitIntoSteelVal for KeyCode {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![sym!(KeyCode), format!("{self:?}")])
    }
}

impl WinitIntoSteelVal for NativeKeyCode {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            sym!(NativeKeyCode),
            match self {
                NativeKeyCode::Unidentified => list![sym!(Unidentified)],
                NativeKeyCode::Android(code) => list![sym!(Android), code],
                NativeKeyCode::MacOS(code) => list![sym!(MacOS), code],
                NativeKeyCode::Windows(code) => list![sym!(Windows), code],
                NativeKeyCode::Xkb(code) => list![sym!(Xkb), code],
            }
        ])
    }
}

impl WinitIntoSteelVal for WindowId {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![sym!(WindowId), u64::from(self).into_steelval()])
    }
}

impl WinitIntoSteelVal for StartCause {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            sym!(StartCause),
            match self {
                StartCause::ResumeTimeReached {
                    start,
                    requested_resume,
                } => list![
                    sym!(ResumeTimeReached),
                    list![sym!(start), start],
                    list![sym!("requested-resume"), requested_resume]
                ],
                StartCause::WaitCancelled {
                    start,
                    requested_resume,
                } => list![
                    sym!(WaitCancelled),
                    list![sym!(start), start],
                    list![sym!("requested-resume"), requested_resume]
                ],
                StartCause::Poll => list![sym!(Poll)],
                StartCause::Init => list![sym!(Init)],
            }
        ])
    }
}

impl WinitIntoSteelVal for Ime {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            sym!(Ime),
            match self {
                Ime::Enabled => list![sym!(Enabled)],
                Ime::Preedit(string, cursor) => list![sym!(Preedit), string, cursor],
                Ime::Commit(string) => list![sym!(Commit), string],
                Ime::Disabled => list![sym!(Disabled)],
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
            sym!(Key),
            match self {
                Key::Named(named_key) => list![sym!(Named), named_key.into_steelval()],
                Key::Character(char) => list![sym!(Character), char.to_string()],
                Key::Unidentified(native_key) =>
                    list![sym!(Unidentified), native_key.into_steelval()],
                Key::Dead(char) => list![sym!(Dead), char],
            }
        ])
    }
}

impl WinitIntoSteelVal for NativeKey {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            "NativeKey",
            match self {
                NativeKey::Unidentified => list![sym!(Unidentified)],
                NativeKey::Android(code) => list![sym!(Android), code],
                NativeKey::MacOS(code) => list![sym!(MacOS), code],
                NativeKey::Windows(code) => list![sym!(Windows), code],
                NativeKey::Xkb(code) => list![sym!(Xkb), code],
                NativeKey::Web(smol_str) => list![sym!(Web), smol_str.to_string()],
            }
        ])
    }
}

impl WinitIntoSteelVal for NamedKey {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![sym!(NamedKey), self.to_text()])
    }
}

impl WinitIntoSteelVal for KeyLocation {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            sym!(KeyLocation),
            match self {
                KeyLocation::Standard => list![sym!(Standard)],
                KeyLocation::Left => list![sym!(Left)],
                KeyLocation::Right => list![sym!(Right)],
                KeyLocation::Numpad => list![sym!(Numpad)],
            }
        ])
    }
}

impl WinitIntoSteelVal for KeyEvent {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            sym!(KeyEvent),
            list![sym!("physical-key"), self.physical_key.into_steelval()],
            list![sym!("logical-key"), self.logical_key.into_steelval()],
            list![sym!(text), self.text.map(|str| str.to_string())],
            list![sym!(location), self.location.into_steelval()],
            list![sym!(state), self.state.into_steelval()],
            list![sym!(repeat), self.repeat]
        ])
    }
}

impl WinitIntoSteelVal for MouseButton {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            sym!(MouseButton),
            match self {
                MouseButton::Left => list![sym!(Left)],
                MouseButton::Right => list![sym!(Right)],
                MouseButton::Middle => list![sym!(Middle)],
                MouseButton::Back => list![sym!(Back)],
                MouseButton::Forward => list![sym!(Forward)],
                MouseButton::Other(code) => list![sym!(Other), code],
            }
        ])
    }
}

impl WinitIntoSteelVal for Touch {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            sym!(Touch),
            list![sym!("device-id"), self.device_id.into_steelval()],
            list![sym!(phase), self.phase.into_steelval()],
            list![sym!(location), self.location.into_steelval()],
            list![
                sym!(force),
                self.force.map(WinitIntoSteelVal::into_steelval)
            ],
            list![sym!(id), self.id]
        ])
    }
}

impl WinitIntoSteelVal for TouchPhase {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            sym!(TouchPhase),
            match self {
                TouchPhase::Started => list![sym!(Started)],
                TouchPhase::Moved => list![sym!(Moved)],
                TouchPhase::Ended => list![sym!(Ended)],
                TouchPhase::Cancelled => list![sym!(Cancelled)],
            }
        ])
    }
}

impl WinitIntoSteelVal for Force {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            sym!(Force),
            match self {
                Force::Calibrated {
                    force,
                    max_possible_force,
                    altitude_angle,
                } => list![
                    sym!(Calibrated),
                    list![sym!(force), force],
                    list![sym!("max-possible-force"), max_possible_force],
                    list![sym!("altitude-angle"), altitude_angle]
                ],
                Force::Normalized(force) => list![sym!(Normalized), force],
            }
        ])
    }
}

impl WinitIntoSteelVal for Theme {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            sym!(Theme),
            match self {
                Theme::Light => list![sym!(Light)],
                Theme::Dark => list![sym!(Dark)],
            }
        ])
    }
}

impl WinitIntoSteelVal for WindowButtons {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![sym!(WindowButtons), self.bits()])
    }
}

impl WinitIntoSteelVal for WindowEvent {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(list![
            sym!(WindowEvent),
            match self {
                WindowEvent::ActivationTokenDone { serial, token } => list![
                    sym!(ActivationTokenDone),
                    list![sym!(serial), serial.into_steelval()],
                    list![sym!(token), token.into_steelval()]
                ],
                WindowEvent::Resized(physical_size) =>
                    list![sym!(Resized), physical_size.into_steelval()],
                WindowEvent::Moved(physical_position) => {
                    list![sym!(Moved), physical_position.into_steelval()]
                }
                WindowEvent::CloseRequested => list![sym!(CloseRequested)],
                WindowEvent::Destroyed => list![sym!(Destroyed)],
                WindowEvent::DroppedFile(path_buf) => list![sym!(DroppedFile), path_buf],
                WindowEvent::HoveredFile(path_buf) => list![sym!(HoveredFile), path_buf],
                WindowEvent::HoveredFileCancelled => list![sym!(HoveredFileCancelled)],
                WindowEvent::Focused(focused) => list![sym!(Focused), focused],
                WindowEvent::KeyboardInput {
                    device_id,
                    event,
                    is_synthetic,
                } => list![
                    sym!(KeyboardInput),
                    list![sym!("device-id"), device_id.into_steelval()],
                    list![sym!(event), event.into_steelval()],
                    list![sym!("is-synthetic"), is_synthetic]
                ],
                WindowEvent::ModifiersChanged(modifiers) => {
                    list![sym!(ModifiersChanged), modifiers.into_steelval()]
                }
                WindowEvent::Ime(ime) => list![sym!(Ime), ime.into_steelval()],
                WindowEvent::CursorMoved {
                    device_id,
                    position,
                } => list![
                    sym!(CursorMoved),
                    list![sym!("device-id"), device_id.into_steelval()],
                    list![sym!(position), position.into_steelval()]
                ],
                WindowEvent::CursorEntered { device_id } => {
                    list![
                        sym!(CursorEntered),
                        list![sym!("device-id"), device_id.into_steelval()]
                    ]
                }
                WindowEvent::CursorLeft { device_id } => list![
                    sym!(CursorLeft),
                    list![sym!("device-id"), device_id.into_steelval()]
                ],
                WindowEvent::MouseWheel {
                    device_id,
                    delta,
                    phase,
                } => list![
                    sym!(MouseWheel),
                    list![sym!("device-id"), device_id.into_steelval()],
                    list![sym!(delta), delta.into_steelval()],
                    list![sym!(phase), phase.into_steelval()]
                ],
                WindowEvent::MouseInput {
                    device_id,
                    state,
                    button,
                } => list![
                    sym!(MouseInput),
                    list![sym!("device-id"), device_id.into_steelval()],
                    list![sym!(state), state.into_steelval()],
                    list![sym!(button), button.into_steelval()]
                ],
                WindowEvent::PinchGesture {
                    device_id,
                    delta,
                    phase,
                } => list![
                    sym!(PinchGesture),
                    list![sym!("device-id"), device_id.into_steelval()],
                    list![sym!(delta), delta],
                    list![sym!(phase), phase.into_steelval()]
                ],
                WindowEvent::PanGesture {
                    device_id,
                    delta,
                    phase,
                } => list![
                    sym!(PanGesture),
                    list![sym!("device-id"), device_id.into_steelval()],
                    list![sym!(delta), delta.into_steelval()],
                    list![sym!(phase), phase.into_steelval()]
                ],
                WindowEvent::DoubleTapGesture { device_id } => {
                    list![
                        sym!(DoubleTapGesture),
                        list![sym!("device-id"), device_id.into_steelval()]
                    ]
                }
                WindowEvent::RotationGesture {
                    device_id,
                    delta,
                    phase,
                } => list![
                    sym!(RotationGesture),
                    list![sym!("device-id"), device_id.into_steelval()],
                    list![sym!(delta), delta],
                    list![sym!(phase), phase.into_steelval()]
                ],
                WindowEvent::TouchpadPressure {
                    device_id,
                    pressure,
                    stage,
                } => list![
                    sym!(TouchpadPressure),
                    list![sym!("device-id"), device_id.into_steelval()],
                    list![sym!(pressure), pressure],
                    list![sym!(stage), stage]
                ],
                WindowEvent::AxisMotion {
                    device_id,
                    axis,
                    value,
                } => list![
                    sym!(AxisMotion),
                    list![sym!("device-id"), device_id.into_steelval()],
                    list![sym!(axis), axis],
                    list![sym!(value), value]
                ],
                WindowEvent::Touch(touch) => list!["Touch", touch.into_steelval()],
                WindowEvent::ScaleFactorChanged {
                    scale_factor,
                    inner_size_writer,
                } => list![
                    sym!(ScaleFactorChanged),
                    list![sym!("scale-factor"), scale_factor],
                    list![sym!("inner-size-writer"), inner_size_writer.into_steelval()]
                ],
                WindowEvent::ThemeChanged(theme) =>
                    list![sym!(ThemeChanged), theme.into_steelval()],
                WindowEvent::Occluded(occluded) => list![sym!(Occluded), occluded],
                WindowEvent::RedrawRequested => list![sym!(RedrawRequested)],
            }
        ])
    }
}
