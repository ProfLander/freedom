use freedom_scheme::steel::{
    SteelErr, SteelVal,
    rvals::Custom,
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
};
use winit::window::Window;

use crate::into_steelval::WinitIntoSteelVal;

pub struct FreedomWindow(pub Window);

impl Custom for FreedomWindow {}

macro_rules! impl_arity_0_inner {
    ($ident:ident) => {
        pub fn $ident(&self) -> SteelVal {
            self.0.$ident().into_steelval().unwrap()
        }
    };
}

macro_rules! impl_arity_0 {
    ($($ident:ident),*) => {
        $(impl_arity_0_inner!($ident);)*
    };
}

macro_rules! register_impl {
    ($module:ident, $ident:ident, $name:expr) => {
        $module.register_fn($name, Self::$ident);
    };
}

macro_rules! register {
    ($module:ident { $($ident:ident : $name:expr),* }) => {
        $(register_impl!($module, $ident, $name);)*
    };
}

impl FreedomWindow {
    pub fn register_type(module: &mut BuiltInModule) {
        register!(module {
            id: "Window-id",
            scale_factor: "Window-scale-factor",
            request_redraw: "Window-request-redraw",
            pre_present_notify: "Window-pre-present-notify",
            inner_position: "Window-inner-position",
            outer_position: "Window-outer-position",
            reset_dead_keys: "Window-reset-dead-keys",
            //set_outer_position: "Window-set-outer-position",
            inner_size: "Window-inner-size",
            //request_inner_size: "Window-request-inner-size",
            outer_size: "Window-outer-size",
            //set_min_inner_size: "Window-set-min-inner-size",
            //set_max_inner_size: "Window-set-max-inner-size",
            resize_increments: "Window-resize-increments",
            //set_resize_increments: "Window-set-resize-increments",
            //set_title: "Window-set-title",
            //set_transparent: "Window-set-transparent",
            //set_blur: "Window-set-blur",
            //set_visible: "Window-set-visible",
            is_visible: "Window-is-visible",
            //set_resizable: "Window-set-resizable",
            is_resizable: "Window-is-resizable",
            //set_enabled_buttons: "Window-set-enabled-buttons",
            enabled_buttons: "Window-enabled-buttons",
            //set_minimized: "Window-set-minimized",
            is_minimized: "Window-is-minimized",
            //set_maximized: "Window-set-maximized",
            is_maximized: "Window-is-maximized",
            //set_fullscreen: "Window-set-fullscreen",
            fullscreen: "Window-fullscreen",
            //set_decorations: "Window-set-decorations",
            is_decorated: "Window-is-decorated",
            //set_window_level: "Window-set-window-level",
            //set_window_icon: "Window-set-window-icon",
            //set_ime_cursor_area: "Window-set-ime-cursor-area",
            //set_ime_allowed: "Window-set-ime-allowed",
            //set_ime_purpose: "Window-set-ime-purpose",
            focus_window: "Window-focus-window",
            has_focus: "Window-has-focus",
            //request_user_attention: "Window-request-user-attention",
            //set_theme: "Window-set-theme",
            theme: "Window-theme",
            //set_context_protected: "Window-set-context-protected",
            title: "Window-title",
            //set_cursor: "Window-set-cursor",
            //set_cursor_position: "Window-set-cursor-position",
            //set_cursor_grab: "Window-set-cursor-grab",
            //set_cursor_visible: "Window-set-cursor-visible",
            drag_window: "Window-drag-window",
            //drag_resize_window: "Window-drag-resize-window",
            //show_window_menu: "Window-show-window-menu",
            //set_cursor_hittest: "Window-set-cursor-hittest"
            current_monitor: "Window-current-monitor",
            available_monitors: "Window-available-monitors",
            primary_monitor: "Window-primary-monitor"
        });
    }

    impl_arity_0! {
        id,
        scale_factor,
        request_redraw,
        pre_present_notify,
        reset_dead_keys,
        inner_position,
        outer_position,
        inner_size,
        outer_size,
        resize_increments,
        is_visible,
        is_resizable,
        enabled_buttons,
        is_minimized,
        is_maximized,
        fullscreen,
        is_decorated,
        focus_window,
        has_focus,
        theme,
        title,
        drag_window,
        current_monitor,
        primary_monitor
    }

    fn available_monitors(&self) -> SteelVal {
        SteelVal::ListV(
            self.0
                .available_monitors()
                .try_fold(vec![], |mut acc, next| {
                    acc.push(next.into_steelval()?);
                    Ok(acc) as Result<_, SteelErr>
                })
                .unwrap()
                .into(),
        )
    }
}

impl WinitIntoSteelVal for Window {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        freedom_scheme::steel::rvals::IntoSteelVal::into_steelval(FreedomWindow(self))
    }
}
