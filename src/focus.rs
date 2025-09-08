use std::borrow::Cow;

use smithay::desktop::{Window, WindowSurface};
pub use smithay::{
    backend::input::KeyState,
    desktop::{LayerSurface, PopupKind},
    input::{
        keyboard::{KeyboardTarget, KeysymHandle, ModifiersState},
        Seat,
    },
    reexports::wayland_server::{backend::ObjectId, protocol::wl_surface::WlSurface, Resource},
    utils::{IsAlive, Serial},
    wayland::seat::WaylandFocus,
};

use crate::state::{Backend, State};

#[derive(Debug, Clone, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum KeyboardFocusTarget {
    Window(Window),
    LayerSurface(LayerSurface),
    Popup(PopupKind),
}

impl IsAlive for KeyboardFocusTarget {
    #[inline]
    fn alive(&self) -> bool {
        match self {
            KeyboardFocusTarget::Window(w) => w.alive(),
            KeyboardFocusTarget::LayerSurface(l) => l.alive(),
            KeyboardFocusTarget::Popup(p) => p.alive(),
        }
    }
}

impl KeyboardFocusTarget {
    fn inner_keyboard_target<BackendData: Backend>(
        &self,
    ) -> &dyn KeyboardTarget<State<BackendData>> {
        match self {
            Self::Window(w) => match w.underlying_surface() {
                WindowSurface::Wayland(w) => w.wl_surface(),
            },
            Self::LayerSurface(l) => l.wl_surface(),
            Self::Popup(p) => p.wl_surface(),
        }
    }
}

impl<BackendData: Backend> KeyboardTarget<State<BackendData>> for KeyboardFocusTarget {
    fn enter(
        &self,
        seat: &Seat<State<BackendData>>,
        data: &mut State<BackendData>,
        keys: Vec<KeysymHandle<'_>>,
        serial: Serial,
    ) {
        self.inner_keyboard_target().enter(seat, data, keys, serial)
    }
    fn leave(
        &self,
        seat: &Seat<State<BackendData>>,
        data: &mut State<BackendData>,
        serial: Serial,
    ) {
        self.inner_keyboard_target().leave(seat, data, serial)
    }
    fn key(
        &self,
        seat: &Seat<State<BackendData>>,
        data: &mut State<BackendData>,
        key: KeysymHandle<'_>,
        state: KeyState,
        serial: Serial,
        time: u32,
    ) {
        self.inner_keyboard_target()
            .key(seat, data, key, state, serial, time)
    }
    fn modifiers(
        &self,
        seat: &Seat<State<BackendData>>,
        data: &mut State<BackendData>,
        modifiers: ModifiersState,
        serial: Serial,
    ) {
        self.inner_keyboard_target()
            .modifiers(seat, data, modifiers, serial)
    }
}

impl WaylandFocus for KeyboardFocusTarget {
    #[inline]
    fn wl_surface(&self) -> Option<Cow<'_, WlSurface>> {
        match self {
            KeyboardFocusTarget::Window(w) => w.wl_surface(),
            KeyboardFocusTarget::LayerSurface(l) => Some(Cow::Borrowed(l.wl_surface())),
            KeyboardFocusTarget::Popup(p) => Some(Cow::Borrowed(p.wl_surface())),
        }
    }
}

impl From<Window> for KeyboardFocusTarget {
    #[inline]
    fn from(w: Window) -> Self {
        KeyboardFocusTarget::Window(w)
    }
}

impl From<LayerSurface> for KeyboardFocusTarget {
    #[inline]
    fn from(l: LayerSurface) -> Self {
        KeyboardFocusTarget::LayerSurface(l)
    }
}

impl From<PopupKind> for KeyboardFocusTarget {
    #[inline]
    fn from(p: PopupKind) -> Self {
        KeyboardFocusTarget::Popup(p)
    }
}

impl From<KeyboardFocusTarget> for WlSurface {
    #[inline]
    fn from(value: KeyboardFocusTarget) -> Self {
        match value {
            KeyboardFocusTarget::Window(w) => match w.underlying_surface() {
                WindowSurface::Wayland(w) => w.wl_surface().clone(),
            },
            KeyboardFocusTarget::LayerSurface(surface) => surface.wl_surface().clone(),
            KeyboardFocusTarget::Popup(popup) => popup.wl_surface().clone(),
        }
    }
}
