// Allow in this module because of existing usage
#![allow(clippy::uninlined_format_args)]
use std::{
    collections::hash_map::HashMap,
    io,
    ops::Not,
    path::Path,
    sync::{atomic::Ordering, Mutex, Once},
    time::{Duration, Instant},
};

use crate::state::{DndIcon, SurfaceDmabufFeedback};
use crate::{
    render::*,
    state::{take_presentation_feedback, update_primary_scanout_output, Backend, State},
};
#[cfg(feature = "renderer_sync")]
use smithay::backend::drm::compositor::PrimaryPlaneElement;
#[cfg(feature = "egl")]
use smithay::backend::renderer::ImportEgl;
use smithay::{
    backend::renderer::element::AsRenderElements,
    backend::{
        allocator::{
            dmabuf::Dmabuf,
            format::FormatSet,
            gbm::{GbmAllocator, GbmBufferFlags, GbmDevice},
            Fourcc, Modifier,
        },
        drm::{
            compositor::{DrmCompositor, FrameFlags},
            exporter::gbm::GbmFramebufferExporter,
            output::{DrmOutput, DrmOutputManager, DrmOutputRenderElements},
            CreateDrmNodeError, DrmAccessError, DrmDevice, DrmDeviceFd, DrmError, DrmEvent,
            DrmEventMetadata, DrmEventTime, DrmNode, DrmSurface, GbmBufferedSurface, NodeType,
        },
        egl::{self, context::ContextPriority, EGLContext, EGLDevice, EGLDisplay},
        input::InputEvent,
        libinput::{LibinputInputBackend, LibinputSessionInterface},
        renderer::{
            damage::Error as OutputDamageTrackerError,
            element::{memory::MemoryRenderBuffer, RenderElementStates},
            gles::{Capability, GlesRenderer},
            multigpu::{gbm::GbmGlesBackend, GpuManager, MultiRenderer},
            ImportDma, ImportMemWl,
        },
        session::{
            libseat::{self, LibSeatSession},
            Event as SessionEvent, Session,
        },
        udev::{all_gpus, primary_gpu, UdevBackend, UdevEvent},
        SwapBuffersError,
    },
    delegate_dmabuf,
    desktop::Window,
    desktop::{
        space::{Space, SurfaceTree},
        utils::OutputPresentationFeedback,
    },
    input::{
        keyboard::LedState,
        pointer::{CursorImageAttributes, CursorImageStatus},
    },
    output::{Mode as WlMode, Output, PhysicalProperties, Scale},
    reexports::{
        calloop::{
            timer::{TimeoutAction, Timer},
            EventLoop, RegistrationToken,
        },
        drm::{
            control::{connector, crtc, ModeTypeFlags},
            Device as _,
        },
        input::{DeviceCapability, Libinput},
        rustix::fs::OFlags,
        wayland_protocols::wp::{
            linux_dmabuf::zv1::server::zwp_linux_dmabuf_feedback_v1,
            presentation_time::server::wp_presentation_feedback,
        },
        wayland_server::{backend::GlobalId, protocol::wl_surface, Display, DisplayHandle},
    },
    utils::Transform,
    utils::{DeviceFd, IsAlive, Logical, Monotonic, Point, Time},
    wayland::{
        compositor,
        dmabuf::{DmabufFeedbackBuilder, DmabufGlobal, DmabufHandler, DmabufState, ImportNotifier},
        drm_syncobj::{supports_syncobj_eventfd, DrmSyncobjHandler, DrmSyncobjState},
        presentation::Refresh,
    },
};
use smithay_drm_extras::{
    display_info,
    drm_scanner::{DrmScanEvent, DrmScanner},
};
use tracing::{debug, error, info, trace, warn};

// we cannot simply pick the first supported format of the intersection of *all* formats, because:
// - we do not want something like Abgr4444, which looses color information, if something better is available
// - some formats might perform terribly
// - we might need some work-arounds, if one supports modifiers, but the other does not
//
// So lets just pick `ARGB2101010` (10-bit) or `ARGB8888` (8-bit) for now, they are widely supported.
const SUPPORTED_FORMATS: &[Fourcc] = &[
    Fourcc::Abgr2101010,
    Fourcc::Argb2101010,
    Fourcc::Abgr8888,
    Fourcc::Argb8888,
];
const SUPPORTED_FORMATS_8BIT_ONLY: &[Fourcc] = &[Fourcc::Abgr8888, Fourcc::Argb8888];

/// Parse output configuration from environment variables
/// Format: INGOT_<output_name>="scale=<scale>,rotation=<rotation>"
/// Example: INGOT_HDMI_A_1="scale=1.6,rotation=270"
fn parse_output_config(output_name: &str) -> (Option<Scale>, Transform) {
    // Replace hyphens with underscores for environment variable name
    let env_name = format!("INGOT_{}", output_name.replace('-', "_"));

    let mut scale = None;
    let mut transform = Transform::Normal;

    if let Ok(config) = std::env::var(&env_name) {
        info!("Found configuration for {}: {}", output_name, config);

        for part in config.split(',') {
            let part = part.trim();
            if let Some(scale_str) = part.strip_prefix("scale=") {
                if let Ok(scale_value) = scale_str.parse::<f64>() {
                    if scale_value > 0.0 {
                        scale = Some(Scale::Fractional(scale_value));
                        info!("Setting scale {} for output {}", scale_value, output_name);
                    }
                }
            } else if let Some(rotation_str) = part.strip_prefix("rotation=") {
                transform = match rotation_str {
                    "0" | "normal" => Transform::Normal,
                    "90" => Transform::_90,
                    "180" => Transform::_180,
                    "270" => Transform::_270,
                    "flipped" => Transform::Flipped,
                    "flipped-90" => Transform::Flipped90,
                    "flipped-180" => Transform::Flipped180,
                    "flipped-270" => Transform::Flipped270,
                    _ => {
                        warn!("Unknown rotation value: {}", rotation_str);
                        Transform::Normal
                    }
                };
                info!(
                    "Setting rotation {:?} for output {}",
                    transform, output_name
                );
            }
        }
    }

    (scale, transform)
}

type UdevRenderer<'a> = MultiRenderer<
    'a,
    'a,
    GbmGlesBackend<GlesRenderer, DrmDeviceFd>,
    GbmGlesBackend<GlesRenderer, DrmDeviceFd>,
>;

#[derive(Debug, PartialEq)]
pub struct UdevOutputId {
    pub device_id: DrmNode,
    pub crtc: crtc::Handle,
}

pub struct UdevData {
    pub session: LibSeatSession,
    display_handle: DisplayHandle,
    dmabuf_state: Option<(DmabufState, DmabufGlobal)>,
    syncobj_state: Option<DrmSyncobjState>,
    primary_gpu: DrmNode,
    gpus: GpuManager<GbmGlesBackend<GlesRenderer, DrmDeviceFd>>,
    backends: HashMap<DrmNode, DrmBackendData>,
    pointer_images: Vec<(xcursor::parser::Image, MemoryRenderBuffer)>,
    pointer_element: PointerElement,
    pointer_image: crate::cursor::Cursor,
    keyboards: Vec<smithay::reexports::input::Device>,
}

impl UdevData {}

impl DmabufHandler for State<UdevData> {
    fn dmabuf_state(&mut self) -> &mut DmabufState {
        &mut self.backend_data.dmabuf_state.as_mut().unwrap().0
    }

    fn dmabuf_imported(
        &mut self,
        _global: &DmabufGlobal,
        dmabuf: Dmabuf,
        notifier: ImportNotifier,
    ) {
        if self
            .backend_data
            .gpus
            .single_renderer(&self.backend_data.primary_gpu)
            .and_then(|mut renderer| renderer.import_dmabuf(&dmabuf, None))
            .is_ok()
        {
            dmabuf.set_node(self.backend_data.primary_gpu);
            let _ = notifier.successful::<State<UdevData>>();
        } else {
            notifier.failed();
        }
    }
}
delegate_dmabuf!(State<UdevData>);

impl Backend for UdevData {
    const HAS_RELATIVE_MOTION: bool = true;
    const HAS_GESTURES: bool = true;

    fn seat_name(&self) -> String {
        self.session.seat()
    }

    fn reset_buffers(&mut self, output: &Output) {
        if let Some(id) = output.user_data().get::<UdevOutputId>() {
            if let Some(gpu) = self.backends.get_mut(&id.device_id) {
                if let Some(surface) = gpu.surfaces.get_mut(&id.crtc) {
                    surface.drm_output.reset_buffers();
                }
            }
        }
    }

    fn early_import(&mut self, surface: &wl_surface::WlSurface) {
        if let Err(err) = self.gpus.early_import(self.primary_gpu, surface) {
            warn!("Early buffer import failed: {}", err);
        }
    }

    fn update_led_state(&mut self, led_state: LedState) {
        for keyboard in self.keyboards.iter_mut() {
            keyboard.led_update(led_state.into());
        }
    }

    fn prepare_render(
        &mut self,
        output: &Output,
    ) -> Option<Box<dyn FnOnce(&mut State<Self>) + 'static>> {
        if let Some(output_id) = output.user_data().get::<UdevOutputId>() {
            // Mark as needing render
            if let Some(device) = self.backends.get_mut(&output_id.device_id) {
                if let Some(surface) = device.surfaces.get_mut(&output_id.crtc) {
                    surface.render_needed = true;
                }
            }

            // Return callback to schedule the actual render
            let device_id = output_id.device_id;
            let crtc = output_id.crtc;
            Some(Box::new(move |state| {
                state.render(device_id, Some(crtc), state.clock.now());
            }))
        } else {
            None
        }
    }
}

pub fn run_udev() {
    let mut event_loop = EventLoop::try_new().unwrap();
    let display = Display::new().unwrap();
    let mut display_handle = display.handle();

    let (session, notifier) = match LibSeatSession::new() {
        Ok(ret) => ret,
        Err(err) => {
            error!("Could not initialize a session: {}", err);
            return;
        }
    };

    let primary_gpu = if let Ok(var) = std::env::var("INGOT_DRM_DEVICE") {
        DrmNode::from_path(var).expect("Invalid drm device path")
    } else {
        primary_gpu(session.seat())
            .unwrap()
            .and_then(|x| {
                DrmNode::from_path(x)
                    .ok()?
                    .node_with_type(NodeType::Render)?
                    .ok()
            })
            .unwrap_or_else(|| {
                all_gpus(session.seat())
                    .unwrap()
                    .into_iter()
                    .find_map(|x| DrmNode::from_path(x).ok())
                    .expect("No GPU!")
            })
    };
    info!("Using {} as primary gpu.", primary_gpu);

    let gpus = GpuManager::new(GbmGlesBackend::with_factory(|display| {
        let context = EGLContext::new_with_priority(display, ContextPriority::High)?;
        let mut capabilities = unsafe { GlesRenderer::supported_capabilities(&context)? };
        if std::env::var("INGOT_GLES_DISABLE_INSTANCING").is_ok() {
            capabilities.retain(|capability| *capability != Capability::Instancing);
        }
        Ok(unsafe { GlesRenderer::with_capabilities(context, capabilities)? })
    }))
    .unwrap();

    let data = UdevData {
        display_handle: display_handle.clone(),
        dmabuf_state: None,
        syncobj_state: None,
        session,
        primary_gpu,
        gpus,
        backends: HashMap::new(),
        pointer_image: crate::cursor::Cursor::load(),
        pointer_images: Vec::new(),
        pointer_element: PointerElement::default(),
        keyboards: Vec::new(),
    };
    let mut state = State::init(display, event_loop.handle(), data, true);

    let udev_backend = match UdevBackend::new(&state.seat_name) {
        Ok(ret) => ret,
        Err(err) => {
            error!(error = ?err, "Failed to initialize udev backend");
            return;
        }
    };

    let mut libinput_context = Libinput::new_with_udev::<LibinputSessionInterface<LibSeatSession>>(
        state.backend_data.session.clone().into(),
    );
    libinput_context.udev_assign_seat(&state.seat_name).unwrap();
    let libinput_backend = LibinputInputBackend::new(libinput_context.clone());

    event_loop
        .handle()
        .insert_source(libinput_backend, move |mut event, _, data| {
            let display_handle = data.backend_data.display_handle.clone();
            if let InputEvent::DeviceAdded { device } = &mut event {
                if device.has_capability(DeviceCapability::Keyboard) {
                    if let Some(led_state) = data
                        .seat
                        .get_keyboard()
                        .map(|keyboard| keyboard.led_state())
                    {
                        device.led_update(led_state.into());
                    }
                    data.backend_data.keyboards.push(device.clone());
                }
            } else if let InputEvent::DeviceRemoved { ref device } = event {
                if device.has_capability(DeviceCapability::Keyboard) {
                    data.backend_data.keyboards.retain(|item| item != device);
                }
            }

            data.process_input_event(&display_handle, event)
        })
        .unwrap();

    event_loop
        .handle()
        .insert_source(notifier, move |event, &mut (), data| match event {
            SessionEvent::PauseSession => {
                libinput_context.suspend();
                info!("pausing session");

                for backend in data.backend_data.backends.values_mut() {
                    backend.drm_output_manager.pause();
                }
            }
            SessionEvent::ActivateSession => {
                info!("resuming session");

                if let Err(err) = libinput_context.resume() {
                    error!("Failed to resume libinput context: {:?}", err);
                }
                for (node, backend) in data
                    .backend_data
                    .backends
                    .iter_mut()
                    .map(|(handle, backend)| (*handle, backend))
                {
                    // if we do not care about flicking (caused by modesetting) we could just
                    // pass true for disable connectors here. this would make sure our drm
                    // device is in a known state (all connectors and planes disabled).
                    // but for demonstration we choose a more optimistic path by leaving the
                    // state as is and assume it will just work. If this assumption fails
                    // we will try to reset the state when trying to queue a frame.
                    backend
                        .drm_output_manager
                        .lock()
                        .activate(false)
                        .expect("failed to activate drm backend");
                    // mark all surfaces as needing render after session resume
                    for surface in backend.surfaces.values_mut() {
                        surface.render_needed = true;
                    }
                    data.handle
                        .insert_idle(move |data| data.render(node, None, data.clock.now()));
                }
            }
        })
        .unwrap();

    // We try to initialize the primary node before others to make sure
    // any display only node can fall back to the primary node for rendering
    let primary_node = primary_gpu
        .node_with_type(NodeType::Primary)
        .and_then(|node| node.ok());
    let primary_device = udev_backend.device_list().find(|(device_id, _)| {
        primary_node
            .map(|primary_node| *device_id == primary_node.dev_id())
            .unwrap_or(false)
            || *device_id == primary_gpu.dev_id()
    });

    if let Some((device_id, path)) = primary_device {
        let node = DrmNode::from_dev_id(device_id).expect("failed to get primary node");
        state
            .device_added(node, path)
            .expect("failed to initialize primary node");
    }

    let primary_device_id = primary_device.map(|(device_id, _)| device_id);
    for (device_id, path) in udev_backend.device_list() {
        if Some(device_id) == primary_device_id {
            continue;
        }

        if let Err(err) = DrmNode::from_dev_id(device_id)
            .map_err(DeviceAddError::DrmNode)
            .and_then(|node| state.device_added(node, path))
        {
            error!("Skipping device {device_id}: {err}");
        }
    }
    state.shm_state.update_formats(
        state
            .backend_data
            .gpus
            .single_renderer(&primary_gpu)
            .unwrap()
            .shm_formats(),
    );

    #[cfg_attr(not(feature = "egl"), allow(unused_mut))]
    let mut renderer = state
        .backend_data
        .gpus
        .single_renderer(&primary_gpu)
        .unwrap();

    // FPS texture loading removed - using log-based FPS reporting instead

    #[cfg(feature = "egl")]
    {
        info!(
            ?primary_gpu,
            "Trying to initialize EGL Hardware Acceleration",
        );
        match renderer.bind_wl_display(&display_handle) {
            Ok(_) => info!("EGL hardware-acceleration enabled"),
            Err(err) => info!(?err, "Failed to initialize EGL hardware-acceleration"),
        }
    }

    // init dmabuf support with format list from our primary gpu
    let dmabuf_formats = renderer.dmabuf_formats();
    let default_feedback = DmabufFeedbackBuilder::new(primary_gpu.dev_id(), dmabuf_formats)
        .build()
        .unwrap();
    let mut dmabuf_state = DmabufState::new();
    let global = dmabuf_state
        .create_global_with_default_feedback::<State<UdevData>>(&display_handle, &default_feedback);
    state.backend_data.dmabuf_state = Some((dmabuf_state, global));

    let gpus = &mut state.backend_data.gpus;
    state
        .backend_data
        .backends
        .iter_mut()
        .for_each(|(node, backend_data)| {
            // Update the per drm surface dmabuf feedback
            backend_data.surfaces.values_mut().for_each(|surface_data| {
                surface_data.dmabuf_feedback = surface_data.dmabuf_feedback.take().or_else(|| {
                    surface_data.drm_output.with_compositor(|compositor| {
                        get_surface_dmabuf_feedback(
                            primary_gpu,
                            surface_data.render_node,
                            *node,
                            gpus,
                            compositor.surface(),
                        )
                    })
                });
            });
        });

    // Expose syncobj protocol if supported by primary GPU
    if let Some(primary_node) = state
        .backend_data
        .primary_gpu
        .node_with_type(NodeType::Primary)
        .and_then(|x| x.ok())
    {
        if let Some(backend) = state.backend_data.backends.get(&primary_node) {
            let import_device = backend.drm_output_manager.device().device_fd().clone();
            if supports_syncobj_eventfd(&import_device) {
                let syncobj_state =
                    DrmSyncobjState::new::<State<UdevData>>(&display_handle, import_device);
                state.backend_data.syncobj_state = Some(syncobj_state);
            }
        }
    }

    event_loop
        .handle()
        .insert_source(udev_backend, move |event, _, data| match event {
            UdevEvent::Added { device_id, path } => {
                if let Err(err) = DrmNode::from_dev_id(device_id)
                    .map_err(DeviceAddError::DrmNode)
                    .and_then(|node| data.device_added(node, &path))
                {
                    error!("Skipping device {device_id}: {err}");
                }
            }
            UdevEvent::Changed { device_id } => {
                if let Ok(node) = DrmNode::from_dev_id(device_id) {
                    data.device_changed(node)
                }
            }
            UdevEvent::Removed { device_id } => {
                if let Ok(node) = DrmNode::from_dev_id(device_id) {
                    data.device_removed(node)
                }
            }
        })
        .unwrap();

    while state.running.load(Ordering::SeqCst) {
        let result = event_loop.dispatch(Some(Duration::from_millis(16)), &mut state);
        if result.is_err() {
            state.running.store(false, Ordering::SeqCst);
        } else {
            state.space.refresh();
            state.popups.cleanup();
            display_handle.flush_clients().unwrap();
        }
    }
}

impl DrmSyncobjHandler for State<UdevData> {
    fn drm_syncobj_state(&mut self) -> Option<&mut DrmSyncobjState> {
        self.backend_data.syncobj_state.as_mut()
    }
}
smithay::delegate_drm_syncobj!(State<UdevData>);

pub type RenderSurface =
    GbmBufferedSurface<GbmAllocator<DrmDeviceFd>, Option<OutputPresentationFeedback>>;

pub type GbmDrmCompositor = DrmCompositor<
    GbmAllocator<DrmDeviceFd>,
    GbmDevice<DrmDeviceFd>,
    Option<OutputPresentationFeedback>,
    DrmDeviceFd,
>;

struct DrmSurfaceData {
    display_handle: DisplayHandle,
    device_id: DrmNode,
    render_node: Option<DrmNode>,
    output: Output,
    global: Option<GlobalId>,
    drm_output: DrmOutput<
        GbmAllocator<DrmDeviceFd>,
        GbmFramebufferExporter<DrmDeviceFd>,
        Option<OutputPresentationFeedback>,
        DrmDeviceFd,
    >,
    disable_direct_scanout: bool,
    #[cfg(feature = "debug")]
    fps: fps_ticker::Fps,
    #[cfg(feature = "debug")]
    last_fps_log: Instant,
    dmabuf_feedback: Option<SurfaceDmabufFeedback>,
    last_presentation_time: Option<Time<Monotonic>>,
    vblank_throttle_timer: Option<RegistrationToken>,
    render_needed: bool,
}

impl Drop for DrmSurfaceData {
    fn drop(&mut self) {
        self.output.leave_all();
        if let Some(global) = self.global.take() {
            self.display_handle.remove_global::<State<UdevData>>(global);
        }
    }
}

struct DrmBackendData {
    surfaces: HashMap<crtc::Handle, DrmSurfaceData>,
    drm_output_manager: DrmOutputManager<
        GbmAllocator<DrmDeviceFd>,
        GbmFramebufferExporter<DrmDeviceFd>,
        Option<OutputPresentationFeedback>,
        DrmDeviceFd,
    >,
    drm_scanner: DrmScanner,
    render_node: Option<DrmNode>,
    registration_token: RegistrationToken,
}

#[derive(Debug, thiserror::Error)]
enum DeviceAddError {
    #[error("Failed to open device using libseat: {0}")]
    DeviceOpen(libseat::Error),
    #[error("Failed to initialize drm device: {0}")]
    DrmDevice(DrmError),
    #[error("Failed to initialize gbm device: {0}")]
    GbmDevice(std::io::Error),
    #[error("Failed to access drm node: {0}")]
    DrmNode(CreateDrmNodeError),
    #[error("Failed to add device to GpuManager: {0}")]
    AddNode(egl::Error),
    #[error("The device has no render node")]
    NoRenderNode,
    #[error("Primary GPU is missing")]
    PrimaryGpuMissing,
}

fn get_surface_dmabuf_feedback(
    primary_gpu: DrmNode,
    render_node: Option<DrmNode>,
    scanout_node: DrmNode,
    gpus: &mut GpuManager<GbmGlesBackend<GlesRenderer, DrmDeviceFd>>,
    surface: &DrmSurface,
) -> Option<SurfaceDmabufFeedback> {
    let primary_formats = gpus.single_renderer(&primary_gpu).ok()?.dmabuf_formats();
    let render_formats = if let Some(render_node) = render_node {
        gpus.single_renderer(&render_node).ok()?.dmabuf_formats()
    } else {
        FormatSet::default()
    };

    let all_render_formats = primary_formats
        .iter()
        .chain(render_formats.iter())
        .copied()
        .collect::<FormatSet>();

    let planes = surface.planes().clone();

    // We limit the scan-out tranche to formats we can also render from
    // so that there is always a fallback render path available in case
    // the supplied buffer can not be scanned out directly
    let planes_formats = surface
        .plane_info()
        .formats
        .iter()
        .copied()
        .chain(planes.overlay.into_iter().flat_map(|p| p.formats))
        .collect::<FormatSet>()
        .intersection(&all_render_formats)
        .copied()
        .collect::<FormatSet>();

    let builder = DmabufFeedbackBuilder::new(primary_gpu.dev_id(), primary_formats);
    let render_feedback = if let Some(render_node) = render_node {
        builder
            .clone()
            .add_preference_tranche(render_node.dev_id(), None, render_formats.clone())
            .build()
            .unwrap()
    } else {
        builder.clone().build().unwrap()
    };

    let scanout_feedback = builder
        .add_preference_tranche(
            surface.device_fd().dev_id().unwrap(),
            Some(zwp_linux_dmabuf_feedback_v1::TrancheFlags::Scanout),
            planes_formats,
        )
        .add_preference_tranche(scanout_node.dev_id(), None, render_formats)
        .build()
        .unwrap();

    Some(SurfaceDmabufFeedback {
        render_feedback,
        scanout_feedback,
    })
}

// Helper to handle SwapBuffersError consistently
fn handle_swap_buffers_error<F>(err: SwapBuffersError, mut reset_device: F) -> bool
where
    F: FnMut(),
{
    warn!("Error during rendering: {:?}", err);
    match err {
        SwapBuffersError::AlreadySwapped => false,
        SwapBuffersError::TemporaryFailure(err) => match err.downcast_ref::<DrmError>() {
            Some(DrmError::DeviceInactive) => true,
            Some(DrmError::Access(DrmAccessError { source, .. })) => {
                source.kind() == io::ErrorKind::PermissionDenied
            }
            _ => false,
        },
        SwapBuffersError::ContextLost(err) => match err.downcast_ref::<DrmError>() {
            Some(DrmError::TestFailed(_)) => {
                reset_device();
                true
            }
            _ => panic!("Rendering loop lost: {err}"),
        },
    }
}

impl State<UdevData> {
    pub fn schedule_render_for_output(&mut self, output: &Output) {
        if let Some(output_id) = output.user_data().get::<UdevOutputId>() {
            // mark as needing render
            if let Some(device) = self.backend_data.backends.get_mut(&output_id.device_id) {
                if let Some(surface) = device.surfaces.get_mut(&output_id.crtc) {
                    surface.render_needed = true;
                }
            }

            // schedule the actual render
            let device_id = output_id.device_id;
            let crtc = output_id.crtc;
            self.handle.insert_idle(move |state| {
                state.render(device_id, Some(crtc), state.clock.now());
            });
        }
    }

    fn device_added(&mut self, node: DrmNode, path: &Path) -> Result<(), DeviceAddError> {
        // Try to open the device
        let fd = self
            .backend_data
            .session
            .open(
                path,
                OFlags::RDWR | OFlags::CLOEXEC | OFlags::NOCTTY | OFlags::NONBLOCK,
            )
            .map_err(DeviceAddError::DeviceOpen)?;

        let fd = DrmDeviceFd::new(DeviceFd::from(fd));

        let (drm, notifier) =
            DrmDevice::new(fd.clone(), true).map_err(DeviceAddError::DrmDevice)?;
        let gbm = GbmDevice::new(fd).map_err(DeviceAddError::GbmDevice)?;

        let registration_token = self
            .handle
            .insert_source(
                notifier,
                move |event, metadata, data: &mut State<_>| match event {
                    DrmEvent::VBlank(crtc) => {
                        profiling::scope!("vblank", &format!("{crtc:?}"));
                        data.frame_finish(node, crtc, metadata);
                    }
                    DrmEvent::Error(error) => {
                        error!("{:?}", error);
                    }
                },
            )
            .unwrap();

        let mut try_initialize_gpu = || {
            let display = unsafe { EGLDisplay::new(gbm.clone()).map_err(DeviceAddError::AddNode)? };
            let egl_device =
                EGLDevice::device_for_display(&display).map_err(DeviceAddError::AddNode)?;

            if egl_device.is_software() {
                return Err(DeviceAddError::NoRenderNode);
            }

            let render_node = egl_device
                .try_get_render_node()
                .ok()
                .flatten()
                .unwrap_or(node);
            self.backend_data
                .gpus
                .as_mut()
                .add_node(render_node, gbm.clone())
                .map_err(DeviceAddError::AddNode)?;

            std::result::Result::<DrmNode, DeviceAddError>::Ok(render_node)
        };

        let render_node = try_initialize_gpu()
            .inspect_err(|err| {
                warn!(?err, "failed to initialize gpu");
            })
            .ok();

        let allocator = render_node
            .is_some()
            .then(|| {
                GbmAllocator::new(
                    gbm.clone(),
                    GbmBufferFlags::RENDERING | GbmBufferFlags::SCANOUT,
                )
            })
            .or_else(|| {
                self.backend_data
                    .backends
                    .get(&self.backend_data.primary_gpu)
                    .or_else(|| {
                        self.backend_data.backends.values().find(|backend| {
                            backend.render_node == Some(self.backend_data.primary_gpu)
                        })
                    })
                    .map(|backend| backend.drm_output_manager.allocator().clone())
            })
            .ok_or(DeviceAddError::PrimaryGpuMissing)?;

        let framebuffer_exporter = GbmFramebufferExporter::new(gbm.clone(), render_node.into());

        let color_formats = if std::env::var("INGOT_DISABLE_10BIT").is_ok() {
            SUPPORTED_FORMATS_8BIT_ONLY
        } else {
            SUPPORTED_FORMATS
        };
        let mut renderer = self
            .backend_data
            .gpus
            .single_renderer(&render_node.unwrap_or(self.backend_data.primary_gpu))
            .unwrap();
        let render_formats = renderer
            .as_mut()
            .egl_context()
            .dmabuf_render_formats()
            .iter()
            .filter(|format| render_node.is_some() || format.modifier == Modifier::Linear)
            .copied()
            .collect::<FormatSet>();

        let drm_output_manager = DrmOutputManager::new(
            drm,
            allocator,
            framebuffer_exporter,
            Some(gbm),
            color_formats.iter().copied(),
            render_formats,
        );

        self.backend_data.backends.insert(
            node,
            DrmBackendData {
                registration_token,
                drm_output_manager,
                drm_scanner: DrmScanner::new(),
                render_node,
                surfaces: HashMap::new(),
            },
        );

        self.device_changed(node);

        Ok(())
    }

    fn connector_connected(
        &mut self,
        node: DrmNode,
        connector: connector::Info,
        crtc: crtc::Handle,
    ) {
        let device = if let Some(device) = self.backend_data.backends.get_mut(&node) {
            device
        } else {
            return;
        };

        let render_node = device.render_node.unwrap_or(self.backend_data.primary_gpu);
        let mut renderer = self
            .backend_data
            .gpus
            .single_renderer(&render_node)
            .unwrap();

        let output_name = format!(
            "{}-{}",
            connector.interface().as_str(),
            connector.interface_id()
        );
        info!(?crtc, "Trying to setup connector {}", output_name,);

        let drm_device = device.drm_output_manager.device();

        let display_info = display_info::for_connector(drm_device, connector.handle());

        let make = display_info
            .as_ref()
            .and_then(|info| info.make())
            .unwrap_or_else(|| "Unknown".into());

        let model = display_info
            .as_ref()
            .and_then(|info| info.model())
            .unwrap_or_else(|| "Unknown".into());

        let serial_number = display_info
            .as_ref()
            .and_then(|info| info.serial())
            .unwrap_or_else(|| "Unknown".into());

        {
            let mode_id = connector
                .modes()
                .iter()
                .position(|mode| mode.mode_type().contains(ModeTypeFlags::PREFERRED))
                .unwrap_or(0);

            let drm_mode = connector.modes()[mode_id];
            let wl_mode = WlMode::from(drm_mode);

            let (phys_w, phys_h) = connector.size().unwrap_or((0, 0));
            let output = Output::new(
                output_name.clone(),
                PhysicalProperties {
                    size: (phys_w as i32, phys_h as i32).into(),
                    subpixel: connector.subpixel().into(),
                    make,
                    model,
                    serial_number,
                },
            );
            let global = output.create_global::<State<UdevData>>(&self.display_handle);

            let x_offset = self.space.outputs().fold(0, |acc, output| {
                acc + self.space.output_geometry(output).unwrap().size.w
            });
            let position = (x_offset, 0).into();

            // Parse output configuration from environment variables
            let (scale, transform) = parse_output_config(&output_name);

            output.set_preferred(wl_mode);
            output.change_current_state(Some(wl_mode), Some(transform), scale, Some(position));
            self.space.map_output(&output, position);

            output.user_data().insert_if_missing(|| UdevOutputId {
                crtc,
                device_id: node,
            });

            let driver = match drm_device.get_driver() {
                Ok(driver) => driver,
                Err(err) => {
                    warn!("Failed to query drm driver: {}", err);
                    return;
                }
            };

            let mut planes = match drm_device.planes(&crtc) {
                Ok(planes) => planes,
                Err(err) => {
                    warn!("Failed to query crtc planes: {}", err);
                    return;
                }
            };

            // Using an overlay plane on a nvidia card breaks
            if driver
                .name()
                .to_string_lossy()
                .to_lowercase()
                .contains("nvidia")
                || driver
                    .description()
                    .to_string_lossy()
                    .to_lowercase()
                    .contains("nvidia")
            {
                planes.overlay = vec![];
            }

            let drm_output = match device
                .drm_output_manager
                .lock()
                .initialize_output::<_, OutputRenderElements<
                    UdevRenderer<'_>,
                    <Window as AsRenderElements<UdevRenderer<'_>>>::RenderElement,
                >>(
                    crtc,
                    drm_mode,
                    &[connector.handle()],
                    &output,
                    Some(planes),
                    &mut renderer,
                    &DrmOutputRenderElements::default(),
                ) {
                Ok(drm_output) => drm_output,
                Err(err) => {
                    warn!("Failed to initialize drm output: {}", err);
                    return;
                }
            };

            let disable_direct_scanout = std::env::var("INGOT_DISABLE_DIRECT_SCANOUT").is_ok();

            let dmabuf_feedback = drm_output.with_compositor(|compositor| {
                get_surface_dmabuf_feedback(
                    self.backend_data.primary_gpu,
                    device.render_node,
                    node,
                    &mut self.backend_data.gpus,
                    compositor.surface(),
                )
            });

            let surface = DrmSurfaceData {
                display_handle: self.display_handle.clone(),
                device_id: node,
                render_node: device.render_node,
                output,
                global: Some(global),
                drm_output,
                disable_direct_scanout,
                #[cfg(feature = "debug")]
                fps: fps_ticker::Fps::default(),
                #[cfg(feature = "debug")]
                last_fps_log: Instant::now(),
                dmabuf_feedback,
                last_presentation_time: None,
                vblank_throttle_timer: None,
                render_needed: true, // Initial frame needed
            };

            device.surfaces.insert(crtc, surface);

            // kick-off rendering
            self.handle.insert_idle(move |state| {
                state.render_surface(node, crtc, state.clock.now());
            });
        }
    }

    fn connector_disconnected(
        &mut self,
        node: DrmNode,
        _connector: connector::Info,
        crtc: crtc::Handle,
    ) {
        let device = if let Some(device) = self.backend_data.backends.get_mut(&node) {
            device
        } else {
            return;
        };

        if let Some(surface) = device.surfaces.remove(&crtc) {
            self.space.unmap_output(&surface.output);
            self.space.refresh();
        }

        let render_node = device.render_node.unwrap_or(self.backend_data.primary_gpu);
        let mut renderer = self
            .backend_data
            .gpus
            .single_renderer(&render_node)
            .unwrap();
        let _ = device
            .drm_output_manager
            .lock()
            .try_to_restore_modifiers::<_, OutputRenderElements<
                UdevRenderer<'_>,
                <Window as AsRenderElements<UdevRenderer<'_>>>::RenderElement,
            >>(
                &mut renderer,
                // FIXME: For a flicker free operation we should return the actual elements for this output..
                // Instead we just use black to "simulate" a modeset :)
                &DrmOutputRenderElements::default(),
            );
    }

    fn device_changed(&mut self, node: DrmNode) {
        let device = if let Some(device) = self.backend_data.backends.get_mut(&node) {
            device
        } else {
            return;
        };

        let scan_result = match device
            .drm_scanner
            .scan_connectors(device.drm_output_manager.device())
        {
            Ok(scan_result) => scan_result,
            Err(err) => {
                tracing::warn!(?err, "Failed to scan connectors");
                return;
            }
        };

        for event in scan_result {
            match event {
                DrmScanEvent::Connected {
                    connector,
                    crtc: Some(crtc),
                } => {
                    self.connector_connected(node, connector, crtc);
                }
                DrmScanEvent::Disconnected {
                    connector,
                    crtc: Some(crtc),
                } => {
                    self.connector_disconnected(node, connector, crtc);
                }
                _ => {}
            }
        }

        // fixup window coordinates
        crate::shell::fixup_positions(&mut self.space, self.pointer.current_location());
    }

    fn device_removed(&mut self, node: DrmNode) {
        let device = if let Some(device) = self.backend_data.backends.get_mut(&node) {
            device
        } else {
            return;
        };

        let crtcs: Vec<_> = device
            .drm_scanner
            .crtcs()
            .map(|(info, crtc)| (info.clone(), crtc))
            .collect();

        for (connector, crtc) in crtcs {
            self.connector_disconnected(node, connector, crtc);
        }

        debug!("Surfaces dropped");

        // drop the backends on this side
        if let Some(backend_data) = self.backend_data.backends.remove(&node) {
            if let Some(render_node) = backend_data.render_node {
                self.backend_data.gpus.as_mut().remove_node(&render_node);
            }

            self.handle.remove(backend_data.registration_token);

            debug!("Dropping device");
        }

        crate::shell::fixup_positions(&mut self.space, self.pointer.current_location());
    }

    fn frame_finish(
        &mut self,
        dev_id: DrmNode,
        crtc: crtc::Handle,
        metadata: &mut Option<DrmEventMetadata>,
    ) {
        profiling::scope!("frame_finish", &format!("{crtc:?}"));

        let device_backend = match self.backend_data.backends.get_mut(&dev_id) {
            Some(backend) => backend,
            None => {
                error!("Trying to finish frame on non-existent backend {}", dev_id);
                return;
            }
        };

        let surface = match device_backend.surfaces.get_mut(&crtc) {
            Some(surface) => surface,
            None => {
                error!("Trying to finish frame on non-existent crtc {:?}", crtc);
                return;
            }
        };

        if let Some(timer_token) = surface.vblank_throttle_timer.take() {
            self.handle.remove(timer_token);
        }

        let output = if let Some(output) = self.space.outputs().find(|o| {
            o.user_data().get::<UdevOutputId>()
                == Some(&UdevOutputId {
                    device_id: surface.device_id,
                    crtc,
                })
        }) {
            output.clone()
        } else {
            // somehow we got called with an invalid output
            return;
        };

        let Some(frame_duration) = output
            .current_mode()
            .map(|mode| Duration::from_secs_f64(1_000f64 / mode.refresh as f64))
        else {
            return;
        };

        let tp = metadata.as_ref().and_then(|metadata| match metadata.time {
            smithay::backend::drm::DrmEventTime::Monotonic(tp) => tp.is_zero().not().then_some(tp),
            smithay::backend::drm::DrmEventTime::Realtime(_) => None,
        });

        let seq = metadata
            .as_ref()
            .map(|metadata| metadata.sequence)
            .unwrap_or(0);

        let (clock, flags) = if let Some(tp) = tp {
            (
                tp.into(),
                wp_presentation_feedback::Kind::Vsync
                    | wp_presentation_feedback::Kind::HwClock
                    | wp_presentation_feedback::Kind::HwCompletion,
            )
        } else {
            (self.clock.now(), wp_presentation_feedback::Kind::Vsync)
        };

        let vblank_remaining_time = surface
            .last_presentation_time
            .map(|last_presentation_time| {
                frame_duration.saturating_sub(Time::elapsed(&last_presentation_time, clock))
            });

        if let Some(vblank_remaining_time) = vblank_remaining_time {
            if vblank_remaining_time > frame_duration / 2 {
                static WARN_ONCE: Once = Once::new();
                WARN_ONCE.call_once(|| {
                    warn!("display running faster than expected, throttling vblanks and disabling HwClock")
                });
                let throttled_time = tp
                    .map(|tp| tp.saturating_add(vblank_remaining_time))
                    .unwrap_or(Duration::ZERO);
                let throttled_metadata = DrmEventMetadata {
                    sequence: seq,
                    time: DrmEventTime::Monotonic(throttled_time),
                };
                let timer_token = self
                    .handle
                    .insert_source(
                        Timer::from_duration(vblank_remaining_time),
                        move |_, _, data| {
                            data.frame_finish(dev_id, crtc, &mut Some(throttled_metadata));
                            TimeoutAction::Drop
                        },
                    )
                    .expect("failed to register vblank throttle timer");
                surface.vblank_throttle_timer = Some(timer_token);
                return;
            }
        }
        surface.last_presentation_time = Some(clock);

        let submit_result = surface
            .drm_output
            .frame_submitted()
            .map_err(Into::<SwapBuffersError>::into);

        match submit_result {
            Ok(user_data) => {
                if let Some(mut feedback) = user_data.flatten() {
                    feedback.presented(clock, Refresh::fixed(frame_duration), seq as u64, flags);
                }
            }
            Err(err) => {
                handle_swap_buffers_error(err, || {});
            }
        };

        // damage-driven rendering: don't automatically schedule next frame
        // frames will be triggered by surface commits instead
    }

    // If crtc is `Some()`, render it, else render all crtcs
    pub fn render(
        &mut self,
        node: DrmNode,
        crtc: Option<crtc::Handle>,
        frame_target: Time<Monotonic>,
    ) {
        let device_backend = match self.backend_data.backends.get_mut(&node) {
            Some(backend) => backend,
            None => {
                error!("Trying to render on non-existent backend {}", node);
                return;
            }
        };

        if let Some(crtc) = crtc {
            self.render_surface(node, crtc, frame_target);
        } else {
            let crtcs: Vec<_> = device_backend.surfaces.keys().copied().collect();
            for crtc in crtcs {
                self.render_surface(node, crtc, frame_target);
            }
        };
    }

    fn render_surface(&mut self, node: DrmNode, crtc: crtc::Handle, frame_target: Time<Monotonic>) {
        profiling::scope!("render_surface", &format!("{crtc:?}"));

        let output = if let Some(output) = self.space.outputs().find(|o| {
            o.user_data().get::<UdevOutputId>()
                == Some(&UdevOutputId {
                    device_id: node,
                    crtc,
                })
        }) {
            output.clone()
        } else {
            // somehow we got called with an invalid output
            return;
        };

        self.pre_repaint(&output, frame_target);

        let device = if let Some(device) = self.backend_data.backends.get_mut(&node) {
            device
        } else {
            return;
        };

        let surface = if let Some(surface) = device.surfaces.get_mut(&crtc) {
            surface
        } else {
            return;
        };

        // only render if needed
        if !surface.render_needed {
            return;
        }

        // clear the flag as we're about to render
        surface.render_needed = false;

        let start = Instant::now();

        // TODO get scale from the rendersurface when supporting HiDPI
        let frame = self
            .backend_data
            .pointer_image
            .get_image(1 /*scale*/, self.clock.now().into());

        let primary_gpu = self.backend_data.primary_gpu;
        let render_node = surface.render_node.unwrap_or(primary_gpu);
        let mut renderer = if primary_gpu == render_node {
            self.backend_data.gpus.single_renderer(&render_node)
        } else {
            let format = surface.drm_output.format();
            self.backend_data
                .gpus
                .renderer(&primary_gpu, &render_node, format)
        }
        .unwrap();

        let pointer_images = &mut self.backend_data.pointer_images;
        let pointer_image = pointer_images
            .iter()
            .find_map(|(image, texture)| {
                if image == &frame {
                    Some(texture.clone())
                } else {
                    None
                }
            })
            .unwrap_or_else(|| {
                let buffer = MemoryRenderBuffer::from_slice(
                    &frame.pixels_rgba,
                    Fourcc::Argb8888,
                    (frame.width as i32, frame.height as i32),
                    1,
                    Transform::Normal,
                    None,
                );
                pointer_images.push((frame, buffer.clone()));
                buffer
            });

        let result = render_surface(
            surface,
            &mut renderer,
            &self.space,
            &output,
            self.pointer.current_location(),
            &pointer_image,
            &mut self.backend_data.pointer_element,
            &self.dnd_icon,
            &mut self.cursor_status,
        );
        let reschedule = match result {
            Ok((has_rendered, states)) => {
                let dmabuf_feedback = surface.dmabuf_feedback.clone();
                self.post_repaint(&output, frame_target, dmabuf_feedback, &states);
                !has_rendered
            }
            Err(err) => handle_swap_buffers_error(err, || {
                device
                    .drm_output_manager
                    .device_mut()
                    .reset_state()
                    .expect("failed to reset drm device");
            }),
        };

        if reschedule {
            // damage-driven rendering: if no damage or temporary failure, don't reschedule
            // next render will be triggered by actual damage/surface commits
            trace!(
                "no damage or temporary failure on {:?}, waiting for damage",
                crtc
            );
            // mark as needing render again since we didn't actually render this time
            if let Some(device) = self.backend_data.backends.get_mut(&node) {
                if let Some(surface) = device.surfaces.get_mut(&crtc) {
                    surface.render_needed = true;
                }
            }
        } else {
            let elapsed = start.elapsed();
            tracing::trace!(?elapsed, "rendered surface");
        }

        profiling::finish_frame!();
    }
}

#[allow(clippy::too_many_arguments)]
#[profiling::function]
fn render_surface<'a>(
    surface: &'a mut DrmSurfaceData,
    renderer: &mut UdevRenderer<'a>,
    space: &Space<Window>,
    output: &Output,
    pointer_location: Point<f64, Logical>,
    pointer_image: &MemoryRenderBuffer,
    pointer_element: &mut PointerElement,
    dnd_icon: &Option<DndIcon>,
    cursor_status: &mut CursorImageStatus,
) -> Result<(bool, RenderElementStates), SwapBuffersError> {
    let output_geometry = space.output_geometry(output).unwrap();
    let scale = smithay::utils::Scale::from(output.current_scale().fractional_scale());

    let mut custom_elements: Vec<CustomRenderElements<_>> = Vec::new();

    if output_geometry.to_f64().contains(pointer_location) {
        let cursor_hotspot = if let CursorImageStatus::Surface(ref surface) = cursor_status {
            compositor::with_states(surface, |states| {
                states
                    .data_map
                    .get::<Mutex<CursorImageAttributes>>()
                    .unwrap()
                    .lock()
                    .unwrap()
                    .hotspot
            })
        } else {
            (0, 0).into()
        };
        let cursor_pos = pointer_location - output_geometry.loc.to_f64();

        // set cursor
        pointer_element.set_buffer(pointer_image.clone());

        // draw the cursor as relevant
        {
            // reset the cursor if the surface is no longer alive
            let mut reset = false;
            if let CursorImageStatus::Surface(ref surface) = *cursor_status {
                reset = !surface.alive();
            }
            if reset {
                *cursor_status = CursorImageStatus::default_named();
            }

            pointer_element.set_status(cursor_status.clone());
        }

        custom_elements.extend(
            pointer_element.render_elements(
                renderer,
                (cursor_pos - cursor_hotspot.to_f64())
                    .to_physical(scale)
                    .to_i32_round(),
                scale,
                1.0,
            ),
        );

        // draw the dnd icon if applicable
        {
            if let Some(icon) = dnd_icon.as_ref() {
                let dnd_icon_pos = (cursor_pos + icon.offset.to_f64())
                    .to_physical(scale)
                    .to_i32_round();
                if icon.surface.alive() {
                    custom_elements.extend(AsRenderElements::<UdevRenderer<'a>>::render_elements(
                        &SurfaceTree::from_surface(&icon.surface),
                        renderer,
                        dnd_icon_pos,
                        scale,
                        1.0,
                    ));
                }
            }
        }
    }

    #[cfg(feature = "debug")]
    {
        surface.fps.tick();
        // Log FPS at most once per second
        let now = Instant::now();
        if now.duration_since(surface.last_fps_log) >= Duration::from_secs(1) {
            let fps = surface.fps.avg().round() as u32;
            info!("FPS: {} (output: {})", fps, output.name());
            surface.last_fps_log = now;
        }
    }

    let (elements, clear_color) = output_elements(output, space, custom_elements, renderer);

    let frame_mode = if surface.disable_direct_scanout {
        FrameFlags::empty()
    } else {
        FrameFlags::DEFAULT
    };
    let (rendered, states) = surface
        .drm_output
        .render_frame(renderer, &elements, clear_color, frame_mode)
        .map(|render_frame_result| {
            #[cfg(feature = "renderer_sync")]
            if let PrimaryPlaneElement::Swapchain(element) = render_frame_result.primary_element {
                element.sync.wait();
            }
            (!render_frame_result.is_empty, render_frame_result.states)
        })
        .map_err(|err| match err {
            smithay::backend::drm::compositor::RenderFrameError::PrepareFrame(err) => {
                SwapBuffersError::from(err)
            }
            smithay::backend::drm::compositor::RenderFrameError::RenderFrame(
                OutputDamageTrackerError::Rendering(err),
            ) => SwapBuffersError::from(err),
            _ => unreachable!(),
        })?;

    update_primary_scanout_output(space, output, dnd_icon, cursor_status, &states);

    if rendered {
        let output_presentation_feedback = take_presentation_feedback(output, space, &states);
        surface
            .drm_output
            .queue_frame(Some(output_presentation_feedback))
            .map_err(Into::<SwapBuffersError>::into)?;
    }

    Ok((rendered, states))
}
