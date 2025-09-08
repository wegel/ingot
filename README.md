# Ingot

A minimal, efficient Wayland compositor based on Smithay, focused on damage-driven rendering with DRM/KMS backend only.

## Acknowledgments

Ingot is based on [Anvil](https://github.com/Smithay/smithay/tree/master/anvil), the reference compositor for the Smithay project, originally authored by:
- [Victor Berger](mailto:victor.berger@m4x.org)
- [Drakulix (Victoria Brekenfeld)](https://github.com/drakulix)

Thanks to them and all Smithay contributors for creating the excellent foundation that makes Ingot possible.

## Dependencies

You'll need to install the following dependencies (note, that those package names may vary depending on your OS and linux distribution):

- `libwayland`
- `libxkbcommon`
- `libudev`
- `libinput`
- `libgbm`
- [`libseat`](https://git.sr.ht/~kennylevinsen/seatd)

## Build and run

You can run it with cargo after having cloned this repository:

```
cargo run
```

### Supported Environment Variables

| Variable                      | Example         | Description                     |
|-------------------------------|-----------------|----------------------------------|
| INGOT_DRM_DEVICE              | /dev/dri/card0  | Specify DRM device              |
| INGOT_DISABLE_10BIT           | any             | Disable 10-bit color support    |
| INGOT_DISABLE_DIRECT_SCANOUT  | any             | Disable direct scanout          |
| INGOT_GLES_DISABLE_INSTANCING | any             | Disable GLES instancing         |
| INGOT_<output_name>           | scale=1.5,rotation=90 | Configure output scale and rotation |
| INGOT_TERMINAL                | alacritty       | Terminal to launch with Super+Enter (default: foot) |
