Erlaudio
========

Still very experimental erlang nif bindings for portaudio. Currently under construction, only (maybe) functional in erlaudio_drv module. Convenience modules not yet finished.

Notable modules:
 *  erlaudio - the main interface, "supported" interface
 *  erlaudio_drv - the driver interface, not directly supported, but there for hackery needs. binds to the nif
 *  erlaudio_srv - the gen_server for interacting with a stream, gets rid of some ugliness of erlaudio_drv
