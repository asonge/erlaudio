-module(erlaudio).

-include_lib("erlaudio/include/erlaudio.hrl").

-export([default_input_device/0, default_output_device/0, device/1, devices/0]).
-export([open_stream/5, open_stream_link/5]).
-export([pa_version/0]).

-spec default_input_device() -> #erlaudio_device{}.
default_input_device() ->
  erlaudio_drv:get_device(erlaudio_drv:get_default_input_device_index()).

-spec default_output_device() -> #erlaudio_device{}.
default_output_device() ->
  erlaudio_drv:get_device(erlaudio_drv:get_default_output_device_index()).

-spec device(Index :: integer()) -> #erlaudio_device{} | {error, nodevice}.
device(Index) when Index >= 0 ->
  DeviceCount = erlaudio_drv:get_device_count(),
  case Index of
    Index when Index < DeviceCount ->
      erlaudio_drv:get_device(Index);
    _ ->
      {error, nodevice}
  end.

-spec devices() -> [#erlaudio_device{}].
devices() ->
  [ erlaudio_drv:get_device(Idx) || Idx <- lists:seq(0, erlaudio_drv:get_device_count() - 1) ].

-spec open_stream(
    Input :: #erlaudio_device_params{} | null | undefined,
    Output :: #erlaudio_device_params{} | null | undefined,
    SampleRate :: float(),
    FramesPerBuffer :: integer(),
    Options :: [erlaudio_drv:device_options()]
) -> {ok, pid()}.
open_stream(Input, Output, SampleRate, FramesPerBuffer, Options) ->
  Flags = erlaudio_drv:stream_opt_to_integer(Options),
  erlaudio_srv:start(Input, Output, SampleRate, FramesPerBuffer, Flags).

-spec open_stream_link(
    Input :: #erlaudio_device_params{} | null | undefined,
    Output :: #erlaudio_device_params{} | null | undefined,
    SampleRate :: float(),
    FramesPerBuffer :: integer(),
    Options :: [erlaudio_drv:device_option()]
) -> {ok, pid()}.
open_stream_link(Input, Output, SampleRate, FramesPerBuffer, Options) ->
  Flags = erlaudio_drv:stream_opt_to_integer(Options),
  erlaudio_srv:start_link(Input, Output, SampleRate, FramesPerBuffer, Flags).

-spec pa_version() -> {integer(), binary()}.
pa_version() ->
  erlaudio_drv:get_pa_version().
