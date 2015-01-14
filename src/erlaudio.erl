-module(erlaudio).

-include_lib("erlaudio/include/erlaudio.hrl").

-export([default_input_device/0, default_output_device/0, device/1, devices/0]).
-export([default_input_params/1, default_output_params/1, input_device_params/2, output_device_params/2, device_params/4]).
-export([format_supported/3, open_stream/5, open_stream_link/5]).
-export([abort_stream/1, close_stream/1]).
-export([portaudio_version/0]).

-type pa_error() :: {error, atom()}.
-type stream_option() :: noclip | nodither | nodropinput.

%% @doc Gets you the default input device
-spec default_input_device() -> #erlaudio_device{}.
default_input_device() ->
  erlaudio_drv:get_device(erlaudio_drv:get_default_input_device_index()).

%% @doc Gets you the default output device
-spec default_output_device() -> #erlaudio_device{}.
default_output_device() ->
  erlaudio_drv:get_device(erlaudio_drv:get_default_output_device_index()).

%% @doc Gets you a device for some integer
%%
%% On some platforms, device offsets are static, on others they're not.
%% You can try to store a device's ID and see if the name remains constant,
%% Though it's recommended that you look at a device's actual name in devices()
-spec device(Index :: integer()) -> #erlaudio_device{} | {error, nodevice}.
device(Index) when Index >= 0 ->
  DeviceCount = erlaudio_drv:get_device_count(),
  case Index of
    Index when Index < DeviceCount ->
      erlaudio_drv:get_device(Index);
    _ ->
      {error, nodevice}
  end.

%% @doc Gets you a list of devices
-spec devices() -> [#erlaudio_device{}].
devices() ->
  [ erlaudio_drv:get_device(Idx) || Idx <- lists:seq(0, erlaudio_drv:get_device_count() - 1) ].


%% @doc Gets you device parameters for the default input device
-spec default_input_params(SampleFormat :: format()) -> #erlaudio_device_params{}.
default_input_params(SampleFormat) ->
  input_device_params(erlaudio_drv:get_default_input_device_index(), SampleFormat).

%% @doc Gets you device parameters for the default output device
-spec default_output_params(SampleFormat :: format()) -> #erlaudio_device_params{}.
default_output_params(SampleFormat) ->
  output_device_params(erlaudio_drv:get_default_output_device_index(), SampleFormat).

%% @doc Gets you reasonable defaults for an input device
-spec input_device_params(Device :: #erlaudio_device{} | integer(),
                          SampleFormat :: format()) -> #erlaudio_device_params{}.
input_device_params(Index, SampleFormat) when is_integer(Index) ->
  input_device_params(erlaudio_drv:get_device(Index), SampleFormat);
input_device_params(#erlaudio_device{index=Index} = Device, SampleFormat) ->
  device_params(Index, Device#erlaudio_device.max_input_channels,
                SampleFormat, Device#erlaudio_device.default_low_input_latency).

%% @doc Gets you reasonable defaults for an output device
-spec output_device_params(Device :: #erlaudio_device{} | integer(),
                           SampleFormat :: format()) -> #erlaudio_device_params{}.
output_device_params(Index, SampleFormat) when is_integer(Index) ->
  output_device_params(erlaudio_drv:get_device(Index), SampleFormat);
output_device_params(#erlaudio_device{index=Index} = Device, SampleFormat) ->
  device_params(Index, Device#erlaudio_device.max_output_channels,
                SampleFormat, Device#erlaudio_device.default_low_output_latency).

%% @doc Just a convenience type-check'd function for returning a device params record
-spec device_params(Device :: #erlaudio_device{} | integer(), Channels :: integer(),
  SampleFormat :: format(), SuggestedLatency :: float()) -> #erlaudio_device_params{}.
device_params(Index, Channels, SampleFormat, SuggestedLatency) when
    is_integer(Index) andalso is_integer(Channels) andalso
    is_atom(SampleFormat) andalso is_float(SuggestedLatency) ->
  #erlaudio_device_params{
    index=Index,
    channel_count=Channels,
    sample_format=SampleFormat,
    suggested_latency=SuggestedLatency
  };
device_params(#erlaudio_device{index=Index}=_Device, Channels, SampleFormat, SuggestedLatency) when
    is_integer(Channels) andalso is_atom(SampleFormat) andalso is_float(SuggestedLatency) ->
  #erlaudio_device_params{
    index=Index,
    channel_count=Channels,
    sample_format=SampleFormat,
    suggested_latency=SuggestedLatency
  }.

%% @doc Check if your combination of parameters is supported
-spec format_supported(
    Input :: #erlaudio_device_params{} | null | undefined,
    Output :: #erlaudio_device_params{} | null | undefined,
    SampleRate :: float()
) -> ok | pa_error().
format_supported(Input, Output, SampleRate) ->
  erlaudio_drv:stream_format_supported(Input, Output, SampleRate).

%% @doc start a stream server
-spec open_stream(
    Input :: #erlaudio_device_params{} | null | undefined,
    Output :: #erlaudio_device_params{} | null | undefined,
    SampleRate :: float(),
    FramesPerBuffer :: integer(),
    Flags :: [stream_option()]
) -> {ok, pid()}.
open_stream(Input, Output, SampleRate, FramesPerBuffer, Flags) ->
  erlaudio_drv:open(Input, Output, SampleRate, FramesPerBuffer, Flags).

%% @doc start_link a stream server
-spec open_stream_link(
    Input :: #erlaudio_device_params{} | null | undefined,
    Output :: #erlaudio_device_params{} | null | undefined,
    SampleRate :: float(),
    FramesPerBuffer :: integer(),
    Flags :: [erlaudio_drv:stream_option()]
) -> {ok, pid()}.
open_stream_link(Input, Output, SampleRate, FramesPerBuffer, Flags) ->
  erlaudio_drv:start_link(Input, Output, SampleRate, FramesPerBuffer, Flags).

%% @doc stop/destroy a stream server, ignoring any remaining buffers
abort_stream(Pid) when is_pid(Pid) ->
  gen_server:cast(Pid, abort).

%% @doc stop/destroy a stream server, wait for buffers to flush
close_stream(Pid) when is_pid(Pid) ->
  close_stream(Pid, 30000).

%% @doc stop/destroy a stream server, wait for buffers to flush
close_stream(Pid, Timeout) when is_pid(Pid) andalso is_integer(Timeout) ->
  gen_server:call(Pid, close, Timeout).

%% @doc Return version information for the portaudio we're linked to
-spec portaudio_version() -> {integer(), binary()}.
portaudio_version() ->
  erlaudio_drv:get_pa_version().
