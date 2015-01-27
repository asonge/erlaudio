%% @headerfile "erlaudio.hrl"
-module(erlaudio).

-include_lib("erlaudio.hrl").

-export([
  default_input_device/0,
  default_output_device/0,
  device/1,
  devices/0
]).
-export([
  default_input_params/1,
  default_output_params/1,
  input_device_params/2,
  output_device_params/2,
  device_params/4
]).
-export([
  stream_format_supported/3,
  stream_open/4,
  stream_open/5,
  stream_start/1,
  stream_info/1,
  stream_read/1,
  stream_read/2,
  stream_write/2,
  stream_abort/1,
  stream_stop/1,
  stream_close/1,
  stream_active/1,
  stream_stopped/1,
  stream_write_available/1,
  stream_write_available/2,
  stream_writebuffer_size/1
]).
-export([
  portaudio_version/0
]).

-opaque handle() :: binary().

-export_type([handle/0]).

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
-spec stream_format_supported(
    Input :: #erlaudio_device_params{} | null | undefined,
    Output :: #erlaudio_device_params{} | null | undefined,
    SampleRate :: float()
) -> ok | pa_error().
stream_format_supported(Input, Output, SampleRate) ->
  erlaudio_drv:stream_format_supported(Input, Output, SampleRate).

%% @doc See stream_open/5
-spec stream_open(
  Input :: #erlaudio_device_params{} | default | null | undefined,
  Output :: #erlaudio_device_params{} | default | null | undefined,
  SampleRate :: float(),
  FramesPerBuffer :: integer()
) -> {ok, handle()} | pa_error().
stream_open(Input, Output, SampleRate, FramesPerBuffer) ->
  stream_open(Input, Output, SampleRate, FramesPerBuffer, []).

%% @doc Open a stream for the given devices
-spec stream_open(
    Input :: #erlaudio_device_params{} | default | null | undefined,
    Output :: #erlaudio_device_params{} | default | null | undefined,
    SampleRate :: float(),
    FramesPerBuffer :: integer(),
    Flags :: [stream_option()]
) -> {ok, handle()} | pa_error().
stream_open(default, Output, SampleRate, FramesPerBuffer, Flags) ->
    Input = default_input_params(int16),
    stream_open(Input, Output, SampleRate, FramesPerBuffer, Flags);
stream_open(Input, default, SampleRate, FramesPerBuffer, Flags) ->
    Output = default_output_params(int16),
    stream_open(Input, Output, SampleRate, FramesPerBuffer, Flags);
stream_open(Input, Output, SampleRate, FramesPerBuffer, Flags) ->
  erlaudio_drv:stream_open(Input, Output, SampleRate, FramesPerBuffer, Flags).

%% @doc start a stream server
-spec stream_start(handle()) -> ok | {error, timeout} | pa_error().
stream_start(Handle) ->
  case erlaudio_drv:stream_start(Handle) of
    ok ->
      receive
        {erlaudio, Handle, started} -> ok
      after 30000 -> {error, timeout}
      end;
    {error, Error} -> {error, Error}
  end.

%% @doc Add stream data to the write buffer
-spec stream_write(handle(), iodata()) -> ok | {error, toobig | badbinsize}.
stream_write(Handle, Data) ->
  erlaudio_drv:stream_write(Handle, Data).

%% @doc Read data from the stream. See stream_read/2.
-spec stream_read(handle()) -> {ok, binary()} | pa_error() | eos.
stream_read(Handle) -> stream_read(Handle, 2000).

%% @doc Read data from the stream, timing out after Timeout milliseconds.
-spec stream_read(handle(), non_neg_integer()) -> {ok, binary()} | pa_error() | eos.
stream_read(Handle, Timeout) when Timeout >= 0 ->
  receive
    {erlaudio, Handle, {pcmdata, Data}} -> {ok, Data};
    {erlaudio, Handle, started} -> stream_read(Handle, Timeout);
    {erlaudio, Handle, closed} -> eos;
    {erlaudio, Handle, {error, Error}} -> {error, Error}
    % As a hack, every second we don't have data, we should probably check to see
    % if we've still got a listening handle
  after Timeout ->
    case erlaudio_drv:stream_is_active(Handle) of
      true -> stream_read(Handle, Timeout); % We loop forever.
      false -> {error, stream_closed};
      {error, Reason} -> {error, Reason}
    end
  end.

%% @doc Stop a stream, ignoring any remaining buffers
-spec stream_abort(handle()) -> ok | pa_error().
stream_abort(Handle) ->
  case erlaudio_drv:stream_abort(Handle) of
    ok -> drain(Handle);
    Else -> Else
  end.

%% @doc Stops and closes a stream, ignoring remaining buffers
-spec stream_close(handle()) -> ok | pa_error().
stream_close(Handle) ->
  case erlaudio_drv:stream_close(Handle) of
    ok -> drain(Handle), ok;
    Else -> Else
  end.

%% @doc Stop a stream. Blocks until audio write buffers finish playing.
-spec stream_stop(handle()) -> ok | pa_error() | {error, timeout}.
stream_stop(Handle) ->
  case erlaudio_drv:stream_stop(Handle) of
    ok ->
      receive
        {erlaudio, Handle, closed} -> drain(Handle)
        after 30000 -> drain(Handle), {error, timeout}
      end;
    Error ->
      Error
  end.

%% @doc Gets stream info
-spec stream_info(handle()) -> #erlaudio_stream_info{}.
stream_info(Handle) -> erlaudio_drv:stream_info(Handle).

%% @doc Tests if the stream is active.
-spec stream_active(handle()) -> true | false | pa_error().
stream_active(Handle) -> erlaudio_drv:stream_is_active(Handle).

%% @doc Tests if the stream is stopped.
-spec stream_stopped(handle()) -> true | false | pa_error().
stream_stopped(Handle) -> erlaudio_drv:stream_is_stopped(Handle).

%% @doc Checks the size of the write buffer.
-spec stream_writebuffer_size(handle()) -> integer().
stream_writebuffer_size(Handle) -> erlaudio_drv:stream_writebuffer_size(Handle).

%% @doc Checks how much of the buffer is available for writing.
-spec stream_write_available(handle()) -> integer().
stream_write_available(Handle) -> stream_write_available(Handle, available).

%% @doc Gives information about the status of the internal buffers.
-spec stream_write_available(handle(), available|internal|total) -> integer().
stream_write_available(Handle, available) ->
  case erlaudio_drv:stream_write_available(Handle) of
    {ok, Avail, _} -> Avail;
    Else -> Else
  end;
stream_write_available(Handle, internal) ->
  case erlaudio_drv:stream_write_available(Handle) of
    {ok, _, Internal} -> Internal;
    Else -> Else
  end;
stream_write_available(Handle, total) ->
  case erlaudio_drv:stream_write_available(Handle) of
    {ok, Avail, Internal} -> Avail + Internal;
    Else -> Else
  end.

%% @doc Return version information for the portaudio we're linked to
-spec portaudio_version() -> {integer(), binary()}.
portaudio_version() -> erlaudio_drv:get_pa_version().

drain(Handle) ->
  Res = receive
    {erlaudio_pcmdata, Handle, _} -> ok;
    {erlaudio, Handle, Msg} when Msg/=closed -> ok;
    {erlaudio_error, Handle, _} -> ok;
    {erlaudio, Handle, closed} -> done
  after 2000 -> done
  end,
  case Res of
    ok -> drain(Handle);
    done -> ok
  end.
