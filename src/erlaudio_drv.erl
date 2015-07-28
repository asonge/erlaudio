%% @headerfile "erlaudio.hrl"
%% @doc
%% This is a separate, private, less-supported API for interacting more directly
%% with the erlaudio nif. It's less natural and a bit harder to use, and
%% functions directly invoke the nifs.
%%
-module(erlaudio_drv).

-export([
  get_pa_version/0,
  get_default_hostapi_index/0,
  get_device_index_from_hostapi/2,
  get_hostapi_count/0,
  get_hostapi_info/1,
  get_hostapi_index_from_type/1
]).
-export([
  get_default_input_device_index/0,
  get_default_output_device_index/0,
  get_device/1,
  get_device_count/0
]).
-export([
  stream_format_supported/3,
  stream_open/5,
  stream_start/1,
  stream_owner/1,
  stream_owner/2,
  stream_reader/1,
  stream_reader/2,
  stream_write/2,
  stream_close/1,
  stream_stop/1,
  stream_abort/1,
  stream_info/1,
  stream_is_stopped/1,
  stream_is_active/1,
  stream_write_available/1,
  stream_writebuffer_size/1
]).

-type handle() :: erlaudio:handle().

-type api_type() :: directsound | mme | asio | soundmanager | coreaudio | oss | alsa | al | beos | wdmks | jack | wasapi | audiosciencehpi | integer().

-export_type([handle/0]).

-on_load(init/0).

-include("erlaudio.hrl").

-define(APPNAME, erlaudio).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
  PrivDir = case code:priv_dir(erlaudio) of
    {error, bad_name} ->
      try escript:script_name() of
        Filename ->
          AppPath = filename:dirname(Filename),
          filename:join(AppPath, "priv")
      catch
        _:_ ->
          EbinDir = filename:dirname(code:which(?MODULE)),
          AppPath = filename:dirname(EbinDir),
          filename:join(AppPath, "priv")
      end;
    Path ->
      Path
  end,
  case os:type() of
    {win32, _} ->
      EnvPath = os:getenv("PATH"),
      os:putenv("PATH", EnvPath++";"++PrivDir),
      os:cmd("echo %PATH%");
    _ -> ok
  end,
  erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

%% @doc Get the version info from portaudio
-spec get_pa_version() -> {integer(), binary()}.
get_pa_version() ->
  ?nif_stub.

%% @doc Get the default hostapi index
-spec get_default_hostapi_index() -> integer().
get_default_hostapi_index() ->
  ?nif_stub.

%% @doc Get the device index from the hostapi
-spec get_device_index_from_hostapi(HostApiIndex :: integer(), HostApiDeviceIndex :: integer()) -> integer().
get_device_index_from_hostapi(_HostApiIndex, _HostApiDeviceIndex) ->
  ?nif_stub.

%% @doc Get the total number of supported hostapi's
-spec get_hostapi_count() -> integer().
get_hostapi_count() ->
  ?nif_stub.

%% @doc Get the hostapi index from the type
-spec get_hostapi_index_from_type(Type :: api_type()) -> integer().
get_hostapi_index_from_type(_Type) ->
  ?nif_stub.

%% @doc Get the hostapi info by the hostapi index
-spec get_hostapi_info(Index :: integer()) -> #erlaudio_hostapi_info{}.
get_hostapi_info(_Index) ->
  ?nif_stub.

%% @doc Get the default output device's index
-spec get_default_input_device_index() -> integer().
get_default_input_device_index() ->
  ?nif_stub.

%% @doc Get the default output device's index
-spec get_default_output_device_index() -> integer().
get_default_output_device_index() ->
  ?nif_stub.

%% @doc Get the device by index
-spec get_device(Index :: integer()) -> #erlaudio_device{}.
get_device(_Index) ->
  ?nif_stub.

%% @doc Get the number of devices
-spec get_device_count() -> integer().
get_device_count() ->
  ?nif_stub.

%% @doc Is this stream supported?
-spec stream_format_supported(
    Input :: #erlaudio_device_params{} | null | undefined,
    Output :: #erlaudio_device_params{} | null | undefined,
    SampleRate :: float()
) -> ok | pa_error().
stream_format_supported(_Input, _Output, _SampleRate) ->
  ?nif_stub.

%% @doc Open a stream
-spec stream_open(
    Input  :: #erlaudio_device_params{} | null | undefined,
    Output :: #erlaudio_device_params{} | null | undefined,
    SampleRate :: float(),
    FramesPerBuffer :: integer(),
    Flags :: [stream_option()]
) -> {ok, handle()} | pa_error().
stream_open(_Input, _Output, _SampleRate, _FramesPerBuffer, _Flags) ->
  ?nif_stub.

%% @doc Stops the stream immediately
-spec stream_start(handle()) -> ok | pa_error() | {error, threadfailed}.
stream_start(_Ref) ->
  ?nif_stub.

%% @doc Stops the stream immediately, and closes the resource
-spec stream_close(handle()) -> ok | pa_error().
stream_close(_Ref) ->
  ?nif_stub.

%% @doc Stops the stream, but blocks while the buffers empty
-spec stream_stop(handle()) -> ok | pa_error().
stream_stop(_Ref) ->
  ?nif_stub.

%% @doc Stops the stream immediately
-spec stream_abort(handle()) -> ok | pa_error().
stream_abort(_Ref) ->
  ?nif_stub.

%% @doc Get the stream "owner", or where control messages go
-spec stream_owner(handle()) -> pid().
stream_owner(_Ref) ->
  ?nif_stub.

%% @doc Set the stream "owner", or where control messages go
-spec stream_owner(handle(), pid()) -> ok | pa_error().
stream_owner(_Ref, _Pid) ->
  ?nif_stub.

%% @doc Get the stream "reader", or where pcmdata messages go
-spec stream_reader(handle()) -> pid().
stream_reader(_Ref) ->
  ?nif_stub.

%% @doc Set the stream "reader", or where pcmdata messages go
-spec stream_reader(handle(), pid()) -> ok | pa_error().
stream_reader(_Ref, _Pid) ->
  ?nif_stub.

%% @doc Write stream data into our ringbuffer
-spec stream_write(handle(), iodata()) -> ok | {error, toobig | badbinsize}.
stream_write(_Ref, _Data) ->
  ?nif_stub.

%% @doc Get stream info
-spec stream_info(handle()) -> #erlaudio_stream_info{}.
stream_info(_Ref) ->
  ?nif_stub.

%% @doc Is the stream stopped?
-spec stream_is_stopped(handle()) -> boolean() | pa_error().
stream_is_stopped(_Ref) ->
  ?nif_stub.

%% @doc Is the stream active?
-spec stream_is_active(handle()) -> boolean() | pa_error().
stream_is_active(_Ref) ->
  ?nif_stub.

%% @doc Get information about the state of the write buffers.
-spec stream_writebuffer_size(handle()) -> integer() | pa_error() | {error, notrunning}.
stream_writebuffer_size(_Ref) ->
  ?nif_stub.

%% @doc Get information about the state of the write buffers.
-spec stream_write_available(handle()) -> {ok, RingBuffer :: integer(), Internal :: integer()} | pa_error() | {error, notrunning}.
stream_write_available(_Ref) ->
  ?nif_stub.
