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
  stream_is_active/1
]).

-type api_type() :: directsound | mme | asio | soundmanager | coreaudio | oss | alsa | al | beos | wdmks | jack | wasapi | audiosciencehpi | integer().
-type pa_error() :: {error, atom()}.
% THIS IS WRONG-type device_option() :: {channel, pos_integer()} | {sample_format, format()} | {latency, float()}.
-type stream_option() :: noclip | nodither | nodrop_input.
-opaque stream() :: reference().

-export_type([stream/0, stream_option/0]).

-on_load(init/0).

-include_lib("erlaudio/include/erlaudio.hrl").

-define(APPNAME, erlaudio).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
  PrivDir = case code:priv_dir(?MODULE) of
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
  erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).


-spec get_pa_version() -> {integer(), binary()}.
get_pa_version() ->
    ?nif_stub.

-spec get_default_hostapi_index() -> integer().
get_default_hostapi_index() ->
    ?nif_stub.

-spec get_device_index_from_hostapi(HostApiIndex :: integer(), HostApiDeviceIndex :: integer()) -> integer().
get_device_index_from_hostapi(_HostApiIndex, _HostApiDeviceIndex) ->
  ?nif_stub.

-spec get_hostapi_count() -> integer().
get_hostapi_count() ->
  ?nif_stub.

-spec get_hostapi_index_from_type(Type :: api_type()) -> integer().
get_hostapi_index_from_type(_Type) ->
  ?nif_stub.

-spec get_hostapi_info(Index :: integer()) -> #erlaudio_hostapi_info{}.
get_hostapi_info(_Index) ->
  ?nif_stub.

-spec get_default_input_device_index() -> integer().
get_default_input_device_index() ->
    ?nif_stub.

-spec get_default_output_device_index() -> integer().
get_default_output_device_index() ->
    ?nif_stub.

-spec get_device(Index :: integer()) -> #erlaudio_device{}.
get_device(_Index) ->
    ?nif_stub.

-spec get_device_count() -> integer().
get_device_count() ->
    ?nif_stub.

-spec stream_format_supported(
    Input :: #erlaudio_device_params{} | null | undefined,
    Output :: #erlaudio_device_params{} | null | undefined,
    SampleRate :: float()
) -> ok | pa_error().
stream_format_supported(_Input, _Output, _SampleRate) ->
    ?nif_stub.

stream_open(_Input, _Output, _SampleRate, _FramesPerBuffer, _Flags) ->
    ?nif_stub.

-spec stream_start(Stream :: stream()) -> ok | pa_error().
stream_start(_Ref) ->
    ?nif_stub.

-spec stream_close(Stream :: stream()) -> ok | pa_error().
stream_close(_Ref) ->
    ?nif_stub.

-spec stream_stop(Stream :: stream()) -> ok | pa_error().
stream_stop(_Ref) ->
    ?nif_stub.

-spec stream_abort(Stream :: stream()) -> ok | pa_error().
stream_abort(_Ref) ->
    ?nif_stub.

-spec stream_owner(Stream :: stream()) -> pid().
stream_owner(_Ref) ->
  ?nif_stub.

-spec stream_owner(Stream :: stream(), Pid :: pid()) -> ok | pa_error().
stream_owner(_Ref, _Pid) ->
    ?nif_stub.

-spec stream_reader(Stream :: stream()) -> pid().
stream_reader(_Ref) ->
  ?nif_stub.

-spec stream_reader(Stream :: stream(), Pid :: pid()) -> ok | pa_error().
stream_reader(_Ref, _Pid) ->
    ?nif_stub.

-spec stream_write(Stream :: stream(), Data :: iolist()) -> ok | pa_error().
stream_write(_Ref, _Data) ->
  ?nif_stub.

-spec stream_info(Stream :: stream()) -> #erlaudio_stream_info{} | pa_error().
stream_info(_Ref) ->
    ?nif_stub.

-spec stream_is_stopped(Stream :: stream()) -> boolean().
stream_is_stopped(_Ref) ->
    ?nif_stub.

-spec stream_is_active(Stream :: stream()) -> boolean().
stream_is_active(_Ref) ->
    ?nif_stub.
