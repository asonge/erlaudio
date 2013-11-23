-module(erlaudio_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("erlaudio/include/erlaudio.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/5, start_link/5]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(s1, {
  handle :: erlaudio_drv:stream(),
  info :: #erlaudio_stream_info{}
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start(
    Input :: #erlaudio_device_params{} | null | undefined,
    Output :: #erlaudio_device_params{} | null | undefined,
    SampleRate :: float(),
    FramesPerBuffer :: integer(),
    Flags :: integer()
) -> {ok, pid()}.
start(Input, Output, SampleRate, FramesPerBuffer, Flags) ->
    lager:info("Start new server?"),
    gen_server:start(?MODULE, [Input, Output, SampleRate, FramesPerBuffer, Flags], [trace, log, statistics]).

-spec start_link(
    Input :: #erlaudio_device_params{} | null | undefined,
    Output :: #erlaudio_device_params{} | null | undefined,
    SampleRate :: float(),
    FramesPerBuffer :: integer(),
    Flags :: integer()
) -> {ok, pid()}.
start_link(Input, Output, SampleRate, FramesPerBuffer, Flags) ->
    lager:info("Start_link new server?"),
    gen_server:start_link(?MODULE, [Input, Output, SampleRate, FramesPerBuffer, Flags], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([Input, Output, SampleRate, FramesPerBuffer, Flags]) ->
  lager:info("INIT new gen_server"),
  case erlaudio_drv:stream_format_supported(Input, Output, SampleRate) of
    ok ->
      lager:info("Params look okay."),
      case erlaudio_drv:stream_open(Input, Output, SampleRate, FramesPerBuffer, Flags) of
        {ok, Handle} ->
          Info = erlaudio_drv:stream_info(Handle),
          {ok, [#s1{handle=Handle, info=Info}]};
        Error ->
          lager:error("Couldn't open: ~p.", [Error]),
          {stop, Error}
      end;
    Error ->
      lager:error("Params were bad: ~p.", [Error]),
      {stop, Error}
  end.



handle_call(get_reader, _From, #s1{handle=H}=State) ->
  {reply, erlaudio_drv:stream_reader(H), State};

handle_call({set_reader, Pid}, _From, #s1{handle=H}=State) ->
  {reply, erlaudio_drv:stream_reader(H, Pid), State};

handle_call(start, _From, #s1{handle=H}=State) ->
  case erlaudio_drv:stream_start(H) of
    ok ->
      {reply, ok, State};
    Error ->
      {stop, Error, Error, State}
  end;

handle_call(stop, _From, #s1{handle=H}=State) ->
  erlaudio_drv:stream_stop(H),
  receive
    {erlaudio, H, stop} ->
      {stop, stopped, State}
    after 120000 ->
      {reply, {error, timeout}, State}
  end;

handle_call(Request, _From, State) ->
  lager:warn("Unknown call ~p~n", [Request]),
  {reply, ok, State}.



handle_cast(abort, #s1{handle=H}=State) ->
  erlaudio_drv:stream_abort(H),
  {stop, aborted, State};

handle_cast(Msg, State) ->
  lager:warn("Unknown cast ~p~n", [Msg]),
  {noreply, State}.



handle_info(Info, State) ->
  lager:warn("Unknown info ~p~n", [Info]),
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

