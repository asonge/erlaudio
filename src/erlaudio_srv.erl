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
  handle
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
	gen_server:start(?MODULE, [Input, Output, SampleRate, FramesPerBuffer, Flags], []).

-spec start_link(
    Input :: #erlaudio_device_params{} | null | undefined,
    Output :: #erlaudio_device_params{} | null | undefined,
    SampleRate :: float(),
    FramesPerBuffer :: integer(),
    Flags :: integer()
) -> {ok, pid()}.
start_link(Input, Output, SampleRate, FramesPerBuffer, Flags) ->
    gen_server:start_link(?MODULE, [Input, Output, SampleRate, FramesPerBuffer, Flags], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Input, Output, SampleRate, FramesPerBuffer, Flags]) ->
    {ok, [#s1{}]}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

