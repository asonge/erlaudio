%% @hidden
-module(erlaudio_escript).

-include("erlaudio.hrl").

-export([main/1]).

main(["devices"|_]) ->
  Devices = [ Dev || Dev <- erlaudio:devices() ],
  io:put_chars(iolist_to_binary([
    [ integer_to_list(Dev#erlaudio_device.index), <<" ">>,
      Dev#erlaudio_device.name, <<"\n">>
    ]
    || Dev <- Devices
  ]));
main(["histogram"]) ->
  main(["histogram","-"]);
main(["histogram","-"|Rest]) ->
  #erlaudio_device{index=Index} = erlaudio:default_input_device(),
  main(["histogram", integer_to_list(Index)|Rest]);
main(["histogram", Input]) ->
  main(["histogram", Input, "30"]);
main(["histogram", Input, Time]) ->
  %
  Params = erlaudio:input_device_params(list_to_integer(Input), int16),
  {ok, Handle} = erlaudio:stream_open(Params, undefined, 48000.0, 2048, []),
  io:setopts([{encoding, unicode}]),
  ok = erlaudio:stream_start(Handle),
  erlang:send_after(list_to_integer(Time)*1000, self(), timeout),
  listen_loop(Handle);
main(["record"]) -> main(["record","test.pcm","20"]);
main(["record",Filename,Time]) ->
  {ok, Handle} = erlaudio:stream_open(default, null, 48000.0, 2048, []),
  ok = erlaudio:stream_start(Handle),
  erlang:send_after(list_to_integer(Time)*1000, self(), timeout),
  {ok, Fh} = file:open(Filename, [write]),
  record_loop(Fh, Handle);
main(["play"]) -> main(["play","test.pcm"]);
main(["play",Filename]) ->
  % #erlaudio_device{index=Idx} = erlaudio:default_output_device()
  Output0 = erlaudio:default_output_params(int16),
  Output = Output0#erlaudio_device_params{channel_count=2},
  {ok, Handle} = erlaudio:stream_open(null, Output, 48000.0, 2048, []),
  {ok, Fh} = file:open(Filename, [read]),
  ok = erlaudio:stream_start(Handle),
  io:format("START!~n"),
  play_loop(Fh, Handle);
main(["pipe"]) -> main(["pipe","-","-"]);
main(["pipe",Input]) -> main(["pipe",Input,"-"]);
main(["pipe","-"|Rest]) ->
  #erlaudio_device{index=Index} = erlaudio:default_input_device(),
  main(["pipe", integer_to_list(Index)|Rest]);
main(["pipe",Input,"-"|Rest]) ->
  #erlaudio_device{index=Index} = erlaudio:default_output_device(),
  main(["pipe", Input, integer_to_list(Index)|Rest]);
main(["pipe",Input,Output]) -> main(["pipe",Input,Output,"30"]);
main(["pipe",Input,Output,Time]) ->
  InputParams = erlaudio:input_device_params (list_to_integer(Input),  int16),
  OutputParams = erlaudio:output_device_params(list_to_integer(Output), int16),
  {ok, Handle} = erlaudio:stream_open(InputParams, OutputParams, 48000.0, 2048, []),
  io:setopts([{encoding, unicode}]),
  erlang:send_after(list_to_integer(Time)*1000, self(), timeout),
  ok = erlaudio:stream_start(Handle),
  listen_pipe(Handle);
main(["flag"]) -> main(["flag","60"]);
main(["flag",Time]) ->
  % observer:start(),
  erlang:send_after(list_to_integer(Time)*1000, self(), timeout),
  listen_flag();
main(["version"]) ->
  {_, Version} = erlaudio:portaudio_version(),
  io:put_chars([Version, "\n"]);
main(_) ->
  usage(),
  halt(1).

play_loop(Fh, Handle) ->
  case erlaudio:stream_write_available(Handle) of
    ToRead when is_integer(ToRead) andalso ToRead > 8192 ->
      case file:read(Fh, 8192) of
        eof ->
          erlaudio:stream_stop(Handle);
        {ok, Data} ->
          erlaudio:stream_write(Handle, Data),
          play_loop(Fh, Handle)
      end;
    ToRead when is_integer(ToRead) ->
      timer:sleep(100),
      play_loop(Fh,Handle)
  end.

record_loop(Fh, Handle) ->
  {ok, Data} = erlaudio:stream_read(Handle),
  file:write(Fh, Data),
  receive
    timeout ->
      file:close(Fh),
      erlaudio:stream_close(Handle)
  after 0 ->
    record_loop(Fh, Handle)
  end.

stream_write(_, _, 0) -> {error, out_of_tries};
stream_write(Handle, Data, Tries) ->
  case erlaudio:stream_write(Handle, Data) of
    ok -> ok;
    {error, toobig} ->
      timer:sleep(1),
      stream_write(Handle, Data, Tries-1)
  end.

flush(H, Count) ->
  receive {erlaudio, H, {pcmdata_, _}} -> flush(H, Count+1)
  after 0 -> Count
  end.

% mem_stats() ->
%   [[$\t,integer_to_list(recon_alloc:memory(T,C))] || T <- [used,allocated,unused], C <- [current, max]].
% mem_stats2() ->
%   lists:sort([ io_lib:format("~p ~s~n", [{Alloc,N}, format_stats(Stats)])
%      || {{Alloc,N},Stats} <- recon_alloc:fragmentation(current),
%         Alloc /= ll_alloc, Alloc /= ets_alloc]).
%
% format_stats(Stats) ->
%   io_lib:format("~p ~p", [proplists:get_value(sbcs_usage,Stats),
%                       proplists:get_value(mbcs_usage,Stats)]).

malloc_stats() ->
  Calls = [ {N,proplists:get_value(calls,Info)}
            || {_,N,Info} <- erlang:system_info({allocator,driver_alloc}) ],
  CallInfo = [ {Type,
                lists:keyfind(driver_alloc,1,C),
                lists:keyfind(driver_free,1,C)} || {Type,C} <- Calls ],
  lists:foldl(fun ({_,{_,_,Alloc},{_,_,Free}}, {Allocs, Frees}) ->
                {Allocs+Alloc,Frees+Free}
              end, {0,0}, CallInfo).

listen_flag() ->
  Input  = erlaudio:default_input_params (int16),
  Output = erlaudio:default_output_params(int16),
  {ok, H} = erlaudio:stream_open(Input, Output, 48000.0, 2048, []),
  erlaudio:stream_start(H),
  timer:sleep(100),
  erlaudio:stream_close(H),
  % io:format("Grabbed ~p~n", [flush(H, 0)]),
  % io:format("Grabbed ~p\t~p\t~p\t~p\t~p~n",[
  %   flush(H, 0),
  %   recon_alloc:memory(used,current),
  %   recon_alloc:memory(used,max),
  %   recon_alloc:memory(allocated,current),
  %   recon_alloc:memory(allocated,max)
  % ]),
  {Allocs,Frees} = malloc_stats(),
  io:format("Flush/Allocs/Frees/Diff: ~p\t~p\t~p\t~p~n", [flush(H,0),Allocs,Frees,Allocs-Frees]),
  garbage_collect(),
  % io:put_chars([mem_stats2(),$\n]),
  case should_stop() of
    true -> ok;
    false -> listen_flag()
  end.

listen_pipe(Handle) ->
  {ok, Data} = erlaudio:stream_read(Handle),
  ok = stream_write(Handle, Data, 3),
  % hist(Data, {0,0,0,0,
  %             0,0,0,0,
  %             0,0,0,0,
  %             0,0,0,0}),
  case should_stop() of
    true -> ok;
    false -> listen_pipe(Handle)
  end.

listen_loop(Handle) ->
  {ok, Data} = erlaudio:stream_read(Handle),
  hist(Data, {0,0,0,0,
              0,0,0,0,
              0,0,0,0,
              0,0,0,0}),
  % io:format("Got ~p bytes~n", [byte_size(Data)]),
  case should_stop() of
    true -> io:format("~nErlaudio Stream: ~p~n", [erlaudio:stream_close(Handle)]);
    false -> listen_loop(Handle)
  end.

char(C) when C < 0.1 -> <<"  "/utf8>>;
char(C) when C < 0.25 -> <<"▁ "/utf8>>;
char(C) when C < 0.4 -> <<"▂ "/utf8>>;
char(C) when C < 0.55 -> <<"▃ "/utf8>>;
char(C) when C < 0.70 -> <<"▅ "/utf8>>;
char(C) when C < 0.85 -> <<"▆ "/utf8>>;
char(_) -> <<"▇ "/utf8>>.

hist(<<>>, Hist) ->
  LHist = tuple_to_list(Hist),
  Max = lists:max(LHist),
  io:put_chars([$\r]),
  io:put_chars([char(I/Max) || I <- LHist]);
hist(<<L:16/signed,R:16/signed,Rest/binary>>, Hist) ->
  L1 = min(trunc(abs(L)/2048)+1, 16),
  R1 = min(trunc(abs(R)/2048)+1, 16),
  Hist1 = setelement(L1, Hist,  element(L1, Hist)+1),
  Hist2 = setelement(R1, Hist1, element(R1, Hist1)+1),
  hist(Rest, Hist2).

should_stop() ->
  receive timeout -> true
  after 0 -> false
  end.

usage() ->
  io:format("Usage: erlaudio [devices|histogram|version|usage]").
