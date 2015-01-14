-module(erlaudio_escript).

-include_lib("erlaudio/include/erlaudio.hrl").

-export([main/1]).

main(["devices"|_]) ->
  Devices = [ Dev || Dev <- erlaudio:devices() ],
  io:put_chars(iolist_to_binary([
    [ integer_to_list(Dev#erlaudio_device.index), <<" ">>,
      Dev#erlaudio_device.name, <<"\n">>
    ]
    || Dev <- Devices
  ]));
main(["histogram", _Input]) ->
  Params = erlaudio:default_input_params(int16),
  {ok, Handle} = erlaudio_drv:stream_open(Params, undefined, 48000.0, 480, []),
  ok = erlaudio_drv:stream_start(Handle),
  listen_loop(Handle);
main(["version"]) ->
  {_, Version} = erlaudio:portaudio_version(),
  io:put_chars([Version, "\n"]);
main(_) ->
  usage(),
  halt(1).

listen_loop(Handle) ->
  receive
    {erlaudio_pcmdata,Handle,Data} ->
      io:format("Got ~p bytes~n", [byte_size(Data)]),
      listen_loop(Handle)
    after 10000 ->
      io:format("~nTIMEOUT~n"),
      erlaudio_drv:stop(Handle)
  end.

usage() ->
  io:format("Usage: erlaudio [devices|histogram|version|usage]").
