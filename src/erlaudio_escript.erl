-module(erlaudio_escript).

-include_lib("erlaudio/include/erlaudio.hrl").

-export([main/1]).

main(["devices"|RawOpts]) ->
  SpecOpts = [
    { verbose, $v, "verbose", {boolean, false}, undefined },
    { input,   $i, "input",   {boolean, true},  undefined },
    { output,  $o, "output",  {boolean, true},  undefined }
  ],
  case getopt:parse(SpecOpts, RawOpts) of
    {ok, {Opts, _Args}} ->
      Input = proplists:get_value(input, Opts),
      Output = proplists:get_value(output, Opts),
      Devices = [ Dev || Dev <- erlaudio:devices(),
        (Input ==true andalso Dev#erlaudio_device.max_input_channels  > 0) orelse
        (Output==true andalso Dev#erlaudio_device.max_output_channels > 0)
      ],
      case proplists:get_value(verbose, Opts) of
        true ->
          io:put_chars(iolist_to_binary([ [print_device(Dev),"\n"] || Dev <- Devices ]));
        false ->
          io:put_chars(iolist_to_binary([
            [ integer_to_list(Dev#erlaudio_device.index), <<" ">>,
              Dev#erlaudio_device.name, <<"\n">>
            ]
            || Dev <- Devices 
          ]))
      end;
    Else ->
      io:format("Error: ~p~n", [Else]),
      usage(),
      halt(1)
  end;
% erlaudio_escript.erl:46: The call
% erlaudio:open_stream_link(
% #erlaudio_device_params{
%   index::integer(),
%   channel_count::integer(),
%   sample_format::'float32' | 'int16' | 'int24' | 'int32' | 'int8' | 'uint8' | 'undefined',
%   suggested_latency::float()
% },
% 'undefined',
% 4.8e+4,
% 'undefined',
% [])
%will never return since the success typing is
% ('null' | 'undefined' | #erlaudio_device_params{
%   index::'undefined' | integer(),
%   channel_count::'undefined' | integer(),
%   sample_format::'float32' | 'int16' | 'int24' | 'int32' | 'int8' | 'uint8' | 'undefined',
%   suggested_latency::'undefined' | float()
% },
% 'null' | 'undefined' | #erlaudio_device_params{
%   index::'undefined' | integer(),
%   channel_count::'undefined' | integer(),
%   sample_format::'float32' | 'int16' | 'int24' | 'int32' | 'int8' | 'uint8' | 'undefined',
%   suggested_latency::'undefined' | float()
% },
% float(),
% integer(),
% ['noclip' | 'nodither' | 'nodrop_input']) ->
% {'ok',pid()} and the contract is
% (Input::#erlaudio_device_params{} | 'null' | 'undefined',Output::#erlaudio_device_params{} | 'null' | 'undefined',SampleRate::float(),FramesPerBuffer::integer(),Options::[erlaudio_drv:stream_option()]) -> {'ok',pid()}
main(["histogram"|RawOpts]) ->
  lager:start(),
  SpecOpts = [
    { verbose, $v, "verbose", {boolean, false}, undefined }
  ],
  case getopt:parse(SpecOpts, RawOpts) of
    {ok, {Opts, [Input]}} ->
      InputIndex = list_to_integer(Input),
      lager:info("OPENING!"),
      {ok, Pid} = erlaudio:open_stream_link(erlaudio:default_input_params(int16), undefined, 48000.0, 512, []),
      lager:info("OPENED!"),
      listen_loop(0);
    Else ->
      io:format("Error: ~p~n", [Else]),
      usage(),
      halt(1)
  end;
% main(["hostapis"]) ->
%  io:put_chars([ print_device(Dev) || Dev <- devices() ]);
main(["version"]) ->
  lager:info("TEST"),
  {_, Version} = erlaudio:portaudio_version(),
  io:put_chars([Version, "\n"]);
main(_) ->
  usage(),
  halt(1).

listen_loop(Counter) when Counter > 100 ->
  io:format("~nDone.~n");
listen_loop(Counter) ->
  io:format("\rBlah: ~w", [Counter]),
  receive
    after 100 ->
      listen_loop(Counter+1)
  end.

-spec print_device(#erlaudio_device{}) -> iolist().
print_device(#erlaudio_device{
    index=Index, name=Name, host_api=HostAPI,
    max_input_channels=InputChans, max_output_channels=OutputChans,
    default_low_input_latency=LowInputLatency, default_low_output_latency=LowOutputLatency,
    default_high_input_latency=HighInputLatency, default_high_output_latency=HighOutputLatency,
    default_sample_rate=DefaultRate
}) ->
  HostInfo = erlaudio_drv:get_hostapi_info(HostAPI),
  [
    Name, ":\n",
      "\tIndex:               ", integer_to_list(Index), "\n",
      "\tHost API:            ", HostInfo#erlaudio_hostapi_info.name, "\n",
      "\tMax Input Chans:     ", integer_to_list(InputChans), "\n",
      "\tMax Output Chans:    ", integer_to_list(OutputChans), "\n",
      "\tLow Input Latency:   ", float_to_list(LowInputLatency   * 1000, [{decimals, 7}, compact]), " ms\n",
      "\tLow Output Latency:  ", float_to_list(LowOutputLatency  * 1000, [{decimals, 7}, compact]), " ms\n",
      "\tHigh Input Latency:  ", float_to_list(HighInputLatency  * 1000, [{decimals, 7}, compact]), " ms\n",
      "\tHigh Output Latency: ", float_to_list(HighOutputLatency * 1000, [{decimals, 7}, compact]), " ms\n",
      "\tDefault Sample Rate: ", float_to_list(DefaultRate, [{decimals, 0}, compact]), " Hz\n"
  ].

usage() ->
  io:format("Usage: blah blah blah.~n").
