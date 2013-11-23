-type format() :: float32 | int32 | int24 | int16 | int8 | uint8.

-record(erlaudio_device, {
    index = -1 :: integer(),
    name = <<>> :: binary(),
    host_api = -1 :: integer(),
    max_input_channels :: integer(),
    max_output_channels :: integer(),
    default_low_input_latency :: float(),
    default_low_output_latency :: float(),
    default_high_input_latency :: float(),
    default_high_output_latency :: float(),
    default_sample_rate :: float()
}).

-record(erlaudio_device_params, {
    index :: integer(),
    channel_count :: integer(),
    sample_format :: format(),
    suggested_latency :: float()
}).

-record(erlaudio_stream_info, {
    input_latency :: float(),
    output_latency :: float(),
    sample_rate :: float()
}).

-record(erlaudio_hostapi_info, {
    type :: integer(),
    name :: binary(),
    device_count :: integer(),
    default_input_device :: integer(),
    default_output_device :: integer()
}).
