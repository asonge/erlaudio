
-record(erlaudio_device, {
    index :: integer(),
    name :: binary(),
    host_api :: integer(),
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
	sample_format :: integer(),
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
