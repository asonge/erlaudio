-type format() :: float32 | int32 | int24 | int16 | int8 | uint8.

-type error() :: not_initialized | unanticipated_host_error |
                 invalid_number_of_channels | invalid_sample_rate |
                 invalid_device | invalid_flag | sample_format_unsupported |
                 bad_device_combo | insufficient_memory | buffer_too_big |
                 buffer_too_small | nocallback | badcallback | timeout |
                 internal_error | device_unavailable |
                 incompatible_host_stream_info | stream_stopped |
                 stream_not_stopped | input_overflowed | output_underflowed |
                 no_hostapi | invalid_hostapi | noread_callback |
                 nowrite_callback | output_only | input_only |
                 incompatible_hostapi | badbuffer | unknownerror.
-type pa_error() :: {error, error()}.
-type stream_option() :: noclip | nodither | nodropinput.

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
