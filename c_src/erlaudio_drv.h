#include "inttypes.h"
#include "stdint.h"

struct int_to_str {
    int num;
    const char *str;
};

struct erlaudio_binqueue {
    ErlNifBinary** bins;
    size_t size;
    size_t head;
    size_t tail;
    size_t head_cursor;
};

struct erlaudio_stream_handle
{
    ErlNifTid    thread;
    ErlNifThreadOpts* thread_opts;
    ErlNifCond*  cnd;
    ErlNifMutex* mtx;
    
    // Stream related
    PaStream* pa;
    PaStreamParameters* input;
    PaStreamParameters* output;
    
    int flags;
    
    double sample_rate;
    unsigned long frames_per_buffer;
    // Erlang-land related
    ErlNifPid* owner_pid;
    ErlNifPid* reader_pid;
    
    // Buffers and pre-calc values
    unsigned short input_frame_size;
    unsigned short input_sample_size;
    
    struct erlaudio_binqueue output_queue;
    
    uint8_t total_samples;
    
    unsigned short output_frame_size;
    unsigned short output_sample_size;
};

static ERL_NIF_TERM erlaudio_get_pa_version                  (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_get_default_hostapi_index       (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_get_device_index_from_hostapi   (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_get_hostapi_count               (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_get_hostapi_info                (ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_get_hostapi_index_from_type     (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_get_default_input_device_index  (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_get_default_output_device_index (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_get_device                      (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_get_device_count                (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_stream_format_supported         (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_stream_open                     (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_stream_start                    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_stream_owner                    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_stream_reader                   (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_stream_stop                     (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_stream_close                    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_stream_abort                    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_stream_write                    (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_stream_info                     (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_stream_is_stopped               (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlaudio_stream_is_active                (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#define MIN(a,b) ((a) < (b) ? (a) : (b))

static ErlNifResourceType* ERLAUDIO_STREAM_RESOURCE = NULL;

static ErlNifFunc nif_funcs[] =
{
    {"get_pa_version",                  0, erlaudio_get_pa_version},
    // Native Host API stuff
    {"get_default_hostapi_index",       0, erlaudio_get_default_hostapi_index},
    {"get_device_index_from_hostapi",   2, erlaudio_get_device_index_from_hostapi},
    {"get_hostapi_count",               0, erlaudio_get_hostapi_count},
    {"get_hostapi_info",                1, erlaudio_get_hostapi_info},
    {"get_hostapi_index_from_type",     1, erlaudio_get_hostapi_index_from_type},
    // Devices
    {"get_default_input_device_index",  0, erlaudio_get_default_input_device_index},
    {"get_default_output_device_index", 0, erlaudio_get_default_output_device_index},
    {"get_device",                      1, erlaudio_get_device},
    {"get_device_count",                0, erlaudio_get_device_count},
    // Stream creation
    {"stream_format_supported",         3, erlaudio_stream_format_supported},
    {"stream_open",                     5, erlaudio_stream_open},
    // Stream management
    // TODO: CHECK OWNERSHIP FOR THE FOLLOWING FUNCTIONS:
    {"stream_start",                    1, erlaudio_stream_start},
    {"stream_owner",                    1, erlaudio_stream_owner},
    {"stream_owner",                    2, erlaudio_stream_owner},
    {"stream_reader",                   1, erlaudio_stream_reader},
    {"stream_reader",                   2, erlaudio_stream_reader},
    {"stream_close",                    1, erlaudio_stream_close},
    {"stream_stop",                     1, erlaudio_stream_stop},
    {"stream_abort",                    1, erlaudio_stream_abort},
    {"stream_write",                    2, erlaudio_stream_write},
    // READ ONLY BITS, NO OWNERSHIP CHECK NEEDED
    {"stream_info",                     1, erlaudio_stream_info},
    {"stream_is_stopped",               1, erlaudio_stream_is_stopped},
    {"stream_is_active",                1, erlaudio_stream_is_active}
};

static struct int_to_str pa_errors[] = {
    { paNotInitialized,                        "not_initialized" },
    { paNotInitialized,                        "not_initialized" },
    { paUnanticipatedHostError,                "unanticipated_host_error" },
    { paInvalidChannelCount,                   "invalid_number_of_channels" },
    { paInvalidSampleRate,                     "invalid_sample_rate" },
    { paInvalidDevice,                         "invalid_device" },
    { paInvalidFlag,                           "invalid_flag" },
    { paSampleFormatNotSupported,              "sample_format_unsupported" },
    { paBadIODeviceCombination,                "bad_device_combo" },
    { paInsufficientMemory,                    "insufficient_memory" },
    { paBufferTooBig,                          "buffer_too_big" },
    { paBufferTooSmall,                        "buffer_too_small" },
    { paNullCallback,                          "nocallback" },
    { paBadStreamPtr,                          "badcallback" },
    { paTimedOut,                              "timeout" },
    { paInternalError,                         "internal_error" },
    { paDeviceUnavailable,                     "device_unavailable" },
    { paIncompatibleHostApiSpecificStreamInfo, "incompatible_host_stream_info" },
    { paStreamIsStopped,                       "stream_stopped" },
    { paStreamIsNotStopped,                    "stream_not_stopped" },
    { paInputOverflowed,                       "input_overflowed" },
    { paOutputUnderflowed,                     "output_underflowed" },
    { paHostApiNotFound,                       "no_hostapi" },
    { paInvalidHostApi,                        "invalid_hostapi" },
    { paCanNotReadFromACallbackStream,         "noread_callback" },
    { paCanNotWriteToACallbackStream,          "nowrite_callback" },
    { paCanNotReadFromAnOutputOnlyStream,      "output_only" },
    { paCanNotWriteToAnInputOnlyStream,        "input_only" },
    { paIncompatibleStreamHostApi,             "incompatible_hostapi" },
    { paBadBufferPtr,                          "badbuffer" },
    { 0, 0 }
};

static struct int_to_str pa_drivers[] = {
    { paInDevelopment,     "indevelopment" },
    { paDirectSound,       "directsound" },
    { paMME,               "mme" },
    { paASIO,              "asio" },
    { paSoundManager,      "soundmanager" },
    { paCoreAudio,         "coreaudio" },
    { paOSS,               "oss" },
    { paALSA,              "alsa" },
    { paAL,                "al" },
    { paBeOS,              "beos" },
    { paWDMKS,             "wdmks" },
    { paJACK,              "jack" },
    { paWASAPI,            "wasapi" },
    { paAudioScienceHPI,   "audiosciencehpi" },
    { 0, 0 }
};
