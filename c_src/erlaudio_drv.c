#include "erl_nif.h"
#include "inttypes.h"
#include "string.h"
#include "stdint.h"
#include "stdio.h"
#include "portaudio.h"
#include "erlaudio_drv.h"
#include "assert.h"

static void erlaudio_ringbuf_init(struct erlaudio_ringbuf *buf, size_t length) {
    // printf("Initing for %zu\n", length);
    buf->length = length;
    buf->head = 0;
    buf->tail = 0;
    buf->data = enif_alloc(sizeof(char) * buf->length);
}

static void erlaudio_ringbuf_destroy(struct erlaudio_ringbuf *buf) {
    if(buf!=NULL && buf->length && buf->data != NULL) {
        enif_free(buf->data);
        buf->length = 0;
        buf->data = NULL;
    }
}

static size_t erlaudio_ringbuf_avail_space(struct erlaudio_ringbuf *buf) {
    if(buf->head > buf->tail)  return buf->head - buf->tail - 1;
    if(buf->head == buf->tail) return buf->length - 1;
    return buf->length - 1 - (buf->head - buf->tail);
}

static size_t erlaudio_ringbuf_avail_data(struct erlaudio_ringbuf *buf) {
    if(buf->tail > buf->head)  return buf->tail - buf->head;
    if(buf->head == buf->tail) return 0;
    return buf->length - (buf->head - buf->tail);
}

static void erlaudio_ringbuf_write(struct erlaudio_ringbuf *buf, unsigned char *data, int length) {
    assert(length <= erlaudio_ringbuf_avail_space(buf));
    size_t tail_write = 0;
    size_t tail = buf->tail;
    if(buf->tail + length > buf->length) {
        tail_write = buf->length - buf->tail;
        memcpy((void *) &buf->data[buf->tail], data, tail_write);
        tail = 0;
    }
    memcpy((void *) &buf->data[tail], &data[tail_write], length - tail_write);
    buf->tail = (buf->tail + length) % buf->length;
}

static struct erlaudio_stream_handle* erlaudio_stream_handle_alloc() {
    struct erlaudio_stream_handle* handle = enif_alloc_resource(
                                                ERLAUDIO_STREAM_RESOURCE,
                                                sizeof(struct erlaudio_stream_handle));
    handle->owner_pid  = (ErlNifPid *) enif_alloc(sizeof(ErlNifPid));
    handle->reader_pid = (ErlNifPid *) enif_alloc(sizeof(ErlNifPid));
    handle->input = NULL;
    handle->output = NULL;
    handle->pa = NULL;
    handle->output_buf = enif_alloc(sizeof(struct erlaudio_ringbuf));
    handle->output_buf->data = NULL;
    return handle;
}

static void erlaudio_stream_resource_cleanup(ErlNifEnv* env, void* arg) {
    struct erlaudio_stream_handle* handle = (struct erlaudio_stream_handle*) arg;
    // Stop, just in case, to cause thread to end.
    Pa_AbortStream(handle->pa);
    enif_thread_join(handle->thread, NULL);
    if(handle->pa) Pa_CloseStream(handle->pa);
    if(handle->input)  enif_free( handle->input);
    if(handle->output) enif_free(handle->output);
    if(handle->owner_pid)  enif_free( handle->owner_pid);
    if(handle->reader_pid) enif_free(handle->reader_pid);
    if(handle->output_buf) {
        erlaudio_ringbuf_destroy(handle->output_buf);
        enif_free(handle->output_buf);
    }
}

static int get_stream_handle(ErlNifEnv* env,
                            ERL_NIF_TERM item,
                            struct erlaudio_stream_handle** handle)
{
    return enif_get_resource(env, item, ERLAUDIO_STREAM_RESOURCE, (void **) handle);
}

/**
 * Return an error message from the list
 */
const char* pa_error_to_char(int err) {
    struct err_to_str* cur = &pa_errors[0];
    while(cur->str != 0) {
        if(cur->num == err) {
            return cur->str;
        }
        cur++;
    }
    return "invalid_error";
}

/**
 * Turn a PA call into an error if there's an error.
 */
static ERL_NIF_TERM pa_error_to_error_tuple(ErlNifEnv *env, int err) {
    return enif_make_tuple2(env,
        enif_make_atom(env, "error"),
        enif_make_atom(env, pa_error_to_char(err))
    );
}

/**
 * Possibly send a PA call to a mailbox as an error, return 0 on fatal error, 1 if good
 */
static int pa_error_message(void *resource, int err, ErlNifPid* pid) {
    if(err != paNoError) {
        ErlNifEnv* env = enif_alloc_env();
        enif_send(NULL, pid, env, enif_make_tuple3(env,
            enif_make_atom(env, "erlaudio_error"),
            enif_make_resource(env, resource),
            enif_make_atom(env, pa_error_to_char(err))
        ));
        enif_free_env(env);
    }
    return err == paNoError || err == paInputOverflowed || err == paOutputUnderflowed;
}

/**
 * Get sample size from atom
 */
static PaSampleFormat atom_to_sample_format(ErlNifEnv *env, ERL_NIF_TERM sample_atom) {
    struct int_to_str* cur = &pa_types[0];
    while(cur->num != 0) {
        if(enif_compare(sample_atom, enif_make_atom(env, cur->str))==0) {
            return cur->num;
        }
        cur++;
    }
    return -1;
}

/**
* Turn a stream flag atom into its value
*/
static int atom_to_stream_flags(ErlNifEnv *env, ERL_NIF_TERM atom) {
  struct sflags_to_str* cur = &pa_flags[0];
  while(cur->num != 0) {
    if(enif_compare(atom, enif_make_atom(env, cur->str))==0) {
      return cur->num;
    }
    cur++;
  }
  return -1;
}

/**
* Get stream flags from a list of atoms
*/
static int list_to_stream_flags(ErlNifEnv *env, ERL_NIF_TERM list, PaStreamFlags *flags) {
    *flags = paNoFlag;
    int flag = paNoFlag;
    unsigned length = 0;
    ERL_NIF_TERM atom;
    if(!enif_get_list_length(env, list, &length)) {
      return 0;
    }
    if(length==0) {
      return 1;
    }
    while(enif_get_list_cell(env, list, &atom, &list)) {
        flag = atom_to_stream_flags(env, atom);
        if(flag >= 0) {
            *flags |= flag;
        } else {
          return 0;
        }
    }
    return 1;
}

/**
 * Convert the stream params record (tuple) to PaStreamParameters
 */
static int
convert_tuple_to_stream_params(ErlNifEnv* env, ERL_NIF_TERM term, PaStreamParameters **stream_params) {
    int arity, device, channelCount;
    double suggestedLatency;
    const ERL_NIF_TERM *tuple;
    PaStreamParameters *params;
    if(enif_compare(term, enif_make_atom(env, "undefined"))==0
        || enif_compare(term, enif_make_atom(env, "none"))==0
        || enif_compare(term, enif_make_atom(env, "null"))==0)
    {
        return 1;
    }

    if(!enif_get_tuple(env, term, &arity, &tuple)
        || arity!=5
        || enif_compare(tuple[0], enif_make_atom(env, "erlaudio_device_params"))!=0
        || !enif_get_int(env,    tuple[1], &device)
        || !enif_get_int(env,    tuple[2], &channelCount)
        || !enif_is_atom(env,    tuple[3])
        || !enif_get_double(env, tuple[4], &suggestedLatency)
        || device < 0
        || device >= Pa_GetDeviceCount()
        || channelCount < 0
    ) {
        return 0;
    }
    params = enif_alloc(sizeof(PaStreamParameters));
    params->device = device;
    params->channelCount = channelCount;
    params->sampleFormat = atom_to_sample_format(env, tuple[3]);
    params->suggestedLatency = suggestedLatency;
    params->hostApiSpecificStreamInfo = NULL;
    *stream_params = params;
    return 1;
}

static int on_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info) {
    PaError err;
    // struct erlaudio_thread_mgr* thread_mgr;
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                     "ERLAUDIO_STREAM_RESOURCE",
                                                     &erlaudio_stream_resource_cleanup,
                                                     flags, NULL);
    if (rt == NULL) return -1;
    ERLAUDIO_STREAM_RESOURCE = rt;

    err = Pa_Initialize();
    if(err!=paNoError) {
        return err;
    }

    return 0;
}

static void on_unload(ErlNifEnv* env, void* priv_data) {
    Pa_Terminate();
}

ERL_NIF_INIT(erlaudio_drv, nif_funcs, &on_load, NULL, NULL, &on_unload);

static int erlaudio_thread_stream_input(struct erlaudio_stream_handle* h) {
    signed long frames_available;
    signed long bytes_available;
    ErlNifEnv* env;
    int err;
    ErlNifBinary input_bin;

    if(!h->input) {
        return 1;
    }

    if ((frames_available = Pa_GetStreamReadAvailable(h->pa)) > 0) {
        bytes_available = frames_available * h->input_frame_size;
        if(!enif_alloc_binary(bytes_available, &input_bin)) {
            return -1;
        }
        err = Pa_ReadStream(h->pa, input_bin.data, frames_available);
        if(err==paNoError) {
            env = enif_alloc_env();
            enif_send(NULL, h->reader_pid, env, enif_make_tuple3(env,
                enif_make_atom(env, "erlaudio_pcmdata"),
                enif_make_resource(env, h),
                enif_make_binary(env, &input_bin)
            ));
            enif_release_binary(&input_bin);
            enif_free_env(env);
        } else {
            enif_release_binary(&input_bin);
            // printf("\nRead Error %d\n", err);
            // Tell the reader we've had an error
            pa_error_message(h, err, h->reader_pid);
            // Tell the owner if the owner is someone else
            if(h->reader_pid != h->owner_pid) {
                pa_error_message(h, err, h->owner_pid);
            }
            return -1;
        }
    }
    if(frames_available < 0) {
        pa_error_message(h, frames_available, h->owner_pid);
        return -1;
    }
    return 1;
}

static int erlaudio_thread_stream_output(struct erlaudio_stream_handle* h) {
    signed long frames_available;
    signed long bytes_available;
    size_t bytes_to_write_available;
    struct erlaudio_ringbuf* buf = h->output_buf;
    size_t head_read = 0;
    size_t head = buf->head;
    int err;

    if(!h->output) {
        return 1;
    }

    frames_available = Pa_GetStreamWriteAvailable(h->pa);
    if(frames_available < 0) {
        pa_error_message(h, frames_available, h->owner_pid);
        return -1;
    }
    bytes_to_write_available = erlaudio_ringbuf_avail_data(buf);
    while(bytes_to_write_available > 0 && frames_available > 0) {
        bytes_available = frames_available * h->output_frame_size;

        bytes_to_write_available = bytes_to_write_available > bytes_available ? bytes_available : bytes_to_write_available;

        head_read = 0;
        head = buf->head;
        if(buf->head + bytes_to_write_available > buf->length) {
            head_read = buf->length - buf->head;
            err = Pa_WriteStream(h->pa, &buf->data[head], head_read / h->output_frame_size);
            assert(head_read % h->output_frame_size == 0);
            if(err!=paNoError) {
                pa_error_message(h, err, h->owner_pid);
                return -1;
            }
            head = 0;
        }
        err = Pa_WriteStream(h->pa, &buf->data[head], (bytes_to_write_available - head_read) / h->output_frame_size);
        if(err!=paNoError) {
            pa_error_message(h, err, h->owner_pid);
            return -1;
        }
        buf->head = (buf->head + bytes_to_write_available) % buf->length;
        
        bytes_to_write_available = erlaudio_ringbuf_avail_data(buf);
        frames_available = Pa_GetStreamWriteAvailable(h->pa);
    }
    return 1;
}

/**
 * The callback of the main loop of the processing thread
 */
static int erlaudio_thread_stream_exec(struct erlaudio_stream_handle* h) {

    if(erlaudio_thread_stream_input(h)!=1
        || erlaudio_thread_stream_output(h)!=1
        || erlaudio_thread_stream_input(h)!=1) {
        return -1;
    }
    return 1;

}

/**
 * The main thread loop
 */
static void* erlaudio_thread_stream_main(void* data) {
    struct erlaudio_stream_handle* h = (struct erlaudio_stream_handle*) data;
    // const struct PaStreamInfo *info = Pa_GetStreamInfo(h->pa);
    PaTime to_sleep;
    int loop = 1;
    int err;
    PaTime time1, time2;
    to_sleep = 10;

    err = Pa_StartStream(h->pa);
    if(err!=paNoError) {
      return NULL;
    }
    assert(1==Pa_IsStreamActive(h->pa));
    while(loop && h->pa!=NULL && Pa_IsStreamActive(h->pa)) {
        time1 = Pa_GetStreamTime(h->pa);
        loop = erlaudio_thread_stream_exec(h) > 0;
        time2 = Pa_GetStreamTime(h->pa);
        Pa_Sleep(5);
    }
    return NULL;
}

static int erlaudio_thread_start(struct erlaudio_stream_handle* handle) {
  return enif_thread_create("erlaudio_thread", &handle->thread,
              erlaudio_thread_stream_main, handle, NULL);
}

/**
 * Get the portaudio version
 */
static ERL_NIF_TERM erlaudio_get_pa_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    const char* version = Pa_GetVersionText();
    ERL_NIF_TERM binary_term;
    char* name_raw = (char *) enif_make_new_binary(env, strlen(version), &binary_term);
    strcpy(name_raw, version);
    return enif_make_tuple2(env,
        enif_make_int(env, Pa_GetVersion()),
        binary_term
    );
}

/**
 * Get the hostapi info record/tuple
 */
static ERL_NIF_TERM erlaudio_get_hostapi_info(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    const PaHostApiInfo* info;
    int index;
    if(argc!=1 || !enif_get_int(env, argv[0], &index)) {
        return enif_make_badarg(env);
    }
    info = Pa_GetHostApiInfo(index);

    ERL_NIF_TERM name;
    char *name_raw = (char *) enif_make_new_binary(env, strlen(info->name), &name);
    strcpy(name_raw, info->name);

    return enif_make_tuple6(env,
        enif_make_atom(env, "erlaudio_hostapi_info"),
        enif_make_int(env, info->type),
        name,
        enif_make_int(env, info->deviceCount),
        enif_make_int(env, info->defaultInputDevice),
        enif_make_int(env, info->defaultOutputDevice)
    );
}

/**
 * Get the default hostapi index
 */
static ERL_NIF_TERM erlaudio_get_default_hostapi_index(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_int(env, Pa_GetDefaultHostApi());
}

/**
 * Get number of hostapi's available
 */
static ERL_NIF_TERM erlaudio_get_hostapi_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_int(env, Pa_GetHostApiCount());
}

/**
 * Get hostapi index from the type
 */
static ERL_NIF_TERM erlaudio_get_hostapi_index_from_type(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int type = -1;
    if(argc!=1) {
        return enif_make_badarg(env);
    }
    if(enif_is_atom(env, argv[0])) {
        struct api_to_str cur = pa_drivers[0];
        while(cur.str != 0) {
            if(enif_compare(enif_make_atom(env, cur.str), argv[0])==0)
                return enif_make_int(env, Pa_HostApiTypeIdToHostApiIndex(cur.num));
        }
        return enif_make_badarg(env);
    }
    if(enif_get_int(env, argv[0], &type)) {
        return enif_make_int(env, Pa_HostApiTypeIdToHostApiIndex(type));
    }
    return enif_make_badarg(env);
}

/**
 * Get device index from the hostapi index and hostapi device index
 */
static ERL_NIF_TERM erlaudio_get_device_index_from_hostapi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int hostapi_device_index;
    int hostapi_index;
    if(argc!=2
        || enif_get_int(env, argv[0], &hostapi_index)
        || enif_get_int(env, argv[1], &hostapi_device_index))
    {
        return enif_make_badarg(env);
    }
    return enif_make_int(env, Pa_HostApiDeviceIndexToDeviceIndex(hostapi_index, hostapi_device_index));
}

/**
 * Get default input device index
 */
static ERL_NIF_TERM erlaudio_get_default_input_device_index(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_int(env, Pa_GetDefaultInputDevice());
}

/**
 * Get default output device index
 */
static ERL_NIF_TERM erlaudio_get_default_output_device_index(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_int(env, Pa_GetDefaultOutputDevice());
}

/**
 * Get a record of the device information
 */
static ERL_NIF_TERM erlaudio_get_device(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned int i;
    const PaDeviceInfo *deviceInfo;
    if(argc != 1
            || !enif_get_uint(env, argv[0], &i)
            || i >= Pa_GetDeviceCount()) {
        return enif_make_badarg(env);
    }
    deviceInfo = Pa_GetDeviceInfo(i);

    ERL_NIF_TERM name;
    char *name_raw = (char *) enif_make_new_binary(env, strlen(deviceInfo->name), &name);
    strcpy(name_raw, deviceInfo->name);

    const ERL_NIF_TERM fields[11] = {
        enif_make_atom(env, "erlaudio_device"),
        enif_make_int(env, i),
        name,
        enif_make_int(env, deviceInfo->hostApi),
        enif_make_int(env, deviceInfo->maxInputChannels),
        enif_make_int(env, deviceInfo->maxOutputChannels),
        enif_make_double(env, deviceInfo->defaultLowInputLatency),
        enif_make_double(env, deviceInfo->defaultLowOutputLatency),
        enif_make_double(env, deviceInfo->defaultHighInputLatency),
        enif_make_double(env, deviceInfo->defaultHighOutputLatency),
        enif_make_double(env, deviceInfo->defaultSampleRate)
    };
    return enif_make_tuple_from_array(env, fields, 11);
}

/**
 * Get the total number of devices
 */
static ERL_NIF_TERM erlaudio_get_device_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int numDevices;
    numDevices = Pa_GetDeviceCount();
    if(numDevices < 0) {
        return pa_error_to_error_tuple(env, numDevices);
    }
    return enif_make_int(env, numDevices);
}

/**
 * Check if the format settings are supported
 */
static ERL_NIF_TERM erlaudio_stream_format_supported(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int err;
    PaStreamParameters *input  = NULL;
    PaStreamParameters *output = NULL;
    double sample_rate;
    if(argc!=3
        || !convert_tuple_to_stream_params(env, argv[0], &input)
        || !convert_tuple_to_stream_params(env, argv[1], &output)
        || !enif_get_double(env,   argv[2], &sample_rate)
    ) {
        if(input!=NULL)  enif_free(input);
        if(output!=NULL) enif_free(output);
        return enif_make_badarg(env);
    }

    err = Pa_IsFormatSupported(input, output, sample_rate);

    if(input!=NULL)  enif_free(input);
    if(output!=NULL) enif_free(output);
    if(err!=paFormatIsSupported) {
        return pa_error_to_error_tuple(env, err);
    }
    return enif_make_atom(env, "ok");
}

/**
 * Open a stream resource
 */
static ERL_NIF_TERM erlaudio_stream_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned long flags;
    int err;
    int ret;
    struct erlaudio_stream_handle* handle = erlaudio_stream_handle_alloc();
    if(argc!=5
        || !convert_tuple_to_stream_params(env, argv[0], &handle->input)
        || !convert_tuple_to_stream_params(env, argv[1], &handle->output)
        || !enif_get_double(env, argv[2], &handle->sample_rate)
        || !enif_get_ulong (env, argv[3], &handle->frames_per_buffer)
        || !list_to_stream_flags(env, argv[4], &flags)
    ) {
        // Gives ownership back to Erlang-land.
        enif_release_resource(handle);
        return enif_make_badarg(env);
    }
    if(handle->input!=NULL) {
        handle->input_sample_size = Pa_GetSampleSize(handle->input->sampleFormat);
        handle->input_frame_size = handle->input_sample_size * handle->input->channelCount;
    } else {
        handle->input_sample_size = 0;
        handle->input_frame_size = 0;
    }
    if(handle->output!=NULL) {
        handle->output_sample_size = Pa_GetSampleSize(handle->output->sampleFormat);
        handle->output_frame_size = handle->output_sample_size * handle->output->channelCount;
        // 3 seconds seems like a reasonable time for the size of the buffer, no?
        erlaudio_ringbuf_init(handle->output_buf, handle->output_frame_size * handle->sample_rate * 3);
        memset(handle->output_buf->data, 0, handle->frames_per_buffer*handle->output_frame_size);
        handle->output_buf->tail = handle->frames_per_buffer*handle->output_frame_size;
      } else {
        handle->output_sample_size = 0;
        handle->output_frame_size = 0;
    }

    err = Pa_OpenStream(&handle->pa, handle->input, handle->output,
                        handle->sample_rate, handle->frames_per_buffer, flags,
                        NULL, NULL);
    handle->mgr = (struct erlaudio_thread_mgr *) enif_priv_data(env);
    if(err!=paNoError) {
        // Gives ownership back to Erlang-land.
        enif_release_resource(handle);
        return pa_error_to_error_tuple(env, err);
    }
    enif_self(env, handle->owner_pid);
    enif_self(env, handle->reader_pid);
    ret = enif_make_tuple2(
        env,
        enif_make_atom(env, "ok"),
        enif_make_resource(env, handle)
    );
    // Gives ownership back to Erlang-land.
    enif_release_resource(handle);
    return ret;
}

/**
 * Start a stream
 */
static ERL_NIF_TERM erlaudio_stream_start(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int err;
    struct erlaudio_stream_handle* handle;
    if(argc != 1 || !get_stream_handle(env, argv[0], &handle)){
        return enif_make_badarg(env);
    }

    err = erlaudio_thread_start(handle);
    if(err!=0) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_tuple2(env,
                enif_make_atom(env, "threadfailed"),
                enif_make_int(env, err)
            )
        );
    }

    return enif_make_atom(env, "ok");
}

/**
 * Get/Set the stream owner's pid
 */
static ERL_NIF_TERM erlaudio_stream_owner(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    struct erlaudio_stream_handle* handle;
    if(argc < 1 || argc > 2 || !get_stream_handle(env, argv[0], &handle)) {
        return enif_make_badarg(env);
    }
    if(argc==1) {
        return enif_make_pid(env, handle->owner_pid);
    }
    if(!enif_get_local_pid(env, argv[1], handle->owner_pid)) {
        return enif_make_badarg(env);
    }
    return enif_make_atom(env, "ok");
}

/**
 * Set the reader's pid
 */
static ERL_NIF_TERM erlaudio_stream_reader(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    struct erlaudio_stream_handle* handle;
    if(argc < 1 || argc > 2 || !get_stream_handle(env, argv[0], &handle)) {
        return enif_make_badarg(env);
    }
    if(argc==1) {
        return enif_make_pid(env, handle->reader_pid);
    }
    if(!enif_get_local_pid(env, argv[1], handle->reader_pid)) {
        return enif_make_badarg(env);
    }
    return enif_make_atom(env, "ok");
}

/**
 * Stop the stream, semi-immediate, doesn't wait for buffers to flush
 */
static ERL_NIF_TERM erlaudio_stream_stop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int err;
    struct erlaudio_stream_handle* handle;
    if(argc != 1 || !get_stream_handle(env, argv[0], &handle)) {
        return enif_make_badarg(env);
    }
    err = Pa_StopStream(handle->pa);
    if(err!=paNoError) {
        return pa_error_to_error_tuple(env, err);
    }
    return enif_make_atom(env, "ok");

}

/**
 * Close the stream, returns immediately and doesn't wait for buffers to flush.
 */
static ERL_NIF_TERM erlaudio_stream_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int err;
    struct erlaudio_stream_handle* handle;
    if(argc != 1 || !get_stream_handle(env, argv[0], &handle)) {
      return enif_make_badarg(env);
    }
    //err = Pa_CloseStream(handle->pa);
    err = Pa_AbortStream(handle->pa);
    if(err!=paNoError) {
      return pa_error_to_error_tuple(env, err);
    }
    return enif_make_atom(env, "ok");
}

/**
 * Practically an emergency stop kind of thing. Only exposed because the API exposes it.
 */
static ERL_NIF_TERM erlaudio_stream_abort(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int err;
    struct erlaudio_stream_handle* handle;
    if(argc != 1 || !get_stream_handle(env, argv[0], &handle)) {
        return enif_make_badarg(env);
    }
    err = Pa_AbortStream(handle->pa);
    if(err!=paNoError) {
        return pa_error_to_error_tuple(env, err);
    }
    return enif_make_atom(env, "ok");
}

/**
 * Write stream
 */
static ERL_NIF_TERM erlaudio_stream_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    struct erlaudio_stream_handle* h;
    struct erlaudio_ringbuf* buf;
    ErlNifBinary bin;
    // printf("WRITE DATA!\n");
    if(argc != 2
        || !get_stream_handle(env, argv[0], &h)
        || !enif_inspect_iolist_as_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }
    buf = h->output_buf;
    // Must be evenly divisible by the frame size (sample format size * output channels)
    if(bin.size % h->output_frame_size != 0) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "badbinsize")
        );
    }
    // printf("Space avail? %zu %zu\n", erlaudio_ringbuf_avail_space(buf), bin.size);
    if(erlaudio_ringbuf_avail_space(buf) < bin.size) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "toobig")
        );
    }
    erlaudio_ringbuf_write(buf, bin.data, bin.size);
    return enif_make_atom(env, "ok");
}

/**
 * Get stream info
 */
static ERL_NIF_TERM erlaudio_stream_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    struct erlaudio_stream_handle* handle;
    const PaStreamInfo *info;
    if(argc != 1 || !get_stream_handle(env, argv[0], &handle)) {
        return enif_make_badarg(env);
    }
    info = Pa_GetStreamInfo(handle->pa);
    return enif_make_tuple4(env,
        enif_make_atom(env, "erlaudio_stream_info"),
        enif_make_double(env, info->inputLatency),
        enif_make_double(env, info->outputLatency),
        enif_make_double(env, info->sampleRate)
    );
}

/**
 * Is stopped
 */
static ERL_NIF_TERM erlaudio_stream_is_stopped(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    struct erlaudio_stream_handle* handle;
    if(argc != 1 || !get_stream_handle(env, argv[0], &handle)) {
        return enif_make_badarg(env);
    }
    PaError ret = Pa_IsStreamStopped(handle->pa);
    if(ret==1) {
      return enif_make_atom(env, "false");
    } else if(ret==paNoError) {
      return enif_make_atom(env, "true");
    } else {
      return pa_error_to_error_tuple(env, ret);
    }
}

/**
 * Stream is active
 */
static ERL_NIF_TERM erlaudio_stream_is_active(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    struct erlaudio_stream_handle* handle;
    if(argc != 1 || !get_stream_handle(env, argv[0], &handle)) {
        return enif_make_badarg(env);
    }
    PaError ret = Pa_IsStreamActive(handle->pa);
    if(ret==1) {
      return enif_make_atom(env, "false");
    } else if(ret==paNoError) {
      return enif_make_atom(env, "true");
    } else {
      return pa_error_to_error_tuple(env, ret);
    }
}
