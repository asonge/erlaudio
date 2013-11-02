#include "erl_nif.h"
#include "inttypes.h"
#include "string.h"
#include "stdint.h"
#include "stdio.h"
#include "portaudio.h"
#include "erlaudio_drv.h"

static void erlaudio_binqueue_alloc(struct erlaudio_binqueue* buf, size_t size) {
    buf->bins = enif_alloc(sizeof(ErlNifBinary **) * size);
    buf->size = size;
    buf->head = 0;
    buf->tail = 0;
    buf->head_cursor = 0;
}

static void erlaudio_binqueue_free(struct erlaudio_binqueue* buf) {
    enif_free(buf->bins);
}

static size_t erlaudio_binqueue_size(struct erlaudio_binqueue* buf) {
    return buf->tail >= buf->head ?
        buf->tail - buf->head :
        buf->size - buf->head + buf->tail;
}

static struct erlaudio_stream_handle* erlaudio_stream_handle_alloc() {
    struct erlaudio_stream_handle* handle = enif_alloc_resource(
                                                ERLAUDIO_STREAM_RESOURCE,
                                                sizeof(struct erlaudio_stream_handle));
    handle->owner_pid  = (ErlNifPid *) enif_alloc(sizeof(ErlNifPid));
    handle->reader_pid = (ErlNifPid *) enif_alloc(sizeof(ErlNifPid));
    erlaudio_binqueue_alloc(&handle->output_queue, 10);

    return handle;
}

static void erlaudio_stream_handle_free(struct erlaudio_stream_handle* handle) {

    // if(handle->input_buffer.size) erlaudio_circbuffer_free(&handle->input_buffer);
    if(handle->pa) Pa_CloseStream(handle->pa);
    if(handle->owner_pid)  enif_free(handle->owner_pid);
    if(handle->reader_pid) enif_free(handle->reader_pid);
    erlaudio_binqueue_free(&handle->output_queue);

    return enif_free(handle);
}

static void erlaudio_stream_resource_cleanup(ErlNifEnv* env, void* arg)
{
    struct erlaudio_stream_handle* handle = (struct erlaudio_stream_handle*) arg;
    erlaudio_stream_handle_free(handle);
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
const char*
pa_error_to_char(int err)
{
    struct int_to_str cur = pa_errors[0];
    while(cur.str != 0) {
        if(cur.num == err)
            return cur.str;
    }
    return "invalid_error";
}

/**
 * Turn a PA call into an error if there's an error.
 */
static ERL_NIF_TERM
pa_error_to_error_tuple(ErlNifEnv *env, int err)
{
    return enif_make_tuple2(env,
        enif_make_atom(env, "error"),
        enif_make_atom(env, pa_error_to_char(err))
    );
}

/**
 * Possibly send a PA call to a mailbox as an error
 */
static void pa_error_message(void *resource, int err, ErlNifPid* pid) {
    if(err != paNoError) {
        ErlNifEnv* env = enif_alloc_env();
        enif_send(NULL, pid, env, enif_make_tuple3(env,
            enif_make_atom(env, "erlaudio_error"),
            enif_make_resource(env, resource),
            enif_make_atom(env, pa_error_to_char(err))
        ));
        enif_free_env(env);
    }
}

/**
 * Get sample size from atom
 */
static PaSampleFormat atom_to_sample_format(ErlNifEnv *env, ERL_NIF_TERM sample_atom)
{
    if(enif_compare(sample_atom, enif_make_atom(env, "float32"))==0) return paFloat32;
    if(enif_compare(sample_atom, enif_make_atom(env, "int32"))==0)   return paInt32;
    if(enif_compare(sample_atom, enif_make_atom(env, "int24"))==0)   return paInt24;
    if(enif_compare(sample_atom, enif_make_atom(env, "int16"))==0)   return paInt16;
    if(enif_compare(sample_atom, enif_make_atom(env, "int8"))==0)    return paInt8;
    if(enif_compare(sample_atom, enif_make_atom(env, "uint8"))==0)   return paUInt8;
    return -1;
}

/**
 * Convert the stream params record (tuple) to PaStreamParameters
 */
static int
convert_tuple_to_stream_params(ErlNifEnv* env, ERL_NIF_TERM term, PaStreamParameters **stream_params)
{
    int arity;
    PaStreamParameters *params = *stream_params;
    const ERL_NIF_TERM *tuple;

    if(enif_compare(term, enif_make_atom(env, "undefined"))==0
        || enif_compare(term, enif_make_atom(env, "none"))==0
        || enif_compare(term, enif_make_atom(env, "null"))==0)
    {
        enif_free(*stream_params);
        *stream_params = NULL;
        return 1;
    }

    if(!enif_get_tuple(env, term, &arity, &tuple)
        || arity!=5
        || enif_compare(tuple[0], enif_make_atom(env, "device_params"))!=0
        || !enif_get_int(env,    tuple[1], &params->device)
        || !enif_get_int(env,    tuple[2], &params->channelCount)
        || !enif_is_atom(env,    tuple[3])
        || !enif_get_double(env, tuple[4], &params->suggestedLatency)
        || params->device < 0
        || params->device >= Pa_GetDeviceCount()
        || params->channelCount < 0
    ) {
        return 0;
    }
    params->sampleFormat = atom_to_sample_format(env, tuple[3]);
    params->hostApiSpecificStreamInfo = NULL;
    return 1;
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                     "ERLAUDIO_STREAM_RESOURCE",
                                                     &erlaudio_stream_resource_cleanup,
                                                     flags, NULL);
    if (rt == NULL)
        return -1;

    ERLAUDIO_STREAM_RESOURCE = rt;
    return Pa_Initialize();
}

ERL_NIF_INIT(erlaudio_drv, nif_funcs, &on_load, NULL, NULL, NULL);

/**
 * The callback of the main loop of the processing thread
 */
static int erlaudio_thread_stream_exec(struct erlaudio_stream_handle* h)
{
    signed long frames_available;
    signed long bytes_available;
    ErlNifEnv* env;
    int err;
    if(h->input && (frames_available = Pa_GetStreamReadAvailable(h->pa)) > 0) {
        // If input is enabled, check for new input capture
        ErlNifBinary input_bin;
        bytes_available = frames_available * h->input_frame_size;
        if(!enif_alloc_binary(bytes_available, &input_bin)) {
            // We have to alloc a new thing for the thing.
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
            enif_free_env(env);
        } else {
            // Tell the reader we've had an error
            pa_error_message(h, err, h->reader_pid);
            // Tell the owner if the owner is someone else
            if(h->reader_pid != h->owner_pid) {
                pa_error_message(h, err, h->owner_pid);
            }
        }
        enif_release_binary(&input_bin);
    }
    if(h->output) {
        // Make sure we have data to write as well)
        while(erlaudio_binqueue_size(&h->output_queue) > 0 && (frames_available = Pa_GetStreamWriteAvailable(h->pa)) > 0) {
            struct erlaudio_binqueue* binq = &h->output_queue;
            ErlNifBinary *bin = binq->bins[binq->head];
            bytes_available = frames_available * h->output_frame_size;
            if(bytes_available > bin->size - binq->head_cursor) {
                // More than enough data in the current binary to read bytes_available
                err = Pa_WriteStream(h->pa, &bin->data[binq->head_cursor], frames_available);
                pa_error_message(h, err, h->owner_pid); // This checks for error condition before sending anyway.
                binq->head_cursor += bytes_available; // We've written bytes
            } else {
                // Read all the data left in the current binary
                size_t bytes_to_write = (bin->size - binq->head_cursor);
                size_t frames_to_write = bytes_to_write / h->output_frame_size;
                err = Pa_WriteStream(h->pa, &bin->data[binq->head_cursor], frames_to_write);
                pa_error_message(h, err, h->owner_pid); // This checks for error condition before sending anyway.
                // Reset head_cursor, clean things up.
                binq->head_cursor = 0;
                binq->head = (binq->head + 1) % binq->size;
                enif_release_binary(bin);
            }
        }
        if(frames_available < 0) {
            pa_error_message(h, frames_available, h->owner_pid);
        }
    }
    return 1;
}

/**
 * The main thread loop
 */
static void* erlaudio_thread_stream_main(void* data)
{
    struct erlaudio_stream_handle* h = (struct erlaudio_stream_handle*) data;
    while(erlaudio_thread_stream_exec(h) > 0) {
        Pa_Sleep(50);
    };
    return NULL;
}

/**
 * Get the portaudio version
 */
static ERL_NIF_TERM erlaudio_get_pa_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const char *version = Pa_GetVersionText();
    ERL_NIF_TERM binary_term;
    char *name_raw = (char *) enif_make_new_binary(env, strlen(version), &binary_term);
    strcpy(name_raw, version);
    return enif_make_tuple2(env, enif_make_int(env, Pa_GetVersion()), binary_term);
}

/**
 * Get the hostapi info record/tuple
 */
static ERL_NIF_TERM erlaudio_get_hostapi_info(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    const PaHostApiInfo* info;
    int index;
    if(argc!=1
        || enif_get_int(env, argv[0], &index))
    {
        return enif_make_badarg(env);
    }
    
    info = Pa_GetHostApiInfo(index);

    ERL_NIF_TERM name;
    char *name_raw = (char *) enif_make_new_binary(env, strlen(info->name), &name);
    strcpy(name_raw, info->name);
    
    return enif_make_tuple6(env,
        enif_make_atom(env, "hostapi_info"),
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
static ERL_NIF_TERM erlaudio_get_default_hostapi_index(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, Pa_GetDefaultHostApi());
}

/**
 * Get number of hostapi's available
 */
static ERL_NIF_TERM erlaudio_get_hostapi_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, Pa_GetHostApiCount());
}

/**
 * Get hostapi index from the type
 */
static ERL_NIF_TERM erlaudio_get_hostapi_index_from_type(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type = -1;
    if(argc!=1) {
        return enif_make_badarg(env);
    }
    if(enif_is_atom(env, argv[0])) {
        struct int_to_str cur = pa_drivers[0];
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
static ERL_NIF_TERM erlaudio_get_device_index_from_hostapi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
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
static ERL_NIF_TERM erlaudio_get_default_input_device_index(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int device;
    device = Pa_GetDefaultInputDevice();
    return enif_make_int(env, device);
}

/**
 * Get default output device index
 */
static ERL_NIF_TERM erlaudio_get_default_output_device_index(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int device;
    device = Pa_GetDefaultOutputDevice();
    return enif_make_int(env, device);
}

/**
 * Get a record of the device information
 */
static ERL_NIF_TERM erlaudio_get_device(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
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
static ERL_NIF_TERM erlaudio_get_device_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int numDevices;
    numDevices = Pa_GetDeviceCount();
    // TODO: detect error
    return enif_make_int(env, numDevices);
}

/**
 * Check if the format settings are supported
 */
static ERL_NIF_TERM erlaudio_stream_format_supported(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int err;
    const PaStreamParameters *input;
    const PaStreamParameters *output;
    double sample_rate;
    // struct erlaudio_state* state = get_state(env);
    if(argc!=3
        || !convert_tuple_to_stream_params(env, argv[0], (PaStreamParameters **) &input)
        || !convert_tuple_to_stream_params(env, argv[1], (PaStreamParameters **) &output)
        || !enif_get_double(env,   argv[2], &sample_rate)
        // I know it's confusing, but inside the !, we must assert things we wish to be true to not fall into the error case
        || !(input ==NULL || input->channelCount  < Pa_GetDeviceInfo(input->device) ->maxInputChannels )
        || !(output==NULL || output->channelCount < Pa_GetDeviceInfo(output->device)->maxOutputChannels)
    ) {
        return enif_make_badarg(env);
    }
    err = Pa_IsFormatSupported(input, output, sample_rate);
    if(err!=paFormatIsSupported) {
        return pa_error_to_error_tuple(env, err);
    }
    return enif_make_atom(env, "ok");
}

/**
 * Open a stream resource
 */
static ERL_NIF_TERM erlaudio_stream_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    unsigned long flags;
    int err;
    int ret;
    // struct erlaudio_state* state = get_state(env);

    struct erlaudio_stream_handle* handle = erlaudio_stream_handle_alloc();
    // TODO: STAILQ_INIT(&handle->write_buffers);
    // handle->last_err = paNoError;
    // TODO: clean this up in the _free(); for the resource!

    if(argc!=6
        || !convert_tuple_to_stream_params(env, argv[0], &handle->input)
        || !convert_tuple_to_stream_params(env, argv[1], &handle->output)
        || !enif_get_double(env,   argv[2], &handle->sample_rate)
        || !enif_get_ulong(env, argv[3], &handle->frames_per_buffer)
        || !enif_get_ulong(env, argv[4], &flags)
    ) {
        fprintf(stderr, "%d %d %d %d %d\r\n",
            !convert_tuple_to_stream_params(env, argv[0], &handle->input),
            !convert_tuple_to_stream_params(env, argv[1], &handle->output),
            !enif_get_double(env,   argv[2], &handle->sample_rate),
            !enif_get_ulong(env, argv[3], &handle->frames_per_buffer),
            !enif_get_ulong(env, argv[4], &flags)
        );
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
        // TODO: alloc the binq here instead of always.
    } else {
        handle->output_sample_size = 0;
        handle->output_frame_size = 0;
    }
    
    err = Pa_OpenStream(&handle->pa, handle->input, handle->output,
                        handle->sample_rate, handle->frames_per_buffer, flags,
                        NULL, NULL);
    if(err!=paNoError) {
        return pa_error_to_error_tuple(env, err);
    }

    enif_self(env, handle->owner_pid);
    enif_self(env, handle->reader_pid);

    // TODO: write the code that adds in the ErlNifBinary.

    ret = enif_make_tuple2(
        env,
        enif_make_atom(env, "ok"),
        enif_make_resource(env, handle)
    );
    enif_release_resource(handle);
    return ret;
    // */
}

/**
 * Start a stream
 */
static ERL_NIF_TERM erlaudio_stream_start(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int err;
    // struct erlaudio_state* state = get_state(env);
    struct erlaudio_stream_handle* handle;

    if(argc != 1 || !get_stream_handle(env, argv[0], &handle))
    {
        return enif_make_badarg(env);
    }

    handle->thread_opts = enif_thread_opts_create("erlaudio_thread_opts");
    err = enif_thread_create("erlaudio_thread", &handle->thread,
                erlaudio_thread_stream_main, handle, handle->thread_opts);
    if(err!=0)
    {
        // todo printf
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_tuple2(env,
                enif_make_atom(env, "threadfailed"),
                enif_make_int(env, err)
            )
        );
    }

    Pa_StartStream(handle->pa);
    if(err!=paNoError) {
        return pa_error_to_error_tuple(env, err);
    }
    return enif_make_atom(env, "ok");
}

/**
 * Get/Set the stream owner's pid
 */
static ERL_NIF_TERM erlaudio_stream_owner(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // struct erlaudio_state* state = get_state(env);
    struct erlaudio_stream_handle* handle;
    if(argc < 1 || argc > 2
        || !enif_get_resource(env, argv[0], ERLAUDIO_STREAM_RESOURCE, (void **) &handle))
    {
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
static ERL_NIF_TERM erlaudio_stream_reader(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // struct erlaudio_state* state = get_state(env);
    struct erlaudio_stream_handle* handle;
    if(argc < 1 || argc > 2
        || !enif_get_resource(env, argv[0], ERLAUDIO_STREAM_RESOURCE, (void **) &handle))
    {
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
static ERL_NIF_TERM erlaudio_stream_stop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int err;
    // struct erlaudio_state* state = get_state(env);
    struct erlaudio_stream_handle* handle;
    if(argc != 1 || !enif_get_resource(env, argv[0], ERLAUDIO_STREAM_RESOURCE, (void **) &handle)) {
        return enif_make_badarg(env);
    }
    err = Pa_StopStream(handle->pa);
    if(err!=paNoError) {
        return pa_error_to_error_tuple(env, err);
    }
    return enif_make_atom(env, "ok");
    
}

/**
 * Close the stream, returns rather later, needs to send a message when destroyed.
 * Waits for buffers to flush.
 */
static ERL_NIF_TERM erlaudio_stream_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // MAKE ASYNC LATER.
    return erlaudio_stream_stop(env, argc, argv);
}

/**
 * Practically an emergency stop kind of thing. Only exposed because the API exposes it.
 */
static ERL_NIF_TERM erlaudio_stream_abort(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int err;
    // struct erlaudio_state* state = get_state(env);
    struct erlaudio_stream_handle* handle;
    if(argc != 1 || !enif_get_resource(env, argv[0], ERLAUDIO_STREAM_RESOURCE, (void **) &handle)) {
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
static ERL_NIF_TERM erlaudio_stream_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct erlaudio_stream_handle* h;
    ErlNifBinary* bin = enif_alloc(sizeof(ErlNifBinary));
    if(argc != 2
        || !enif_get_resource(env, argv[0], ERLAUDIO_STREAM_RESOURCE, (void **) &h)
        || !enif_inspect_iolist_as_binary(env, argv[1], bin)) {
        return enif_make_badarg(env);
    }
    // Must be evenly divisible by the frame size (sample format size * output channels)
    if(bin->size % h->output_frame_size != 0) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "badbinsize")
        );
    }
    // Must have room in the queue
    if(erlaudio_binqueue_size(&h->output_queue) < h->output_queue.size - 1) {
        size_t newtail = h->output_queue.tail + 1 == h->output_queue.size ?
            0 :
            h->output_queue.tail + 1;
        h->output_queue.bins[newtail] = bin;
        h->output_queue.tail = newtail;
        return enif_make_atom(env, "ok");
    } else {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "fullbinqueue")
        );
    }
    
}

/**
 * Get stream info
 */
static ERL_NIF_TERM erlaudio_stream_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // struct state* state = get_state(env);
    struct erlaudio_stream_handle* handle;
    const PaStreamInfo *info;
    if(argc != 1 || !enif_get_resource(env, argv[0], ERLAUDIO_STREAM_RESOURCE, (void **) &handle)) {
        return enif_make_badarg(env);
    }
    info = Pa_GetStreamInfo(handle->pa);
    return enif_make_tuple4(env,
        enif_make_atom(env, "stream_info"),
        enif_make_double(env, info->inputLatency),
        enif_make_double(env, info->outputLatency),
        enif_make_double(env, info->sampleRate)
    );
}

/**
 * Is stopped
 */
static ERL_NIF_TERM erlaudio_stream_is_stopped(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // struct state* state = get_state(env);
    struct erlaudio_stream_handle* handle;
    if(argc != 1 || !enif_get_resource(env, argv[0], ERLAUDIO_STREAM_RESOURCE, (void**) &handle)) {
        return enif_make_badarg(env);
    }
    return Pa_IsStreamStopped(handle->pa) ? enif_make_atom(env, "true") : enif_make_atom(env, "false");
}

/**
 * Stream is active
 */
static ERL_NIF_TERM erlaudio_stream_is_active(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // struct state* state = get_state(env);
    struct erlaudio_stream_handle* handle;
    if(argc != 1 || !enif_get_resource(env, argv[0], ERLAUDIO_STREAM_RESOURCE, (void **) &handle)) {
        return enif_make_badarg(env);
    }
    return Pa_IsStreamActive(handle->pa) ? enif_make_atom(env, "true") : enif_make_atom(env, "false");
}

