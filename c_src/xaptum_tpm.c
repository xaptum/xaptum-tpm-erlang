#include <stdio.h>
#include <string.h>
#include <erl_nif.h>
#include <tss2/tss2_sys.h>
#include <tss2/tss2_tcti_socket.h>
#include <limits.h>
#include <unistd.h>

#define PORT_NAME_MAX 5
#define HOST_NAME_MAX 256

ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ERROR;

ErlNifResourceType* TCTI_RESOURCE_TYPE;
ErlNifResourceType* SAPI_RESOURCE_TYPE;

void
finalize_sapi(ErlNifEnv* env, void* sapi_context)
{
    TSS2_TCTI_CONTEXT * tcti_context;
    TSS2_RC rc = Tss2_Sys_GetTctiContext((TSS2_SYS_CONTEXT * ) sapi_context, &tcti_context);

    if (TSS2_RC_SUCCESS != rc) {
        fprintf(stderr, "Can't release tcti resource: error %d while getting TCTI Context pointer out of SAPI context\n", rc);
    }
    else{
        enif_release_resource(tcti_context);
        printf("Released tcti_context resource %p\n", tcti_context);
    }

    Tss2_Sys_Finalize(sapi_context);
}

void finalize_tcti(ErlNifEnv* env, void* tcti_context){
    // This closes tcti socket
    tss2_tcti_finalize(tcti_context);
    printf("Finalized tcti_context %p\n", tcti_context);
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    const char* mod = "xaptum_tpm";

    TCTI_RESOURCE_TYPE = enif_open_resource_type(
              env, mod, "tcti", finalize_tcti, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL
          );

    if(TCTI_RESOURCE_TYPE == NULL)
        return -1;

     SAPI_RESOURCE_TYPE = enif_open_resource_type(
        env, mod, "sapi", finalize_sapi, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL
     );

    if(SAPI_RESOURCE_TYPE == NULL)
        return -1;

    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_ERROR = enif_make_atom(env, "error");

    puts("Loaded TPM NIFs");

    return 0;
}

static ERL_NIF_TERM
tss2_tcti_initialize_socket_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 2) {
        return enif_make_badarg(env);
    }

    char hostname[HOST_NAME_MAX];
    char port[PORT_NAME_MAX];

    int str = enif_get_string(env, argv[0], hostname, sizeof(hostname), ERL_NIF_LATIN1);
    if(str <= 0) {
        fprintf(stderr, "Bad hostname arg at position 0, str %d \n", str);
            return enif_make_badarg(env);
    }

    str = enif_get_string(env, argv[1], port, sizeof(port), ERL_NIF_LATIN1);
    if(str <= 0) {
        fprintf(stderr, "Bad port arg at position 1: str %d\n", str);
        return enif_make_badarg(env);
    }

    size_t tcti_ctx_size = tss2_tcti_getsize_socket();

    TSS2_TCTI_CONTEXT * tcti_context = enif_alloc_resource(TCTI_RESOURCE_TYPE, tcti_ctx_size);

    printf("Initializing tcti_context at %p of size %zu on '%s:%s'\n", tcti_context, tcti_ctx_size, hostname, port);

    TSS2_RC rc =
    tss2_tcti_init_socket(hostname,
                          port,
                          tcti_context);

    ERL_NIF_TERM ret;

    if (rc == TSS2_RC_SUCCESS) {
        printf("Initialized tcti_context at %p\n", tcti_context);

        ERL_NIF_TERM tcti_resource = enif_make_resource(env, tcti_context);

        ret = enif_make_tuple2(env, ATOM_OK, tcti_resource);
    }
    else{
        fprintf(stderr, "Unable to initialize tcti socket due to error %d!\n", rc);

        ret = enif_make_tuple2(env, ATOM_ERROR, enif_make_int(env, rc)); // TODO return rc description instead of int
    }

    enif_release_resource(tcti_context);

    return ret;
}


static ERL_NIF_TERM
tss2_sys_initialize_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 1) {
        return enif_make_badarg(env);
    }

    TSS2_TCTI_CONTEXT * tcti_context;

    if(!enif_get_resource(env, argv[0], TCTI_RESOURCE_TYPE, (void**) &tcti_context)) {
        return enif_make_badarg(env);
    }

    size_t sapi_ctx_size = Tss2_Sys_GetContextSize(0);

    TSS2_SYS_CONTEXT *sapi_context = enif_alloc_resource(SAPI_RESOURCE_TYPE, sapi_ctx_size);

    TSS2_ABI_VERSION abi_version = TSS2_ABI_CURRENT_VERSION;

    TSS2_RC rc = Tss2_Sys_Initialize(
                              sapi_context,
                              sapi_ctx_size,
                              tcti_context,
                              &abi_version);

    if (TSS2_RC_SUCCESS != rc) {
        fprintf(stderr, "Error %d initializing TPM SAPI context\n", rc);
        enif_release_resource(sapi_context);
        return enif_make_tuple2(env, ATOM_ERROR, enif_make_int(env, rc));
    }
    else{
        ERL_NIF_TERM sapi_resource = enif_make_resource(env, sapi_context);
        // the user explicitely has to call tss2_tcti_ptr_release to call enif_release_resource on tcti_context when done with sapi which keeps a pointer to it
        enif_keep_resource(tcti_context);
        enif_release_resource(sapi_context);
        return enif_make_tuple2(env, ATOM_OK, sapi_resource);
    }
}

static ERL_NIF_TERM
tss2_sys_nv_read_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 3) {
        return enif_make_badarg(env);
    }

    uint16_t size;

    if(!enif_get_uint(env, argv[0], &size)) {
        return enif_make_badarg(env);
    }

    TPM_HANDLE index;
    if(!enif_get_uint(env, argv[1], &index)) {
        return enif_make_badarg(env);
    }

    TSS2_SYS_CONTEXT * sapi_context;
    if(!enif_get_resource(env, argv[2], SAPI_RESOURCE_TYPE, (void**) &sapi_context)) {
        fprintf(stderr, "Bad SAPI context arg at position 2\n");
        return enif_make_badarg(env);
    }

    TSS2_RC rc = TSS2_RC_SUCCESS;

    TPMS_AUTH_COMMAND session_data = {
        .sessionHandle = TPM_RS_PW,
        .sessionAttributes = {0},
    };

    // TODO move client test code into erlang

    printf("Reading %d bytes from nvram at %x\n", size, index);

    TPMS_AUTH_RESPONSE sessionDataOut = {{0}, {0}, {0}};
    (void)sessionDataOut;
    TSS2_SYS_CMD_AUTHS sessionsData;
    TSS2_SYS_RSP_AUTHS sessionsDataOut;
    TPMS_AUTH_COMMAND *sessionDataArray[1];
    sessionDataArray[0] = &session_data;
    TPMS_AUTH_RESPONSE *sessionDataOutArray[1];
    sessionDataOutArray[0] = &sessionDataOut;
    sessionsDataOut.rspAuths = &sessionDataOutArray[0];
    sessionsData.cmdAuths = &sessionDataArray[0];
    sessionsDataOut.rspAuthsCount = 1;
    sessionsData.cmdAuthsCount = 1;
    sessionsData.cmdAuths[0] = &session_data;

    uint16_t data_offset = 0;

    ErlNifBinary out_buffer_bin;
    enif_alloc_binary(size, &out_buffer_bin);

    while (size > 0) {
        uint16_t bytes_to_read = size;

        TPM2B_MAX_NV_BUFFER nv_data = {.size=0};

        rc = Tss2_Sys_NV_Read(sapi_context,
                               index,
                               index,
                               &sessionsData,
                               bytes_to_read,
                               data_offset,
                               &nv_data,
                               &sessionsDataOut);

        if (rc != TSS2_RC_SUCCESS) {
            fprintf(stderr, "Error reading from NVRAM due to error %d remaining bytes %d\n", rc, size);
            break;
        }

        size -= nv_data.size;

        memcpy(out_buffer_bin.data + data_offset, nv_data.buffer, nv_data.size);
        data_offset += nv_data.size;
    }

    if (rc == TSS2_RC_SUCCESS) {
        return enif_make_tuple2(env, ATOM_OK, enif_make_binary(env, &out_buffer_bin));
    } else {
        return enif_make_tuple2(env, ATOM_ERROR, enif_make_int(env, rc));
    }
}

static ErlNifFunc nif_funcs[] = {
    {"tss2_tcti_initialize_socket_nif", 2, tss2_tcti_initialize_socket_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"tss2_sys_initialize_nif", 1, tss2_sys_initialize_nif, 0},
    {"tss2_sys_nv_read_nif", 3, tss2_sys_nv_read_nif, ERL_NIF_DIRTY_JOB_IO_BOUND}
};

ERL_NIF_INIT(xaptum_tpm, nif_funcs, &load, NULL, NULL, NULL);


