#include <stdio.h>
#include <string.h>
#include <erl_nif.h>
#include <tss2/tss2_sys.h>
#include <tss2/tss2_tcti_socket.h>

ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ERROR;

ErlNifResourceType* STRUCT_RESOURCE_TYPE;

void
free_resource(ErlNifEnv* env, void* obj)
{
   enif_free(obj);
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    const char* mod = "xaptum_tpm";
    const char* name = "struct";

    STRUCT_RESOURCE_TYPE = enif_open_resource_type(
        env, mod, name, free_resource, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL
    );

    if(STRUCT_RESOURCE_TYPE == NULL)
        return -1;

    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_ERROR = enif_make_atom(env, "error");

    puts("Loaded TPM NIFs");

    return 0;
}

static ERL_NIF_TERM
tss2_tcti_initialize_socket(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    puts("Running NIF tss2_tcti_initialize_socket\n");

    if(argc != 2) {
        return enif_make_badarg(env);
    }

    ErlNifBinary hostname;
    ErlNifBinary port;

    if(!enif_inspect_binary(env, argv[0], &hostname) ) {
        fprintf(stderr, "Bad hostname arg at position 0\n");
            return enif_make_badarg(env);
    }

    if(!enif_inspect_binary(env, argv[1], &port) ) {
        fprintf(stderr, "Bad port arg at position 1\n");
        return enif_make_badarg(env);
    }

    ErlNifBinary tcti_context_buffer_bin;
    enif_alloc_binary(128, &tcti_context_buffer_bin);

    TSS2_RC rc =
    tss2_tcti_init_socket(hostname.data,
                          port.data,
                          (TSS2_TCTI_CONTEXT *) tcti_context_buffer_bin.data);

    if (rc == TSS2_RC_SUCCESS) {
        return enif_make_tuple2(env,
        ATOM_OK,
        enif_make_binary(env, &tcti_context_buffer_bin));
    }
    else{
        fprintf(stderr, "Unable to initialize tcti socket due to error %d!\n", rc);
        return enif_make_tuple2(env, ATOM_ERROR, enif_make_int(env, rc)); // TODO return rc description instead of int
    }
}


static ERL_NIF_TERM
tss2_sys_initialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    puts("Running NIF tss2_sys_initialize\n");

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    ErlNifBinary tcti_context_bin;

    if(!enif_inspect_binary(env, argv[0], &tcti_context_bin) ) {
        fprintf(stderr, "Bad tcti_context arg at position 1\n");
        return enif_make_badarg(env);
    }

    size_t sapi_ctx_size = Tss2_Sys_GetContextSize(0);

    ErlNifBinary sapi_context_bin;
    enif_alloc_binary(sapi_ctx_size, &sapi_context_bin);

    TSS2_ABI_VERSION abi_version = TSS2_ABI_CURRENT_VERSION;

    TSS2_RC rc = Tss2_Sys_Initialize(
                              (TSS2_SYS_CONTEXT *) sapi_context_bin.data,
                              sapi_ctx_size,
                              (TSS2_TCTI_CONTEXT *) tcti_context_bin.data,
                              &abi_version);

    if (TSS2_RC_SUCCESS != rc) {
        fprintf(stderr, "Error %d initializing TPM SAPI context\n", rc);
        return enif_make_tuple2(env, ATOM_ERROR, enif_make_int(env, rc));
    }
    else
    return enif_make_tuple3(env,
        ATOM_OK,
        enif_make_binary(env, &sapi_context_bin),
        enif_make_binary(env, &tcti_context_bin)
    );
}

static ERL_NIF_TERM
tss2_sys_nv_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    puts("Running NIF tss2_sys_nv_read\n");

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

    ErlNifBinary tcti_context_bin;

    if(!enif_inspect_binary(env, argv[2], &tcti_context_bin) ) {
        fprintf(stderr, "Bad tcti_context arg at position 2\n");
        return enif_make_badarg(env);
    }

    size_t sapi_ctx_size = Tss2_Sys_GetContextSize(0);

    ErlNifBinary sapi_context_bin;
    enif_alloc_binary(sapi_ctx_size, &sapi_context_bin);


    TPMS_AUTH_COMMAND session_data = {
        .sessionHandle = TPM_RS_PW,
        .sessionAttributes = {0},
    };
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

    TSS2_RC rc = TSS2_RC_SUCCESS;

    printf("Reading %d bytes from nvram\n", size);

    while (size > 0) {
        uint16_t bytes_to_read = size;

        TPM2B_MAX_NV_BUFFER nv_data = {.size=0};

        rc = Tss2_Sys_NV_Read((TSS2_SYS_CONTEXT *) sapi_context_bin.data,
                               index,
                               index,
                               &sessionsData,
                               bytes_to_read,
                               data_offset,
                               &nv_data,
                               &sessionsDataOut);

        if (rc != TSS2_RC_SUCCESS) {
            fprintf(stderr, "Error reading from NVRAM due to error %d remaining bytes %d\n", tc, size);
            break;
        }

        size -= nv_data.size;

        memcpy(out_buffer_bin.data + data_offset, nv_data.buffer, nv_data.size);
        data_offset += nv_data.size;
    }

    if (rc == TSS2_RC_SUCCESS) {
        return enif_make_tuple3(env,
        ATOM_OK,
        enif_make_binary(env, &out_buffer_bin),
        enif_make_binary(env, &tcti_context_bin)
        );
    } else {
        return enif_make_tuple2(env, ATOM_ERROR, enif_make_int(env, rc));
    }
}


static ErlNifFunc nif_funcs[] = {
    {"tss2_tcti_initialize_socket", 2, tss2_tcti_initialize_socket, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"tss2_sys_initialize", 1, tss2_sys_initialize, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"tss2_sys_nv_read", 3, tss2_sys_nv_read, ERL_NIF_DIRTY_JOB_CPU_BOUND}
    };

ERL_NIF_INIT(xaptum_tpm, nif_funcs, &load, NULL, NULL, NULL);


