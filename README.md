# xaptum_tpm_erlang
NIF wrappers for [Xaptum TPM project](https://github.com/xaptum/xaptum-tpm) project

```
make 
make test
```

Use plain NIFs in the following order (returns uninterpreted TSS2 error codes as integers only): 

```
{ok, TctiContext} = tss2_tcti_initialize_socket_nif(Hostname, Port).

{ok, SapiContext} = tss2_sys_initialize_nif(TctiContext).

{ok, NvReadBin} = tss2_sys_nv_read_nif(Size, Index, SapiContext).

```


Optionally use higher level Xaptum-TPM API: 

1. Wrapped NIFs log info on success and error with human readable TSS2 error codes.
2. They enforce only a single Tcti socket per TPM host preventing blocked processes. 
3. Explicit call to create Tcti socket not required to create Sapi context. 

```
%% Use only when TctiContext is needed by itself:
{ok, TctiContext} = tss2_tcti_maybe_initialize_socket(Hostname, Port).

%% Takes care of creating TctiContext too if it doesn't yet exist.
{ok, SapiContext} = tss2_sys_maybe_initialize(Hostname, Port).

{ok, NvReadBin} = tss2_sys_nv_read(Size, Index, SapiContext).

```

