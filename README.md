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

ok = tss2_tcti_ptr_release_nif(SapiContext).
```


Or use wrapped NIFs that will log info on success and error with human readable TSS2 error codes:

```
{ok, TctiContext} = tss2_tcti_initialize_socket(Hostname, Port).

{ok, SapiContext} = tss2_sys_initialize(TctiContext).

{ok, NvReadBin} = tss2_sys_nv_read(Size, Index, SapiContext).

ok = tss2_tcti_ptr_release(SapiContext).
```

