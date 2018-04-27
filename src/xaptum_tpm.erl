-module(xaptum_tpm).

%% API exports
-export([
  init/0,
  error_code/1,
  tss2_tcti_initialize_socket_nif/2,
  tss2_sys_initialize_nif/1,
  tss2_sys_nv_read_nif/3,
  tss2_tcti_finalize_socket_nif/1,
  tss2_tcti_initialize_socket/2,
  tss2_sys_initialize/1,
  tss2_sys_nv_read/3
  ]).

-export([priv_dir/0]).

-define(TPM_APPNAME, 'xaptum_tpm_erlang').
-define(TPM_LIBNAME, 'libxaptum_tpm_erlang').

-on_load(init/0).

-define(TCTI_LEVEL_ERROR, 16#A0000).
-define(SYS_SAPI_LEVEL_ERROR, 16#80000).
-define(SYS_PART2_LEVEL_ERROR, 16#90000).


error_code(0)->"Success";
error_code(RC) ->
  case RC bor ?TCTI_LEVEL_ERROR =:= RC of
    true ->
        "TCTI: " ++ tcti_error_code(RC - ?TCTI_LEVEL_ERROR);
    _False ->
        case RC bor ?SYS_SAPI_LEVEL_ERROR =:= RC of
          true -> "Sys SAPI: " ++ sapi_error_code(RC - ?SYS_SAPI_LEVEL_ERROR);
          _False -> case RC bor ?SYS_PART2_LEVEL_ERROR =:= RC of
              true -> "Sys PART2: " ++ part2_error_code(RC - ?SYS_PART2_LEVEL_ERROR);
              _False -> "Unknown TSS2 error level"
                    end
        end
    end.

%% PUT the error codes from TSS-spec as defines into a separate include file i.e. tss.hrl

common_error_code(4)->"ABI mismatch (Passed in ABI version doesn't match called module's ABI version)";
common_error_code(5)->"Bad reference (a pointer is NULL that isn't allowed to be NULL)";
common_error_code(6)->"Insufficient buffer";
common_error_code(7)->"Bad sequence (Function called in the wrong order)";
common_error_code(11)->"Bad parameter value";
common_error_code(17)->"Malformed response";
common_error_code(_Unclassified)->"Unclassified".

tcti_error_code(2)->"Not implemented (If called functionality isn't implemented)";
tcti_error_code(3)->"Bad context structure";
tcti_error_code(8)->"No connection (Fails to connect to next lower layer)";
tcti_error_code(9)->"Try again (Operation timed out; function must be called again to be completed)";
tcti_error_code(10)->"IO error";
tcti_error_code(12)->"Operation not permitted";
tcti_error_code(21)->"Functionality not supported";
tcti_error_code(MaybeCommonErrCode)->common_error_code(MaybeCommonErrCode).


sapi_error_code(13)->"Invalid sessions (Session structures were sent, but command doesn't use them or doesn't use the specifed number of them)";
sapi_error_code(14)->"No decrypt parameter (If function called that uses decrypt parameter, but command doesn't support decrypt parameter)";
sapi_error_code(15)->"No encrypt parameter (If function called that uses encrypt parameter, but command doesn't support decrypt parameter)";
sapi_error_code(16)->"Bad parameter size";
sapi_error_code(18)->"Insufficient context";
sapi_error_code(19)->"Insufficient response";
sapi_error_code(20)->"Incompatible TCTI (Unknown or unusable TCTI version)";
sapi_error_code(21)->"Bad TCTI context";
sapi_error_code(MaybeCommonErrCode)-> common_error_code(MaybeCommonErrCode).

part2_error_code(_Unclassified)->"Unclassified!".

init() ->
  application:ensure_all_started(lager),

  SoName = filename:join([priv_dir(), ?TPM_LIBNAME]),
  lager:info("Loading NIFs from ~p", [SoName]),
  case erlang:load_nif(SoName, 0) of
    ok ->
      lager:info("Successfully loaded NIFs from ~p", [SoName]);
    {error, {reload, ReloadMessage}} ->
      lager:info("Reload attempt: ~p", [ReloadMessage]),
      ok;
    {error, RealError} -> lager:error("Error loading NIF library: ~p", [RealError])
  end.


priv_dir() ->
  case code:priv_dir(?TPM_APPNAME) of
    {error, bad_name} ->
      case filelib:is_dir(filename:join(["..", priv])) of
        true ->
          filename:join(["..", priv]);
        _ -> "priv"
      end;
    Dir -> Dir
  end.

%%====================================================================
%% NIFs
%%====================================================================

tss2_tcti_initialize_socket_nif(_Hostname, _Port) ->
  erlang:nif_error(?LINE).

tss2_sys_initialize_nif(_TctiContext) ->
  erlang:nif_error(?LINE).

tss2_sys_nv_read_nif(_Size, _Index, _SapiContext)->
  erlang:nif_error(?LINE).

tss2_tcti_finalize_socket_nif(_TctiContext)->
  erlang:nif_error(?LINE).

%%====================================================================
%% Optional API functions with human readable error log
%%====================================================================

%% PASS Port as int here becasue this API is for normal people
tss2_tcti_initialize_socket(Hostname, Port) ->
  case tss2_tcti_initialize_socket_nif(Hostname, Port) of
    {ok, TctiContext} ->
      lager:info("TCTI init socket successful!"),
      {ok, TctiContext};
    {error, ErrorCode} ->
      lager:error("~s", [error_code(ErrorCode)]),
      {error, ErrorCode}
  end.

tss2_sys_initialize(TctiContext) ->
  case tss2_sys_initialize_nif(TctiContext) of
    {ok, SapiContext} ->
      lager:info("SAPI context init successful!"),
      {ok, SapiContext};
    {error, ErrorCode} ->
      lager:error("~s", [error_code(ErrorCode)]),
      {error, ErrorCode}
  end.

tss2_sys_nv_read(Size, Index, SapiContext)->
  case tss2_sys_nv_read_nif(Size, Index, SapiContext) of
    {ok, OutBin} ->
      lager:info("nv read ~b bytes at ~s successful", [Size, integer_to_list(Index, 16)] ),
      {ok, OutBin};
    {error, ErrorCode} ->
      lager:error("~s", [error_code(ErrorCode)]),
      {error, ErrorCode}
  end.
