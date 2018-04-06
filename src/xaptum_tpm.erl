-module(xaptum_tpm).

%% API exports
-export([
  init/0,
  tss2_tcti_initialize_socket/2,
  tss2_sys_initialize/1,
  tss2_sys_nv_read/3,
  tss2_tcti_ptr_release/1
  ]).

-export([priv_dir/0]).

-define(APPNAME, xaptum_tpm_erlang).
-define(LIBNAME, 'xaptum-tpm-erlang').

init() ->
  SoName = filename:join([priv_dir(), ?LIBNAME]),
  lager:info("Loading NIFs from ~p", [SoName]),
  ok = erlang:load_nif(SoName, 0).

priv_dir() ->
  case code:priv_dir(?APPNAME) of
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

tss2_tcti_initialize_socket(_Hostname, _Port) ->
  erlang:nif_error(?LINE).

tss2_sys_initialize(_TctiContext) ->
  erlang:nif_error(?LINE).

tss2_sys_nv_read(_Size, _Index, _SapiContext)->
  erlang:nif_error(?LINE).

tss2_tcti_ptr_release(_SapiContext)->
  erlang:nif_error(?LINE).

%%====================================================================
%% Internal functions
%%====================================================================

