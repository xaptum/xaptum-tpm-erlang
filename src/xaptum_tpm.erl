-module(xaptum_tpm).

%% API exports
-export([
  init/0,
  tss2_tcti_initialize_socket/2,
  tss2_sys_initialize/1,
  tss2_sys_nv_read/3
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

tss2_tcti_initialize_socket(Hostname, Port) ->
  erlang:nif_error(?LINE).

tss2_sys_initialize(TctiContextBin) when is_binary(TctiContextBin) ->
  erlang:nif_error(?LINE).

tss2_sys_nv_read(_Size, _Index, _TctiContextBin)->
  erlang:nif_error(?LINE).

%%====================================================================
%% Internal functions
%%====================================================================

