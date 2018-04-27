%%%-------------------------------------------------------------------
%%% @author iguberman
%%% @copyright (C) 2018, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2018 3:33 PM
%%%-------------------------------------------------------------------
-module(tss2_sys).
-author("iguberman").

-export([nv_read_test/0]).

-include_lib("eunit/include/eunit.hrl").

-define(HOSTNAME, "localhost").
-define(PORT, "2321").
-define(PUB_KEY_FILE, "pub_key.txt").
-define(HANDLE, "handle.txt").


-define(XTT_DAA_CRED_SIZE, 260).
-define(XTT_DAA_GROUP_PUB_KEY_SIZE, 258).
-define(XTT_DAA_ROOT_ID_SIZE, 16).
-define(XTT_DAA_ROOT_PUB_KEY_SIZE, 32).

-define(KEY_HANDLE, 16#81800000).
-define(GPK_HANDLE, 16#1410000).
-define(CRED_HANDLE, 16#1410001).
-define(ROOT_ID_HANDLE, 16#1410003).
-define(ROOT_PUBKEY_HANDLE, 16#1410004).


nv_read_test() ->
  lager:info("STARTING SINGLE PROCESS TEST..."),

  {ok, TctiContext} = xaptum_tpm:tss2_tcti_initialize_socket(?HOSTNAME, ?PORT),
  {ok, SapiContext} = xaptum_tpm:tss2_sys_initialize(TctiContext),

  {ok, CredOutBufferBin} = xaptum_tpm:tss2_sys_nv_read(?XTT_DAA_CRED_SIZE, ?CRED_HANDLE, SapiContext),
  lager:info("CRED nv read: ~p", [CredOutBufferBin]),

  {ok, GpkOutBufferBin} = xaptum_tpm:tss2_sys_nv_read( ?XTT_DAA_GROUP_PUB_KEY_SIZE, ?GPK_HANDLE, SapiContext),
  lager:info("GPK nv read: ~p", [GpkOutBufferBin]),

  {ok, RootIdBin} = xaptum_tpm:tss2_sys_nv_read( ?XTT_DAA_ROOT_ID_SIZE, ?ROOT_ID_HANDLE, SapiContext),
  lager:info("Root ID nv read: ~p", [RootIdBin]),

  {ok, RootPubKeyBin} = xaptum_tpm:tss2_sys_nv_read( ?XTT_DAA_ROOT_PUB_KEY_SIZE, ?ROOT_PUBKEY_HANDLE, SapiContext),
  lager:info("Root pub key nv read: ~p", [RootPubKeyBin]),

  xaptum_tpm:tss2_tcti_finalize_socket_nif(TctiContext).


nv_read_multi_process_test()->
  timer:sleep(1000),

  lager:info("STARTING MULTI PROCESS TEST..."),

  {ok, TctiContext} = xaptum_tpm:tss2_tcti_initialize_socket(?HOSTNAME, ?PORT),
  {ok, SapiContext} = xaptum_tpm:tss2_sys_initialize(TctiContext),

  {ok, CredOutBufferBin} = nv_read_from_child_proc(?XTT_DAA_CRED_SIZE, ?CRED_HANDLE, SapiContext),
  lager:info("CHILD PROC CRED nv read: ~p", [CredOutBufferBin]),

  {ok, GpkOutBufferBin} = nv_read_from_child_proc( ?XTT_DAA_GROUP_PUB_KEY_SIZE, ?GPK_HANDLE, SapiContext),
  lager:info("CHILD PROC GPK nv read: ~p", [GpkOutBufferBin]),

  {ok, RootIdBin} = nv_read_from_child_proc( ?XTT_DAA_ROOT_ID_SIZE, ?ROOT_ID_HANDLE, SapiContext),
  lager:info("CHILD PROC Root ID nv read: ~p", [RootIdBin]),

  {ok, RootPubKeyBin} = nv_read_from_child_proc( ?XTT_DAA_ROOT_PUB_KEY_SIZE, ?ROOT_PUBKEY_HANDLE, SapiContext),
  lager:info("CHILD PROC Root pub key nv read: ~p", [RootPubKeyBin]),

  xaptum_tpm:tss2_tcti_finalize_socket_nif(TctiContext).

nv_read_from_child_proc(Size, Handle, SapiContext)->
  Parent = self(),
  NvReadFun =
    fun() ->
      Response = xaptum_tpm:tss2_sys_nv_read( Size, Handle, SapiContext),
      Parent ! {nvread, Response}
    end,
  Pid = spawn(NvReadFun),
  lager:info("Running nv read from proc ~p", [Pid]),
  receive
    {nvread, Result}  ->
      lager:info("Received nvread result from ~p: ~p~n", [Pid, Result]),
      Result
  after
    5000 ->
      lager:error("Nvread timeout from ~p", [Pid]),
      {error, timeout_after_5000}
  end.




