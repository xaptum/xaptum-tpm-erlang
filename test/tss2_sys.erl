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

  ok.





