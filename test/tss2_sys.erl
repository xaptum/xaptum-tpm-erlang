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

-include_lib("eunit/include/eunit.hrl").

-define(HOSTNAME, <<"localhost">>).
-define(PORT, <<"2321">>).
-define(PUB_KEY_FILE, "pub_key.txt").
-define(HANDLE, "handle.txt").


-define(XTT_DAA_CRED_SIZE, 260).
-define(XTT_DAA_GROUP_PUB_KEY_SIZE, 258).
-define(XTT_DAA_ROOT_ID_SIZE, 16).
-define(XTT_DAA_ROOT_PUB_KEY_SIZE, 32).

-define(KEY_HANDLE, list_to_integer("81800000", 16)).
-define(GPK_HANDLE, list_to_integer("1410000", 16)).
-define(CRED_HANDLE, list_to_integer("1410001", 16)).
-define(ROOT_ID_HANDLE, list_to_integer("1410003", 16)).
-define(ROOT_PUBKEY_HANDLE, list_to_integer("1410004", 16)).

nv_read_test() ->
  application:ensure_all_started(lager),

  xaptum_tpm:init(),

  {ok, TctiContext} = xaptum_tpm:tss2_tcti_initialize_socket(?HOSTNAME, ?PORT),
  {ok, SapiContext} = xaptum_tpm:tss2_sys_initialize(TctiContext),

  {ok, CredOutBufferBin} = xaptum_tpm:tss2_sys_nv_read(?XTT_DAA_CRED_SIZE, ?CRED_HANDLE, SapiContext),
  lager:info("CRED nv read: ~p", [CredOutBufferBin]),

  {ok, GpkOutBufferBin} = xaptum_tpm:tss2_sys_nv_read( ?XTT_DAA_GROUP_PUB_KEY_SIZE, ?GPK_HANDLE, SapiContext),
  lager:info("GPK nv read: ~p", [GpkOutBufferBin]),

  ok = xaptum_tpm:tss2_tcti_ptr_release(SapiContext),

  false = true.





