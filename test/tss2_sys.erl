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

-define(HOSTNAME, "localhost").
-define(PORT, "2321").
-define(PUB_KEY_FILE, "pub_key.txt").
-define(HANDLE, "handle.txt").


-define(XTT_DAA_CRED_SIZE, 260).
-define(XTT_DAA_GROUP_PUB_KEY_SIZE, 258).
-define(XTT_DAA_ROOT_ID_SIZE, 16).
-define(XTT_DAA_ROOT_PUB_KEY_SIZE, 32).

-define(KEY_HANDLE, list_to_integer("81800000", 16)).
-define(GPK_HANDLE, list_to_integer("1400000", 16)).
-define(CRED_HANDLE, list_to_integer("1400001", 16)).
-define(ROOT_ID_HANDLE, list_to_integer("1400003", 16)).
-define(ROOT_PUBKEY_HANDLE, list_to_integer("1400004", 16)).


nv_read_test() ->
  application:ensure_all_started(lager),

  xaptum_tpm:init(),

  {ok, TctiContextBin} = xaptum_tpm:tss2_tcti_initialize_socket(?HOSTNAME, ?PORT),
  {ok, _SapiContextBin, TctiContextBin} = xaptum_tpm:tss2_sys_initialize(TctiContextBin),

  {ok, CredOutBufferBin, _TctiContextBin} = xaptum_tpm:tss2_sys_nv_read(?XTT_DAA_CRED_SIZE, ?CRED_HANDLE, TctiContextBin),
  lager:info("CRED nv read: ~p", [CredOutBufferBin]),

  {ok, GpkOutBufferBin, _TctiContextBin} = xaptum_tpm:tss2_sys_nv_read( ?XTT_DAA_GROUP_PUB_KEY_SIZE, ?GPK_HANDLE, TctiContextBin),
  lager:info("GPK nv read: ~p", [GpkOutBufferBin]),

  false = true.





