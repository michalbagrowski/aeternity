%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% Chain API for FATE
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_chain_api).

-export([ new/1
        ]).

-export([ contract_fate_code/2
        , account_balance/2
        ]).

-include_lib("aebytecode/include/aeb_fate_data.hrl").
-include("../../aecontract/include/aecontract.hrl").

%%%-------------------------------------------------------------------
%%% NOTE: We accept that this module causes havoc in the dependency
%%%       graph for now. The state/chain handling will move down to
%%%       a lower level in the dependency graph later.
%%%-------------------------------------------------------------------

%%%===================================================================
%%% API
%%% ===================================================================

new(#{ trees  := Trees
     , height := Height
     , tx_env := TxEnv
     }) ->
    aeprimop_state:new(Trees, Height, TxEnv).

contract_fate_code(Pubkey, State) ->
    case aeprimop_state:find_contract_without_store(Pubkey, State) of
        none -> error;
        {value, Contract} ->
            case aect_contracts:vm_version(Contract) of
                VMV when ?IS_FATE_SOPHIA(VMV) ->
                    SerCode = aect_contracts:code(Contract),
                    #{ byte_code := ByteCode} = aect_sophia:deserialize(SerCode),
                    try aeb_fate_asm:bytecode_to_fate_code(ByteCode, []) of
                        FateCode -> {ok, FateCode, State}
                    catch _:_ -> error
                    end;
                _ ->
                    error
            end
    end.

account_balance(Pubkey, State) ->
    case aeprimop_state:find_account(Pubkey, State) of
        {Account, State1} -> {ok, aec_accounts:balance(Account), State1};
        none              -> error
    end.
