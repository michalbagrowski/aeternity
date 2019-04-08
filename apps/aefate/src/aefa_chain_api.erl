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
        , final_trees/1
        , origin/1
        , tx_env/1
        ]).

-include_lib("aebytecode/include/aeb_fate_data.hrl").
-include("../../aecontract/include/aecontract.hrl").

%%%-------------------------------------------------------------------
%%% NOTE: We accept that this module causes havoc in the dependency
%%%       graph for now. The state/chain handling will move down to
%%%       a lower level in the dependency graph later.
%%%-------------------------------------------------------------------

-record(state, { primop_state :: aeprimop_state:state()
               , gas_price    :: non_neg_integer()
               , origin       :: aeb_fate_data:address()
               , tx_env       :: aetx_env:tx_env()
               }).

%%%===================================================================
%%% API
%%% ===================================================================

new(#{ gas_price := GasPrice
     , origin := Origin
     , trees  := Trees
     , tx_env := TxEnv
     }) ->
    #state{ primop_state = aeprimop_state:new(Trees, TxEnv)
          , gas_price    = GasPrice
          , origin       = Origin
          , tx_env       = TxEnv
          }.

contract_fate_code(Pubkey, #state{primop_state = PState} = S) ->
    case aeprimop_state:find_contract_without_store(Pubkey, PState) of
        none -> error;
        {value, Contract} ->
            case aect_contracts:vm_version(Contract) of
                VMV when ?IS_FATE_SOPHIA(VMV) ->
                    SerCode = aect_contracts:code(Contract),
                    #{ byte_code := ByteCode} = aect_sophia:deserialize(SerCode),
                    try aeb_fate_asm:bytecode_to_fate_code(ByteCode, []) of
                        FateCode -> {ok, FateCode, S#state{primop_state = PState}}
                    catch _:_ -> error
                    end;
                _ ->
                    error
            end
    end.

account_balance(Pubkey, #state{primop_state = PState} = S) ->
    case aeprimop_state:find_account(Pubkey, PState) of
        {Account, PState1} ->
            {ok, aec_accounts:balance(Account), S#state{primop_state = PState1}};
        none ->
            error
    end.

tx_env(#state{tx_env = TxEnv}) ->
    TxEnv.

final_trees(#state{primop_state = PState}) ->
    aeprimop_state:final_trees(PState).

origin(#state{origin = Origin}) ->
    Origin.
