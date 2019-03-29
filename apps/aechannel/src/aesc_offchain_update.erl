-module(aesc_offchain_update).

-define(INITIAL_VSN, 1).
-define(PINNED_BLOCKHASH_VSN, 2).

-define(NO_BLOCK, <<>>).

-type(pinned_block_hash() :: aec_blocks:block_header_hash() | ?NO_BLOCK).

-record(transfer, {
          from_id                 :: aeser_id:id(),
          to_id                   :: aeser_id:id(),
          amount                  :: non_neg_integer(),
          vsn = ?INITIAL_VSN      :: non_neg_integer(),
          block_hash = ?NO_BLOCK  :: pinned_block_hash() 
         }).

-record(withdraw, {
          to_id                   :: aeser_id:id(),
          amount                  :: non_neg_integer(),
          vsn = ?INITIAL_VSN      :: non_neg_integer(),
          block_hash = ?NO_BLOCK  :: pinned_block_hash() 
         }).

-record(deposit, {
          from_id                 :: aeser_id:id(),
          amount                  :: non_neg_integer(),
          vsn = ?INITIAL_VSN      :: non_neg_integer(),
          block_hash = ?NO_BLOCK  :: pinned_block_hash() 
         }).

-record(create_contract, {
          owner_id                :: aeser_id:id(),
          vm_version              :: aect_contracts:vm_version(),
          abi_version             :: aect_contracts:abi_version(),
          code                    :: binary(),
          deposit                 :: non_neg_integer(),
          call_data               :: binary(),
          vsn = ?INITIAL_VSN      :: non_neg_integer(),
          block_hash = ?NO_BLOCK  :: pinned_block_hash()
         }).

-record(call_contract, {
          caller_id               :: aeser_id:id(),
          contract_id             :: aeser_id:id(),
          abi_version             :: aect_contracts:abi_version(),
          amount                  :: non_neg_integer(),
          call_data               :: binary(),
          call_stack              :: [non_neg_integer()],
          gas_price               :: non_neg_integer(),
          gas                     :: non_neg_integer(),
          vsn = ?INITIAL_VSN      :: non_neg_integer(),
          block_hash = ?NO_BLOCK  :: pinned_block_hash() 
         }).

-opaque update() :: #transfer{}
                  | #withdraw{}
                  | #deposit{}
                  | #create_contract{}
                  | #call_contract{}.

-type update_type() :: transfer | withdraw | deposit | create_contract |
                       call_contract.

-export_type([update/0,
              pinned_block_hash/0]).

-export([ op_transfer/3
        , op_transfer/4
        , op_deposit/2
        , op_deposit/3
        , op_withdraw/2
        , op_withdraw/3
        , op_new_contract/6
        , op_new_contract/7
        , op_call_contract/6
        , op_call_contract/7
        , op_call_contract/8
        , op_call_contract/9
        ]).

-export([serialize/1,
         deserialize/1,
         for_client/1,
         apply_on_trees/6]).

-export([is_call/1,
         is_contract_create/1,
         extract_call/1,
         extract_caller/1,
         extract_contract_pubkey/1,
         extract_amounts/1,
         extract_abi_version/1,
         no_pinned_block/0
         ]).

-spec op_transfer(aeser_id:id(), aeser_id:id(), non_neg_integer()) -> update().
op_transfer(From, To, Amount) ->
    op_transfer(From, To, Amount, no_pinned_block(), ?INITIAL_VSN).

-spec op_transfer(aeser_id:id(), aeser_id:id(), non_neg_integer(),
                  pinned_block_hash()) -> update().
op_transfer(From, To, Amount, BlockHash) ->
    op_transfer(From, To, Amount, BlockHash, ?PINNED_BLOCKHASH_VSN).

-spec op_transfer(aeser_id:id(), aeser_id:id(), non_neg_integer(),
                  pinned_block_hash(), non_neg_integer()) -> update().
op_transfer(From, To, Amount, BlockHash, Vsn) ->
    account = aeser_id:specialize_type(From),
    account = aeser_id:specialize_type(To),
    #transfer{from_id     = From,
              to_id       = To,
              amount      = Amount,
              vsn         = Vsn,
              block_hash  = BlockHash}.

-spec op_deposit(aeser_id:id(), non_neg_integer()) -> update().
op_deposit(FromId, Amount) ->
    op_deposit(FromId, Amount, no_pinned_block(), ?INITIAL_VSN).

-spec op_deposit(aeser_id:id(), non_neg_integer(), pinned_block_hash())
    -> update().
op_deposit(FromId, Amount, BlockHash) ->
    op_deposit(FromId, Amount, BlockHash, ?PINNED_BLOCKHASH_VSN).

-spec op_deposit(aeser_id:id(), non_neg_integer(),
                 pinned_block_hash(), non_neg_integer()) -> update().
op_deposit(FromId, Amount, BlockHash, Vsn) ->
    account = aeser_id:specialize_type(FromId),
    #deposit{from_id = FromId, amount = Amount,
             block_hash = BlockHash, vsn = Vsn}.

-spec op_withdraw(aeser_id:id(), non_neg_integer()) -> update().
op_withdraw(ToId, Amount) ->
    op_withdraw(ToId, Amount, no_pinned_block(), ?INITIAL_VSN).

-spec op_withdraw(aeser_id:id(), non_neg_integer(), pinned_block_hash()) -> update().
op_withdraw(ToId, Amount, BlockHash) ->
    op_withdraw(ToId, Amount, BlockHash, ?PINNED_BLOCKHASH_VSN).

-spec op_withdraw(aeser_id:id(), non_neg_integer(), pinned_block_hash(),
                  non_neg_integer()) -> update().
op_withdraw(ToId, Amount, BlockHash, Vsn) ->
    account = aeser_id:specialize_type(ToId),
    #withdraw{to_id = ToId, amount = Amount,
              block_hash = BlockHash, vsn = Vsn}.

-spec op_new_contract(aeser_id:id(), aect_contracts:vm_version(), aect_contracts:abi_version(),
           binary(), non_neg_integer(), binary()) -> update().
op_new_contract(OwnerId, VmVersion, ABIVersion, Code, Deposit, CallData) ->
    op_new_contract(OwnerId, VmVersion, ABIVersion, Code, Deposit, CallData,
                    no_pinned_block(), ?INITIAL_VSN).

-spec op_new_contract(aeser_id:id(), aect_contracts:vm_version(), aect_contracts:abi_version(),
           binary(), non_neg_integer(), binary(), pinned_block_hash()) -> update().
op_new_contract(OwnerId, VmVersion, ABIVersion, Code, Deposit, CallData,
                BlockHash) ->
    op_new_contract(OwnerId, VmVersion, ABIVersion, Code, Deposit, CallData,
                    BlockHash, ?PINNED_BLOCKHASH_VSN).

-spec op_new_contract(aeser_id:id(), aect_contracts:vm_version(), aect_contracts:abi_version(),
           binary(), non_neg_integer(), binary(), pinned_block_hash(),
           non_neg_integer()) -> update().
op_new_contract(OwnerId, VmVersion, ABIVersion, Code, Deposit, CallData,
                BlockHash, Vsn) ->
    account = aeser_id:specialize_type(OwnerId),
    #create_contract{owner_id    = OwnerId,
                     vm_version  = VmVersion,
                     abi_version = ABIVersion,
                     code        = Code,
                     deposit     = Deposit,
                     call_data   = CallData,
                     block_hash  = BlockHash,
                     vsn         = Vsn}.

-spec op_call_contract(aeser_id:id(), aeser_id:id(), aect_contracts:abi_version(),
                       non_neg_integer(), Call, [non_neg_integer()]) -> update()
    when Call :: _.
op_call_contract(CallerId, ContractId, ABIVersion, Amount, CallData, CallStack) ->
    op_call_contract(CallerId, ContractId, ABIVersion, Amount, CallData,
                     CallStack, 1, 1000000, no_pinned_block(), ?INITIAL_VSN).

-spec op_call_contract(aeser_id:id(), aeser_id:id(), aect_contracts:abi_version(),
                       non_neg_integer(), Call, [non_neg_integer()],
                       pinned_block_hash()) -> update()
    when Call :: _.
op_call_contract(CallerId, ContractId, ABIVersion, Amount, CallData,
                 CallStack, BlockHash) ->
    op_call_contract(CallerId, ContractId, ABIVersion, Amount, CallData,
                     CallStack, 1, 1000000, BlockHash, ?PINNED_BLOCKHASH_VSN).

-spec op_call_contract(aeser_id:id(), aeser_id:id(), aect_contracts:abi_version(),
                       non_neg_integer(), Call,
                       [non_neg_integer()],
                       non_neg_integer(), non_neg_integer(),
                       pinned_block_hash(), non_neg_integer()) -> update()
    when Call :: _.
op_call_contract(CallerId, ContractId, ABIVersion, Amount, CallData, CallStack,
                 GasPrice, Gas, BlockHash, Vsn) ->
    account = aeser_id:specialize_type(CallerId),
    contract = aeser_id:specialize_type(ContractId),
    #call_contract{caller_id    = CallerId,
                   contract_id  = ContractId,
                   abi_version  = ABIVersion,
                   amount       = Amount,
                   call_data    = CallData,
                   call_stack   = CallStack,
                   gas_price    = GasPrice,
                   gas          = Gas,
                   block_hash   = BlockHash,
                   vsn          = Vsn}.

-spec apply_on_trees(aesc_offchain_update:update(), aec_trees:trees(),
                     aec_trees:trees(), aetx_env:env(),
                     non_neg_integer(), non_neg_integer()) -> aec_trees:trees().
apply_on_trees(Update, Trees0, OnChainTrees, OnChainEnv, Round, Reserve) ->
    case Update of
        #transfer{from_id = FromId, to_id = ToId, amount = Amount} ->
            From = account_pubkey(FromId),
            To = account_pubkey(ToId),
            Trees = remove_tokens(From, Amount, Trees0, Reserve),
            add_tokens(To, Amount, Trees);
        #deposit{from_id = FromId, amount = Amount} ->
            From = account_pubkey(FromId),
            add_tokens(From, Amount, Trees0);
        #withdraw{to_id = ToId, amount = Amount} ->
            To = account_pubkey(ToId),
            remove_tokens(To, Amount, Trees0, Reserve);
        #create_contract{owner_id = OwnerId, vm_version  = VmVersion,
                         abi_version = ABIVersion, code = Code,
                         deposit = Deposit, call_data   = CallData} ->
            Owner = account_pubkey(OwnerId),
            {ContractId, _Contract, Trees1} =
                aect_channel_contract:new(Owner, Round, #{vm => VmVersion, abi => ABIVersion},
                                          Code, Deposit, Trees0),
            ContractPubKey = contract_pubkey(ContractId),
            Trees2 = remove_tokens(Owner, Deposit, Trees1, Reserve),
            Trees3 = create_account(ContractPubKey, Trees2),
            Trees4 = add_tokens(ContractPubKey, Deposit, Trees3),
            Call = aect_call:new(OwnerId, Round, ContractId, Round, 0),
            _Trees = aect_channel_contract:run_new(ContractPubKey, Call, CallData, Trees4,
                                                  OnChainTrees, OnChainEnv);
        #call_contract{caller_id = CallerId, contract_id = ContractId,
                       abi_version = ABIVersion, amount = Amount,
                       call_data = CallData, call_stack = CallStack,
                       gas_price = GasPrice, gas = Gas} ->
            Caller = account_pubkey(CallerId),
            ContractPubKey = contract_pubkey(ContractId),
            Trees1 = remove_tokens(Caller, Amount, Trees0, Reserve),
            Trees2 = add_tokens(ContractPubKey, Amount, Trees1),
            Call = aect_call:new(CallerId, Round, ContractId, Round,
                                 GasPrice),
            _Trees = aect_channel_contract:run(ContractPubKey, ABIVersion, Call,
                                              CallData, CallStack,
                                              Trees2, Amount, GasPrice, Gas,
                                              OnChainTrees, OnChainEnv)
    end.

-spec for_client(update()) -> map().
for_client(#transfer{from_id = FromId, to_id = ToId, amount = Amount}) ->
    #{<<"op">> => <<"OffChainTransfer">>, % swagger name
      <<"from">> => aeser_api_encoder:encode(id_hash, FromId),
      <<"to">> => aeser_api_encoder:encode(id_hash, ToId),
      <<"am">>   => Amount};
for_client(#withdraw{to_id = ToId, amount = Amount}) ->
    #{<<"op">> => <<"OffChainWithdrawal">>, % swagger name
      <<"to">> => aeser_api_encoder:encode(id_hash, ToId),
      <<"am">>   => Amount};
for_client(#deposit{from_id = FromId, amount = Amount}) ->
    #{<<"op">> => <<"OffChainDeposit">>, % swagger name
      <<"from">> => aeser_api_encoder:encode(id_hash, FromId),
      <<"am">>   => Amount};
for_client(#create_contract{owner_id = OwnerId, vm_version  = VmVersion,
                         abi_version = ABIVersion, code = Code,
                         deposit = Deposit, call_data   = CallData}) ->
    #{<<"op">>          => <<"OffChainNewContract">>, % swagger name
      <<"owner">>       => aeser_api_encoder:encode(id_hash, OwnerId),
      <<"vm_version">>  => VmVersion,
      <<"abi_version">> => ABIVersion,
      <<"code">>        => Code,
      <<"deposit">>     => Deposit,
      <<"call_data">>   => aeser_api_encoder:encode(contract_bytearray, CallData)};
for_client(#call_contract{caller_id = CallerId, contract_id = ContractId,
                          abi_version = ABIVersion, amount = Amount,
                           call_data = CallData, call_stack = CallStack,
                          gas_price = GasPrice, gas = Gas}) ->
    #{<<"op">>          => <<"OffChainCallContract">>, % swagger name
      <<"caller">>      => aeser_api_encoder:encode(id_hash, CallerId),
      <<"contract">>    => aeser_api_encoder:encode(id_hash, ContractId),
      <<"abi_version">> => ABIVersion,
      <<"amount">>      => Amount,
      <<"gas">>         => Gas,
      <<"gas_price">>   => GasPrice,
      <<"call_data">>   => aeser_api_encoder:encode(contract_bytearray, CallData),
      <<"call_stack">>  => CallStack}.

-spec serialize(update()) -> binary().
serialize(Update) ->
    Fields = update2fields(Update),
    Vsn = version(Update),
    UpdateType = record_to_update_type(Update),
    aeser_chain_objects:serialize(
      ut2type(UpdateType),
      Vsn,
      update_serialization_template(Vsn, UpdateType),
      Fields).

-spec deserialize(binary()) -> update().
deserialize(Bin) ->
    {Type, Vsn, RawFields} =
        aeser_chain_objects:deserialize_type_and_vsn(Bin),
    UpdateType = type2ut(Type),
    Template = update_serialization_template(Vsn, UpdateType),
    Fields = aeserialization:decode_fields(Template, RawFields),
    fields2update(Vsn, UpdateType, Fields).

update2fields(Update) ->
    Vsn = version(Update),
    Fields0 = update2fields_(Update),
    case Vsn of
        ?INITIAL_VSN -> Fields0;
        ?PINNED_BLOCKHASH_VSN ->
            BH = block_hash(Update),
            Fields0 ++ [{vsn, Vsn},
                        {block_hash, BH}]
    end.

update2fields_(#transfer{from_id = FromId, to_id = ToId, amount = Amount}) ->
    [ {from,        FromId},
      {to,          ToId},
      {amount,      Amount}];
update2fields_(#deposit{from_id = FromId, amount = Amount}) ->
    [ {from,        FromId},
      {amount,      Amount}];
update2fields_(#withdraw{to_id = ToId, amount = Amount}) ->
    [ {to,          ToId},
      {amount,      Amount}];
update2fields_(#create_contract{owner_id = OwnerId, vm_version  = VmVersion,
                               abi_version = ABIVersion, code = Code,
                               deposit = Deposit, call_data   = CallData}) ->
    [ {owner,       OwnerId},
      {ct_version,  aect_contracts:pack_vm_abi(#{vm => VmVersion, abi => ABIVersion})},
      {code,        Code},
      {deposit,     Deposit},
      {call_data,   CallData}];
update2fields_(#call_contract{caller_id = CallerId, contract_id = ContractId,
                             abi_version = ABIVersion, amount = Amount,
                             call_data = CallData, call_stack = CallStack,
                             gas_price = GasPrice, gas = Gas}) ->
    [ {caller, CallerId},
      {contract, ContractId},
      {abi_version, ABIVersion},
      {amount, Amount},
      {gas, Gas},
      {gas_price, GasPrice},
      {call_data, CallData},
      {call_stack, CallStack}].

-spec fields2update(non_neg_integer(), update_type(), list()) -> update().
fields2update(Vsn, Type, Fields0) ->
    Fields =
        case Vsn of
            ?INITIAL_VSN -> Fields0 ++ [{vsn, Vsn},
                                        {block_hash, ?NO_BLOCK}];
            ?PINNED_BLOCKHASH_VSN -> Fields0
        end,
    fields2update(Type, Fields).

-spec fields2update(update_type(), list()) -> update().
fields2update(transfer, [{from,       From},
                         {to,         To},
                         {amount,     Amount},
                         {vsn,        Vsn},
                         {block_hash, BlockHash}]) ->
    op_transfer(From, To, Amount, BlockHash, Vsn);
fields2update(deposit, [{from,       From},
                        {amount,     Amount},
                        {vsn,        Vsn},
                        {block_hash, BlockHash}]) ->
    op_deposit(From, Amount, BlockHash, Vsn);
fields2update(withdraw, [{to,         To},
                         {amount,     Amount},
                         {vsn,        Vsn},
                         {block_hash, BlockHash}]) ->
    op_withdraw(To, Amount, BlockHash, Vsn);
fields2update(create_contract, [{owner,       OwnerId},
                                {ct_version,  CTVersion},
                                {code,        Code},
                                {deposit,     Deposit},
                                {call_data,   CallData},
                                {vsn,         Vsn},
                                {block_hash,  BlockHash}]) ->
    #{vm := VmVersion, abi := ABIVersion} = aect_contracts:split_vm_abi(CTVersion),
    op_new_contract(OwnerId, VmVersion, ABIVersion, Code, Deposit,
                    CallData, BlockHash, Vsn);
fields2update(call_contract, [ {caller,       CallerId},
                               {contract,     ContractId},
                               {abi_version,  ABIVersion},
                               {amount,       Amount},
                               {gas,          Gas},
                               {gas_price,    GasPrice},
                               {call_data,    CallData},
                               {call_stack,   CallStack},
                               {vsn,          Vsn},
                               {block_hash,   BlockHash}]) ->
    op_call_contract(CallerId, ContractId, ABIVersion, Amount, CallData,
                     CallStack, GasPrice, Gas, BlockHash, Vsn).

-spec ut2type(update_type())  -> atom().
ut2type(transfer)             -> channel_offchain_update_transfer;
ut2type(deposit)              -> channel_offchain_update_deposit;
ut2type(withdraw)             -> channel_offchain_update_withdraw;
ut2type(create_contract)      -> channel_offchain_update_create_contract;
ut2type(call_contract)        -> channel_offchain_update_call_contract.

-spec type2ut(atom()) -> update_type().
type2ut(channel_offchain_update_transfer)         -> transfer;
type2ut(channel_offchain_update_deposit)          -> deposit;
type2ut(channel_offchain_update_withdraw)         -> withdraw;
type2ut(channel_offchain_update_create_contract)  -> create_contract;
type2ut(channel_offchain_update_call_contract)    -> call_contract.

-spec update_serialization_template(non_neg_integer(), update_type()) -> list().
update_serialization_template(?INITIAL_VSN, Type) ->
    initial_serialization_template(Type);
update_serialization_template(?PINNED_BLOCKHASH_VSN, Type) ->
    initial_serialization_template(Type) ++ [{vsn,        int},
                                             {block_hash, binary}].

initial_serialization_template(transfer) ->
    [ {from,    id},
      {to,      id},
      {amount,  int}];
initial_serialization_template(deposit) ->
    [ {from,    id},
      {amount,  int}];
initial_serialization_template(withdraw) ->
    [ {to,      id},
      {amount,  int}];
initial_serialization_template(create_contract) ->
    [ {owner,       id},
      {ct_version,  int},
      {code,        binary},
      {deposit,     int},
      {call_data,   binary}];
initial_serialization_template(call_contract) ->
    [ {caller,      id},
      {contract,    id},
      {abi_version, int},
      {amount,      int},
      {gas,         int},
      {gas_price,   int},
      {call_data,   binary},
      {call_stack,  [int]}].

-spec record_to_update_type(update()) -> update_type().
record_to_update_type(#transfer{})        -> transfer;
record_to_update_type(#deposit{})         -> deposit;
record_to_update_type(#withdraw{})        -> withdraw;
record_to_update_type(#create_contract{}) -> create_contract;
record_to_update_type(#call_contract{})   -> call_contract.

check_min_amt(Amt, Reserve) ->
    if Amt < Reserve ->
            update_error(insufficient_balance);
       true ->
            Amt
    end.

create_account(Pubkey, Trees) ->
    AccountTrees = aec_trees:accounts(Trees),
    %TODO none = aec_accounts_trees:lookup(Pubkey, Trees),
    Acc = aec_accounts:new(Pubkey, 0),
    AccountTrees1 = aec_accounts_trees:enter(Acc, AccountTrees),
    aec_trees:set_accounts(Trees, AccountTrees1).


add_tokens(Pubkey, Amount, Trees) ->
    AccountTrees = aec_trees:accounts(Trees),
    Acc0 = aec_accounts_trees:get(Pubkey, AccountTrees), %% enforce account is present
    {ok, Acc} = aec_accounts:earn(Acc0, Amount),
    AccountTrees1 = aec_accounts_trees:enter(Acc, AccountTrees),
    aec_trees:set_accounts(Trees, AccountTrees1).

remove_tokens(Pubkey, Amount, Trees, Reserve) ->
    AccountTrees = aec_trees:accounts(Trees),
    Acc0 = aec_accounts_trees:get(Pubkey, AccountTrees),
    Balance = aec_accounts:balance(Acc0),
    check_min_amt(Balance - Amount, Reserve),
    Nonce = aec_accounts:nonce(Acc0),
    {ok, Acc} = aec_accounts:spend(Acc0, Amount, Nonce), %no nonce bump
    AccountTrees1 = aec_accounts_trees:enter(Acc, AccountTrees),
    aec_trees:set_accounts(Trees, AccountTrees1).

account_pubkey(Id) ->
    aeser_id:specialize(Id, account).

contract_pubkey(Id) ->
    aeser_id:specialize(Id, contract).

-spec extract_caller(update()) -> aec_keys:pubkey().
extract_caller(#call_contract{caller_id = CallerId}) ->
    account_pubkey(CallerId);
extract_caller(#create_contract{owner_id = OwnerId}) ->
    account_pubkey(OwnerId).

-spec is_call(update()) -> boolean().
is_call(#call_contract{}) ->
      true;
is_call(_) ->
      false.

-spec is_contract_create(update()) -> boolean().
is_contract_create(#create_contract{}) ->
      true;
is_contract_create(_) ->
      false.

-spec extract_call(aesc_offchain_update:update()) -> {aect_contracts:pubkey(), aec_keys:pubkey()}
                                                     | not_call.
extract_call(Update) ->
    case Update of
        #call_contract{caller_id = CallerId, contract_id = ContractId} ->
            {contract_pubkey(ContractId), account_pubkey(CallerId)};
        _ -> not_call
    end.

-spec extract_contract_pubkey(update()) -> aect_contracts:pubkey().
extract_contract_pubkey(#call_contract{contract_id = ContractId}) ->
    contract_pubkey(ContractId).

-spec extract_amounts(update()) -> {non_neg_integer(),
                                    non_neg_integer(),
                                    non_neg_integer()} | not_call.
extract_amounts(Update) ->
    case Update of
        #call_contract{amount = Amount, gas_price = GasPrice, gas = Gas}->
            {Amount, GasPrice, Gas};
        _ -> not_call
    end.

-spec extract_abi_version(update()) -> aect_contracts:abi_version().
extract_abi_version(#call_contract{abi_version = ABIVersion}) ->
    ABIVersion.

-spec no_pinned_block() -> pinned_block_hash().
no_pinned_block() -> ?NO_BLOCK.

update_error(Err) ->
    error({off_chain_update_error, Err}).

-spec version(update())               -> non_neg_integer().
version(#transfer{vsn = Vsn})         -> Vsn;
version(#deposit{vsn = Vsn})          -> Vsn;
version(#withdraw{vsn = Vsn})         -> Vsn;
version(#create_contract{vsn = Vsn})  -> Vsn;
version(#call_contract{vsn = Vsn})    -> Vsn.

-spec block_hash(update())                     -> pinned_block_hash().
block_hash(#transfer{block_hash = BH})         -> BH;
block_hash(#deposit{block_hash = BH})          -> BH;
block_hash(#withdraw{block_hash = BH})         -> BH;
block_hash(#create_contract{block_hash = BH})  -> BH;
block_hash(#call_contract{block_hash = BH})    -> BH.

