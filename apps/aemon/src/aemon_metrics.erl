-module(aemon_metrics).

-export([create/1]).

-export([publisher_balance/1]).
-export([publisher_post_tx/1]).

%% publisher

publisher_balance(Error) when is_atom(Error) ->
    update([publisher, balance, Error], 1);
publisher_balance(Balance) ->
    update([publisher, balance], Balance).

publisher_post_tx(Action) ->
    update([publisher, post_tx, Action], 1).


%% on start metric creation

create(publisher) ->
    create([publisher, balance], gauge),
    create([publisher, post_tx, success], counter),
    create([publisher, post_tx, max_adjustment], counter),
    create([publisher, post_tx, nonce_too_low], counter),
    create([publisher, post_tx, nonce_too_high], counter),
    ok;
create(_) ->
    ok.

%% internals

create(Name, Type) ->
    exometer:ensure(metric(Name), Type, []),
    ok.

update(Name, Value) ->
    aec_metrics:try_update(metric(Name), Value).

metric(Name) when is_atom(Name) -> metric([Name]);
metric(Name) ->
    [ae, epoch, aemon | Name ].
