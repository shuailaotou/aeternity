-module(aestratum_user_register).

-behaviour(gen_server).

%% API.
-export([start_link/0,
         stop/0,
         add/2,
         del/1,
         member/1,
         find/1,
         notify/1,
         size/0
        ]).

%% gen_server.
-export([init/1,
         handle_call/3,
         handle_cast/2
        ]).

-export_type([user/0]).

-define(SERVER, ?MODULE).

-define(TAB, aestratum_user_register).
-define(TAB_REV, aestratum_user_register_reverse).

-type user()     :: binary().

-type conn_pid() :: pid().

-type value()    :: map().

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

-spec add(user(), conn_pid()) -> ok | {error, already_present}.
add(User, ConnPid) when is_binary(User) and is_pid(ConnPid) ->
    %% Write access to the tables is serialized to avoid race conditions.
    gen_server:call(?SERVER, {add, User, ConnPid}).


-spec del(user() | conn_pid()) -> ok | {error, not_found}.
del(User) when is_binary(User) ->
    gen_server:call(?SERVER, {del_user, User});
del(ConnPid) when is_pid(ConnPid) ->
    gen_server:call(?SERVER, {del_conn_pid, ConnPid}).

-spec member(user() | conn_pid()) -> boolean().
member(User) when is_binary(User) ->
    ets:member(?TAB, User);
member(ConnPid) when is_pid(ConnPid) ->
    ets:member(?TAB_REV, ConnPid).

-spec find(user() | conn_pid()) -> {ok, value()} | {error, not_found}.
find(User) when is_binary(User) ->
    case ets:lookup(?TAB, User) of
        [{User, Value}] -> {ok, Value};
        []              -> {error, not_found}
    end;
find(ConnPid) when is_pid(ConnPid) ->
    case ets:lookup(?TAB_REV, ConnPid) of
        [{ConnPid, User}] ->
            case ets:lookup(?TAB, User) of
                [{User, Value}] ->
                    {ok, maps:without([conn_pid], Value#{user => User})};
                [] ->
                    {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.

-spec notify(term()) -> ok.
notify(Msg) ->
    gen_server:cast(?SERVER, {notify, Msg}).

-spec size() -> non_neg_integer().
size() ->
    proplists:get_value(size, ets:info(?TAB)).

%% Callbacks.

init([]) ->
    ?TAB = ets:new(?TAB, [set, public, named_table]),
    ?TAB_REV = ets:new(?TAB_REV, [set, public, named_table]),
    {ok, undefined}.

handle_call({add, User, ConnPid}, _From, State) ->
    handle_add(User, ConnPid, State);
handle_call({del_user, User}, _From, State) ->
    handle_del_user(User, State);
handle_call({del_conn_pid, ConnPid}, _From, State) ->
    handle_del_conn_pid(ConnPid, State).

handle_cast({notify, Msg}, State) ->
    handle_notify(Msg, State).

%% Internal functions.

handle_add(User, ConnPid, State) ->
    Reply =
        case (not ets:member(?TAB, User)) andalso
             (not ets:member(?TAB_REV, ConnPid)) of
            true ->
                add_entries(User, ConnPid),
                ok;
            false ->
                {error, already_present}
        end,
    {reply, Reply, State}.

handle_del_user(User, State) ->
    Reply =
        case ets:lookup(?TAB, User) of
            [{User, #{conn_pid := ConnPid}}] ->
                del_entries(User, ConnPid);
            [] ->
                {error, not_found}
        end,
    {reply, Reply, State}.

handle_del_conn_pid(ConnPid, State) ->
    Reply =
        case ets:lookup(?TAB_REV, ConnPid) of
            [{ConnPid, User}] ->
                del_entries(User, ConnPid);
            [] ->
                {error, not_found}
        end,
    {reply, Reply, State}.

handle_notify(Msg, State) ->
    ets:safe_fixtable(?TAB, true),
    send_notify(ets:first(?TAB), Msg),
    ets:safe_fixtable(?TAB, false),
    {noreply, State}.

add_entries(User, ConnPid) ->
    Entry = {User, #{conn_pid => ConnPid,
                     created => aestratum_utils:timestamp()}},
    ets:insert(?TAB, Entry),
    ets:insert(?TAB_REV, {ConnPid, User}),
    ok.

del_entries(User, ConnPid) ->
    ets:delete(?TAB, User),
    ets:delete(?TAB_REV, ConnPid),
    ok.

send_notify(User, Msg) when User =/= '$end_of_table' ->
    [{User, #{conn_pid := ConnPid}}] = ets:lookup(?TAB, User),
    ConnPid ! Msg,
    send_notify(ets:next(?TAB, User), Msg);
send_notify('$end_of_table', _Msg) ->
    ok.
