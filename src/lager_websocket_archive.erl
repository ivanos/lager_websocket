-module(lager_websocket_archive).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         insert/1,
         lookup/1,
         last/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert(Message) ->
    gen_server:call(?MODULE, {insert, Message}).

lookup(LogId) ->
    gen_server:call(?MODULE, {lookup, LogId}).

last(Count) ->
    gen_server:call(?MODULE, {last, Count}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    gen_server:cast(self(), start),
    State = #{
        tref => undefined
    },
    {ok, State}.

handle_call({insert, Message}, _From, State) ->
    LogId = save_message(Message),
    {reply, LogId, State};
handle_call({lookup, LogId}, _From, State) ->
    Reply = ets_lookup(LogId),
    {reply, Reply, State};
handle_call({last, Count}, _From, State) ->
    Reply = last_messages(last(), Count, []),
    {reply, Reply, State};
handle_call(Request, _From, State) ->
    {stop, {not_implemented, Request}, State}.

handle_cast(start, State) ->
    {noreply, State#{tref := schedule_cleaner()}};
handle_cast(Request, State) ->
    {stop, {not_implemented, Request}, State}.

handle_info(clean, State) ->
    clean(),
    {noreply, State#{tref := schedule_cleaner()}};
handle_info(Info, State) ->
    {stop, {not_implemented, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

schedule_cleaner() ->
    After = get_env(clean_interval_sec, 1),
    {ok, TRef} = timer:send_after(After * 1000, self(), clean),
    TRef.

clean() ->
    MaxRetentionMin = get_env(max_retention_minutes, 60),
    Now = os:timestamp(),
    MaxRetentionMicroSec = minutes_to_microsecs(MaxRetentionMin),
    FilterFn = fun(#{timestamp := Timestamp}) ->
        timer:now_diff(Now, Timestamp) > MaxRetentionMicroSec
    end,
    clean(FilterFn, first()).

clean(_, '$end_of_table') ->
    ok;
clean(FilterFn, LogId) ->
    case ets_lookup(LogId) of
        [] ->
            % someone else deleted the record
            clean(FilterFn, first());
        [{LogId, Message}] ->
            case FilterFn(Message) of
                true ->
                    delete(LogId),
                    clean(FilterFn, first());
                false ->
                    ok
            end
    end.

first() ->
    ets:first(lager_websocket).

delete(Key) ->
    ets:delete(lager_websocket, Key).

get_env(Key, Default) ->
    application:get_env(lager_websocket, Key, Default).

minutes_to_microsecs(Mins) ->
    timer:minutes(Mins) * 1000.

last_messages('$end_of_table', _, Acc) ->
    Acc;
last_messages(_, 0, Acc) ->
    Acc;
last_messages(Key, Count, Acc) ->
    [Record] = ets_lookup(Key),
    last_messages(prev(Key), Count - 1, [Record | Acc]).

save_message(Message) ->
    LogId = next_log_id(),
    case insert_new({LogId, Message}) of
        true ->
            LogId;
        false ->
            save_message(Message)
    end.

next_log_id() ->
    last_log_id() + 1.

last_log_id() ->
    case last() of
        '$end_of_table' -> 1;
        LastId -> LastId
    end.

last() ->
    ets:last(lager_websocket).

prev(Key) ->
    ets:prev(lager_websocket, Key).

ets_lookup(Key) ->
    ets:lookup(lager_websocket, Key).

insert_new(Record) ->
    ets:insert_new(lager_websocket, Record).
