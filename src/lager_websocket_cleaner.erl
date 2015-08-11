-module(lager_websocket_cleaner).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    gen_server:cast(self(), start),
    State = #{
        tref => undefined
    },
    {ok, State}.

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
    case lookup(LogId) of
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

lookup(Key) ->
    ets:lookup(lager_websocket, Key).

delete(Key) ->
    ets:delete(lager_websocket, Key).

get_env(Key, Default) ->
    application:get_env(lager_websocket, Key, Default).

minutes_to_microsecs(Mins) ->
    timer:minutes(Mins) * 1000.
