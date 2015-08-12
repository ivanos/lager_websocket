%
% Distrubutes messages received from lager_websocket to the websockets
% of the clients.
%
-module(lager_websocket_publisher).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         subscribe/1,
         publish/1]).

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

subscribe(Pid) ->
    gen_server:cast(?MODULE, {subscribe, Pid}).

publish(Msg) ->
    gen_server:cast(?MODULE, {publish, Msg}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #{subscribers => []}}.

handle_call(Request, _From, State) ->
    {stop, {not_implemented, Request}, State}.

handle_cast(start, State) ->
    {noreply, State};
handle_cast({subscribe, Pid}, State = #{subscribers := Subscribers}) ->
    monitor(process, Pid),
    send_last(Pid),
    {noreply, State#{subscribers := [Pid | Subscribers]}};
handle_cast({publish, Message}, State = #{subscribers := []}) ->
    save_message(Message),
    {noreply, State};
handle_cast({publish, Message}, State = #{subscribers := Subscribers}) ->
    LogId = save_message(Message),
    send(Subscribers, {LogId, Message}),
    {noreply, State};
handle_cast(Msg, State) ->
    {stop, {not_implemented, Msg}, State}.

handle_info({'DOWN', _, _, Pid, _}, State = #{subscribers := Subscribers}) ->
    {noreply, State#{subscribers := lists:delete(Pid, Subscribers)}};
handle_info(Msg, State) ->
    {stop, {not_implemented, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% helpers
%% ------------------------------------------------------------------

% publish the last 5 message to new subscriber
send_last(Pid) ->
    Records = last_messages(new_subscriber_message_count()),
    lists:foreach(fun(Record) -> send_to_ws(Pid, Record) end, Records).

send(Pids, Record) ->
    EncodedRecord = encode(Record),
    lists:foreach(
        fun(Pid) ->
            [send_to_ws(Pid, R) || R <- EncodedRecord]
        end, Pids).

encode({LogId, #{message := Message,
                 datetime := {Date, Time},
                 severity := Severity,
                 metadata := Metadata}}) ->
    {[
        {<<"logid">>, to_binary(LogId)},
        {<<"message">>, iolist_to_binary(Message)},
        {<<"date">>, iolist_to_binary(Date)},
        {<<"time">>, iolist_to_binary(Time)},
        {<<"severity">>, to_binary(Severity)},
        {<<"metadata">>, encode_metadata(Metadata)}
    ]}.

encode_metadata(Metadata) ->
    {[{to_binary(Key), to_binary(Value)} || {Key, Value} <- Metadata]}.

to_binary(B) when is_binary(B) ->
    B;
to_binary(S) when is_list(S) ->
    iolist_to_binary(S);
to_binary(I) when is_integer(I) ->
    integer_to_binary(I);
to_binary(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
to_binary(P) when is_pid(P) ->
    list_to_binary(pid_to_list(P)).

last_messages(Count) ->
    lager_websocket_archive:last(Count).

save_message(Message) ->
    lager_websocket_archive:insert(Message).

send_to_ws(Pid, Message) ->
    send_to_ws(Pid, Message).

new_subscriber_message_count() ->
    application:get_env(lager_websocket, new_message_count, 5).
