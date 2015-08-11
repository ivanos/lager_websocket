%% lager_websocket allows websocket clients to receive lager messages.
%% Configured with the loglevel and maximum queue time. Messages
%% are held in a queue up to the maximum queue time. Messages older than
%% the maximum queue time are discarded. This allows clients to receive
%% a short history of messages and also resync if they temporarily lose
%% the connection.

-module(lager_websocket).
-behaviour(gen_event).

%% ------------------------------------------------------------------
%% gen_event Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% gen_event Function Definitions
%% ------------------------------------------------------------------

init(Level) ->
    create_table(),
    State = #{
        level => lager_util:config_to_mask(Level)
    },
    {ok, State}.

handle_event({log, Message}, State = #{level := Levels}) ->
    case lager_util:is_loggable(Message, Levels, ?MODULE) of
        true ->
            MessageMap = message_map(Message),
            insert_message(MessageMap),
            {ok, State};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_call(get_loglevel, State = #{level := Levels}) ->
    {ok, Levels, State};
handle_call({set_loglevel, Level}, State) ->
    {ok, ok, State#{level := lager_util:config_to_mask(Level)}};
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

insert_message(Message) ->
    case ets:insert_new(lager_websocket, {next_log_id(), Message}) of
        true ->
            ok;
        false ->
            insert_message(Message)
    end.

next_log_id() ->
    last_log_id() + 1.

last_log_id() ->
    case ets:last(lager_websocket) of
        '$end_of_table' -> 1;
        LastId -> LastId
    end.

message_map(Message) ->
    #{
        message => lager_msg:message(Message),
        timestamp => lager_msg:timestamp(Message),
        datetime => lager_msg:datetime(Message),
        severity => lager_msg:severity(Message),
        metadata => lager_msg:metadata(Message)
    }.

create_table() ->
    case catch {ok, ets:new(lager_websocket,
                                    [named_table, public, ordered_set])} of
        {ok, lager_websocket} -> ok;
        {'EXIT', {badarg, _}} -> ok
    end.
