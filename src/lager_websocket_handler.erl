-module(lager_websocket_handler).

-behavior(cowboy_websocket_handler).

-include("lager_websocket_logger.hrl").

-export([send/2]).

-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

% API

send(Pid, Msg) ->
    Pid ! {text, Msg}.

% websocket callbacks

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    ?INFO("new websocket connection handler: ~p", [self()]),
    lager_websocket_publisher:subscribe(self()),
    {ok, Req, #{}}.

websocket_handle({text, Msg}, Req, State) ->
    Reply = lager_websocket_command:do(Msg),
    {reply, {text, Reply}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({text, Msg}, Req, State) ->
    {reply, [{text, Msg}], Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ?INFO("terminate websocket connection handler: ~p", [self()]),
    ok.
