-module(ct_room_chat).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

%% API
init([Player]) ->
  lager:debug("room chat event handler initialized~n"),
  {ok, Player}.

handle_event({talk, PlayerWhoTalksName, Message}, Player) ->
  lager:debug("Talk: ~p~n", [Message]),
  ct_player:heard(Player, PlayerWhoTalksName, Message),
  {ok, Player}.

handle_call(_, State) ->
  {ok, ok, State}.

handle_info(_, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Args, _State) ->
  lager:debug("room chat event handler terminating~n"),
  ok.

