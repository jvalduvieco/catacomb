-module(ct_room_events).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

%% API
init([FeedbackData]) ->
  lager:debug("room event handler initialized~n"),
  {ok, FeedbackData}.

handle_event({talk, PlayerWhoTalksName, Message}, FeedbackData) ->
  lager:debug("Talk: ~p~n", [Message]),
  ct_feedback:send(
    {obj,[{"type",<<"room_chat_talk">>},
      {"body",{obj,[
        {"player_name", PlayerWhoTalksName},
        {"message", Message}
      ]}}
    ]},
    FeedbackData),
  {ok, FeedbackData};
handle_event({object_dropped_by_player,Player,Object},FeedbackData) ->
  ct_feedback:send(
    {obj,[{"type",<<"object_dropped_by_player">>},
      {"body",{obj,[
        {"object", {obj,Object}},
        {"player_id", ct_player:get_public_id(Player)},
        {"player_name",ct_player:get_name(Player)}
      ]}}
    ]},
    FeedbackData),
  {ok,FeedbackData};
handle_event({object_picked_by_player,Player,Object},FeedbackData) ->
  ct_feedback:send(
    {obj,[{"type",<<"object_picked_by_player">>},
      {"body",{obj,[
      {"object",{obj,Object}},
      {"player_id", ct_player:get_public_id(Player)},
      {"player_name",ct_player:get_name(Player)}
      ]}}
    ]},
    FeedbackData),
  {ok,FeedbackData}.

handle_call(_, Player) ->
  {ok, ok, Player}.

handle_info(_, FeedbackData) ->
  {ok, FeedbackData}.

code_change(_OldVsn, FeedbackData, _Extra) ->
  {ok, FeedbackData}.

terminate(_Args, _FeedbackData) ->
  lager:debug("room chat event handler terminating~n"),
  ok.

