-module(ct_ai).
-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([get_handler/1, send_feedback/2, do_fun/2]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2, code_change/3, handle_info/2]).

-include ("include/ct_ai.hrl").

-spec start_link(_) -> 'ignore' | {'error',_} | {'ok',pid()}.

start_link(AiSpecs) ->
  gen_server:start_link(?MODULE, AiSpecs, []).

-spec send_feedback(_,_) -> 'ok'.

send_feedback(Player, Feedback) ->
  ClientPid = ct_player:get_client(Player),
% Depending on app configuration do a callback
  io:format("AI receives feedback: ~p~n", [Feedback]),
  gen_server:cast(ClientPid, {feedback, Feedback}).

-spec get_handler(_) -> any().

get_handler(Pid) ->
  gen_server:call(Pid, {get_handler}).

-spec do_fun(atom() | pid() | {atom(),_} | {'via',_,_},_) -> 'ok'.

do_fun(Pid, Fun) ->
  gen_server:cast(Pid, {do_fun, Fun}).


%% Internal functions
-spec init(#ai_specs{name::maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | byte(),binary() | [])}) -> {'ok',#ai_state{pid::pid(),players_seen::[]}}.

init(AiSpecs) ->
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A,B,C}),
  AiCharacter = [
      {<<"id">>, 32908230982},
      {<<"name">>, list_to_binary(AiSpecs#ai_specs.name)},
      {<<"max_life_points">>, 1000000},
      {<<"life_points">>, 150000},
      {<<"level">>, 60},
      {<<"experience_points">>, 200000},
      {<<"coord_x">>, 1},
      {<<"coord_y">>, 1}
  ],
  {ok, AiPlayer} = ct_player_sup:start_player([{obj, AiCharacter}]),
  ct_player:set_client(AiPlayer, self()),
  ct_player:set_feedback_fun(AiPlayer, fun (Player, Feedback) -> ct_ai:send_feedback(Player, Feedback) end),

  State = #ai_state{pid = self(),
                    player = AiPlayer,
                    behaviour_on_room_enter = AiSpecs#ai_specs.behaviour_on_room_enter,
                    behaviour_on_player_seen = AiSpecs#ai_specs.behaviour_on_player_seen,
                    behaviour_on_player_unseen = AiSpecs#ai_specs.behaviour_on_player_unseen
  },
  io:format("ct_ai has started (~w)~n", [self()]),
  {ok, State}.
-spec stop() -> 'ok'.

stop() -> gen_server:cast(?MODULE, stop).
%% User Callbacks
-spec handle_cast({'do_fun',fun((_) -> any())} | {'feedback',_},_) -> {'noreply',_}.

handle_cast({feedback, Feedback}, State) ->
  NewState = case Feedback of
    {obj, [{"type", <<"room_info">>},
           {"body", {obj, [{"name", RoomName},
                           {"exits", Exits}]}}]} ->
      io:format("room name: ~p~n", [RoomName]),
      io:format("exits: ~p~n", [Exits]),
% behaviour on enter room
      NewState2 = State#ai_state{room_name = RoomName, room_exits = Exits},
      Fun = State#ai_state.behaviour_on_room_enter,
      NewState3 = Fun(NewState2),
      NewState3;
    {obj, [{"type", <<"seen_by_info">>},
           {"body", {obj, [{"name", PlayerName},
                           {"player_id", PlayerId}]}}]} ->
      PlayersSeen = State#ai_state.players_seen,
      PlayersSeen2 = proplists:delete(PlayerId, PlayersSeen),
      PlayersSeen3 = PlayersSeen2 ++ [{PlayerId, PlayerName}],
% behaviour on player seen
      NewState2 = State#ai_state{players_seen = PlayersSeen3},
      Fun = State#ai_state.behaviour_on_player_seen,
      NewState3 = Fun(NewState2),
      NewState3;
    {obj, [{"type", <<"unseen_by_info">>},
           {"body", {obj, LastPlayerUnseen}}]} ->
%%LastPlayerUnseen = [{"name",PlayerName},{"player_id",PlayerId},{"direction",Direction}]
      PlayersSeen = State#ai_state.players_seen,
      PlayerId = proplists:get_value("player_id", LastPlayerUnseen),
      PlayersSeen2 = proplists:delete(PlayerId, PlayersSeen),
% behaviour on player seen
      NewState2 = State#ai_state{players_seen = PlayersSeen2, last_player_unseen = LastPlayerUnseen},
      Fun = State#ai_state.behaviour_on_player_unseen,
      NewState3 = Fun(NewState2),
      NewState3;
    Default ->
      io:format("unexpected feedback: ~p~n", [Default]),
      State
  end,
  io:format("new state: ~p~n", [NewState]),
  {noreply, NewState};
handle_cast({do_fun, Fun}, State) ->
  NewState = Fun(State),
  {noreply, NewState}.

-spec handle_call({'get_handler'},_,#ai_state{}) -> {'reply',_,#ai_state{}}.

handle_call({get_handler}, _From, State) ->
  {reply, State#ai_state.player, State}.

%% System callbacks
-spec terminate(_,_) -> {'ok',_}.

terminate(_Reason, State) -> {ok, State}.

-spec code_change(_,_,_) -> {'ok',_}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec handle_info(_,_) -> {'noreply',_}.

handle_info(_, State) -> {noreply, State}.