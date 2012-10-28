-module(ct_ai).
-behaviour(gen_server).

-export([start_link/1,stop/0]).
-export([get_handler/1,send_feedback/2, start_moving/1]).
-export([init/1,handle_cast/2,handle_call/3,terminate/2,code_change/3,handle_info/2]).

-record(ai_state,{
		player,
		room_name,
		room_exits,
		players_seen = [],
		last_player_unseen,
		behaviour_fun
	}).

start_link(AiSpecs) ->
    gen_server:start_link(?MODULE, AiSpecs, []).

send_feedback(Player,Feedback)->
	ClientPid=ct_player:get_client(Player),
	% Depending on app configuration do a callback
	io:format("AI receives feedback: ~p~n", [Feedback]),
	gen_server:cast(ClientPid, {feedback, Feedback}).

start_moving(AiPid) ->
	%% move
	gen_server:cast(AiPid, {move, AiPid}).

get_handler(Pid) ->
	gen_server:call(Pid,{get_handler}).

%% Internal functions
init(AiSpecs) ->
	AiCharacter=[
		{<<"id">>, 32908230982},
		{<<"name">>, <<"HAL">>},
		{<<"max_life_points">>, 1000000},
		{<<"life_points">>, 150000},
		{<<"level">>, 60},
		{<<"experience_points">>, 200000},
		{<<"coord_x">>, 1},
		{<<"coord_y">>, 1}
	],
	{ok, AiPlayer} = ct_player_sup:start_player([{obj,AiCharacter}]),
	ct_player:set_client(AiPlayer,self()),
	ct_player:set_feedback_fun(AiPlayer, fun(Player, Feedback) -> ct_ai:send_feedback(Player, Feedback) end),

	State=#ai_state{player=AiPlayer
					behaviour_fun=AiSpecs#ai_specs.behaviour_fun},
	io:format("ct_ia has started (~w)~n", [self()]),
    {ok, State}.
stop() -> gen_server:cast(?MODULE, stop).
%% User Callbacks
handle_cast({feedback, Feedback}, State) ->
	NewState = case Feedback of
		{obj,[{"type",<<"room_info">>},
              {"body",{obj,[{"name",RoomName},
              				{"exits",Exits}]}}]} -> 
			io:format("room name: ~p~n", [RoomName]), 
			io:format("exits: ~p~n", [Exits]),
			State#ai_state{room_name=RoomName,room_exits=Exits};
        {obj,[{"type",<<"seen_by_info">>},
              {"body",{obj,[{"name",PlayerName},
              				{"player_id",PlayerId}]}}]} -> 
 			PlayersSeen = State#ai_state.players_seen,
 			PlayersSeen2 = proplists:delete(PlayerId, PlayersSeen),
 			PlayersSeen3 = PlayersSeen2 ++ [{PlayerId, PlayerName}],
 			State#ai_state{players_seen=PlayersSeen3};
        {obj,[{"type",<<"unseen_by_info">>},
              {"body",{obj,LastPlayerUnseen}}]} ->
            %%LastPlayerUnseen = [{"name",PlayerName},{"player_id",PlayerId},{"direction",Direction}]
 			PlayersSeen = State#ai_state.players_seen,
 			PlayerId = proplists:get_value("player_id", LastPlayerUnseen),
 			PlayersSeen2 = proplists:delete(PlayerId, PlayersSeen),
            State#ai_state{players_seen=PlayersSeen2,last_player_unseen=LastPlayerUnseen};
 		Default ->
 			io:format("unexpected feedback: ~p~n", [Default]),
 			State            
	end,
	io:format("new state: ~p~n", [NewState]),
    {noreply, NewState};
handle_cast({move, AiPid}, State) ->
	%io:format("moving ai...~n"),
	%% select rand exit
	Exits = State#ai_state.room_exits,
	RandomExit = lists:nth(random:uniform(length(Exits)), Exits),
	%io:format("RandomExit: ~p~n", [RandomExit]),
	%% move player
	ct_player:go(State#ai_state.player, RandomExit),
	%% wait
	%io:format("waiting...~n"),
	timer:sleep(2000),
	ct_ai:start_moving(AiPid),
	{noreply,State}.
handle_call({get_handler},_From,State) -> 
	{reply,State#ai_state.player,State}.

%% System callbacks
terminate(_Reason, State) -> {ok,State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.