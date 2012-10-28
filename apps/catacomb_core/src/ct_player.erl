-module(ct_player).
-behaviour(gen_server).

-export([start_link/1,stop/0]).
-export([get_handler/1,is_player/1,get_pid/1,get_name/1,get_max_life_points/1,get_life_points/1,get_client/1,set_client/2]).
-export([go/2, set_room/2,seen/2,unseen/2,entered/4,leave_denied/1,attack/2,hit/5]).
-export([init/1,handle_cast/2,handle_call/3,terminate/2,code_change/3,handle_info/2]).

%dcodix tmp
-export([get_players/1]).


-include ("ct_character_info.hrl").


-record(player_state,{id,
	my_pid,
	client,
	name,
	max_life_points,
	life_points,
	hit_chance,
	dodge_chance,
	max_damage,
	min_damage,
	armor,
	level,
	experience_points,
	room,			
	room_exits,
	located_players=[],
	params=[]}).
%% Accessors
get_handler(Pid) ->
	gen_server:call(Pid,{get_handler}).
is_player(Player) when is_record(Player,player_state) ->
	true;
is_player(_) ->
	bad_arg.
get_pid(#player_state{my_pid=Pid} = _Player) ->
	Pid.
get_name(#player_state{name=Name} = _Player) ->
	Name.
get_max_life_points(#player_state{max_life_points=MaxLifePoints} = _Player) ->
	MaxLifePoints.
get_life_points(#player_state{life_points=LifePoints} = _Player) ->
	LifePoints.
get_client(#player_state{client=Client} = _Player) ->
	Client.
start_link(CharacterSpecs) ->
    gen_server:start_link(?MODULE,CharacterSpecs, []).

%% Client API
go(Player,Direction) ->
    gen_server:cast(ct_player:get_pid(Player), {go, Direction}).
%% To be called when the user is created
set_room(Player,[X,Y]) ->
    gen_server:cast(ct_player:get_pid(Player), {set_room, [X,Y]}).
set_client(Player,Client) ->
	gen_server:cast(ct_player:get_pid(Player), {set_client, Client}).
%% Events
entered(Player, RoomPid, RoomExits, RoomName) ->
	gen_server:cast(ct_player:get_pid(Player), {entered, RoomPid, RoomExits, RoomName}).
seen(Player,OtherPlayer) ->
	gen_server:cast(ct_player:get_pid(Player), {seen, OtherPlayer}).
unseen(Player,OtherPlayer) ->
	gen_server:cast(ct_player:get_pid(Player), {unseen, OtherPlayer}).
leave_denied(Player) ->
	gen_server:cast(ct_player:get_pid(Player),{leave_denied}).
hit(Player,OtherPlayer,HitChance,MaxDamage,MinDamage) ->
	gen_server:cast(ct_player:get_pid(Player),{hit, OtherPlayer, HitChance, MaxDamage, MinDamage}).
attack(Player,OtherPlayer) ->
	gen_server:cast(ct_player:get_pid(Player),{attack, OtherPlayer}).


%dcodix tmp
get_players(Player) ->
	gen_server:cast(ct_player:get_pid(Player), {get_players}).

%% Internal functions
init([{obj,CharacterSpecs}]) ->
	State=#player_state{
		id=proplists:get_value(<<"id">>, CharacterSpecs, none),
		my_pid=self(),
		name=proplists:get_value(<<"name">>, CharacterSpecs, none),
		max_life_points=proplists:get_value(<<"max_life_points">>, CharacterSpecs, none),
		life_points=proplists:get_value(<<"life_points">>, CharacterSpecs, none),
		hit_chance=proplists:get_value(<<"hit_chance">>, CharacterSpecs, none),
		dodge_chance=proplists:get_value(<<"dodge_chance">>, CharacterSpecs, none),
		max_damage=proplists:get_value(<<"max_damage">>, CharacterSpecs, none),
		min_damage=proplists:get_value(<<"min_damage">>, CharacterSpecs, none),
		armor=proplists:get_value(<<"armor">>, CharacterSpecs, none),
		level=proplists:get_value(<<"level">>, CharacterSpecs, none),
		experience_points=proplists:get_value(<<"experience_points">>, CharacterSpecs, none)
		%room=ct_room_sup:get_pid([proplists:get_value(<<"coord_x">>, CharacterSpecs, none),proplists:get_value(<<"coord_y">>, CharacterSpecs, none)])
	},
	io:format("ct_player has started (~w)~n", [self()]),
    {ok, State}.
stop() -> gen_server:cast(?MODULE, stop).
%% User Callbacks
handle_cast({go, Direction}, State) ->
	%Feedback=io_lib:format("Going ~p~n", [Direction]),
	%ct_client_command:send_feedback(State,Feedback),
    ct_room:request_leave(State#player_state.room,Direction,State),
    {noreply, State};
handle_cast({set_room, [X,Y]}, State) ->
	{ok,RoomPid}=ct_room_sup:get_pid([X,Y]),
	ct_room:enter(RoomPid, State, null),
	{noreply, State};
handle_cast({set_client,Client},State) ->
	NewState=State#player_state{client=Client},
	{noreply,NewState};
handle_cast({seen, OtherPlayer}, State) ->
	%%Decide wether to attack or not.
	CleanOtherPlayer=OtherPlayer#player_state{located_players=[]},
	NewState=State#player_state{located_players=[CleanOtherPlayer|State#player_state.located_players]},
	io:format("~p~n",[NewState]),
	io:format("~s: has been seen by ~s~n",[State#player_state.name,ct_player:get_name(OtherPlayer)]),
	ct_client_command:send_feedback(State,
		{obj,[{"type",<<"seen_by_info">>},
			{"body",{obj,[
				{"name",ct_player:get_name(OtherPlayer)},
				{"player_id",99} % TODO define a way of referring to players, objs, etc..
			]}}
		]}),
	{noreply,NewState};
handle_cast({unseen, OtherPlayer}, State) ->
	%%The player left the room
	NewState=State#player_state{located_players=[P || P <- State#player_state.located_players, ct_player:get_pid(P)=/=ct_player:get_pid(OtherPlayer)]},
	%io:format("~s: no longer see ~s~n",[State#player_state.name,ct_player:get_name(OtherPlayer)]),
	ct_client_command:send_feedback(State,
		{obj,[{"type",<<"unseen_by_info">>},
			{"body",{obj,[
				{"name",ct_player:get_name(OtherPlayer)},
				{"player_id",99} % TODO define a way of referring to players, objs, etc..
			]}}
		]}),
	{noreply,NewState};
handle_cast({entered, RoomPid, RoomExits, RoomName}, State) ->
	NewState=State#player_state{room_exits=RoomExits,room=RoomPid},
	ct_client_command:send_feedback(State,
		{obj,[{"type",<<"room_info">>},
			{"body",{obj,[
				{"name",RoomName},
				{"exits",[X || {X,_} <- RoomExits]}
			]}}
		]}),
	%ct_client_command:send_feedback(State,io_lib:format("~s is entering into a ~s ~n", [State#player_state.name,RoomName])),
	%ct_client_command:send_feedback(State,io_lib:format("Room exits ~p ~n", [[X || {X,_} <- RoomExits]])),
	{noreply, NewState};
handle_cast({leave_denied},State) ->
	io:format("~s has hit with a wall ~n", [State#player_state.name]),
	ct_client_command:send_feedback(State,
		{obj,[{"type",<<"game_message_info">>},
			{"body",{obj,[
				{"msg_type",<<"warning">>},
				{"contents",<<"You hit a cold, hard wall">>}
			]}}
		]}),
	{noreply,State};



handle_cast({hit, OtherPlayer, HitChance, MaxDamage, MinDamage}, State) ->
	case random:uniform(100) > HitChance of
		false -> 
			case random:uniform(100) > State#player_state.dodge_chance of
				true -> 
					%io:format("min ~p max ~p armor ~p hitch ~p dodch ~p .~n",[MinDamage,MaxDamage,State#player_state.armor,HitChance,State#player_state.dodge_chance]),
					Damage=(MinDamage+random:uniform(MaxDamage-MinDamage))-State#player_state.armor,
					NewLifePoints = State#player_state.life_points - Damage,
					io:format("~p hits ~p dealing ~p .~n",[OtherPlayer#player_state.name,State#player_state.name,Damage]),
					NewState=State#player_state{life_points=NewLifePoints};
				false -> 
					%io:format("min ~p max ~p armor ~p hitch ~p dodch ~p .~n",[MinDamage,MaxDamage,State#player_state.armor,HitChance,State#player_state.dodge_chance]),
					NewState=State,
					io:format("~p tried to hit ~p but ~p dodged. ~n",[OtherPlayer#player_state.name,State#player_state.name,State#player_state.name])
			end;
		true -> 
			%io:format("min ~p max ~p armor ~p hitch ~p dodch ~p .~n",[MinDamage,MaxDamage,State#player_state.armor,HitChance,State#player_state.dodge_chance]),
			NewState=State,
			io:format("~p failed tying to hit ~p. ~n",[OtherPlayer#player_state.name,State#player_state.name])
	end,
	%io:format("~p ~n", [NewState#player_state.life_points]),
	{noreply, NewState};
handle_cast({attack, OtherPlayer}, State) ->
	ct_player:hit(OtherPlayer,State,State#player_state.hit_chance,State#player_state.max_damage,State#player_state.min_damage),
	%io:format("otherplayer is:    ::  ~p ~n", [OtherPlayer]),
	{noreply, State};

% dcodix tmp
handle_cast({get_players}, State) ->
	io:format("located:  ~p~n",[State#player_state.located_players]),
	{noreply,State};

handle_cast(stop, State) -> {stop, normal, State}.

handle_call({get_handler},_From,State) -> 
	{reply,State,State}.

%% System callbacks
terminate(_Reason, State) -> {ok,State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.