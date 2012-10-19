-module(ct_player).
-behaviour(gen_server).

-export([start_link/1,stop/0]).
-export([get_handler/1,is_player/1,get_pid/1,get_name/1,get_max_life_points/1,get_life_points/1]).
-export([go/2, set_room/2,seen/2,unseen/2,entered/4,leave_denied/1]).
-export([init/1,handle_cast/2,handle_call/3,terminate/2,code_change/3,handle_info/2]).

-include ("ct_character_info.hrl").

-record(player_state,{id,
	my_pid,
	client_pid,
	name,
	max_life_points,
	life_points,
	room,			
	room_exits,
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

start_link(CharacterSpecs) ->
    gen_server:start_link(?MODULE,CharacterSpecs, []).

%% Client API
go(Player,Direction) ->
    gen_server:cast(ct_player:get_pid(Player), {go, Direction}).
%% To be called when the user is created
set_room(Player,[X,Y]) ->
    gen_server:cast(ct_player:get_pid(Player), {set_room, [X,Y]}).

%% Events
entered(Player, RoomPid, RoomExits, RoomName) ->
	gen_server:cast(ct_player:get_pid(Player), {entered, RoomPid, RoomExits, RoomName}).
seen(Player,OtherPlayer) ->
	gen_server:cast(ct_player:get_pid(Player), {seen, OtherPlayer}).
unseen(Player,OtherPlayer) ->
	gen_server:cast(ct_player:get_pid(Player), {unseen, OtherPlayer}).
leave_denied(Player) ->
	gen_server:cast(ct_player:get_pid(Player),{leave_denied}).

%% Internal functions
init(CharacterSpecs) ->
	State=#player_state{id=CharacterSpecs#ct_character_info.id,my_pid=self(),name=CharacterSpecs#ct_character_info.name},
	io:format("ct_player has started (~w)~n", [self()]),
    {ok, State}.
stop() -> gen_server:cast(?MODULE, stop).
%% User Callbacks
handle_cast({go, Direction}, State) ->
	io:format("Going ~p~n", [Direction]),
    ct_room:request_leave(State#player_state.room,Direction,State),
    {noreply, State};
handle_cast({set_room, [X,Y]}, State) ->
	{ok,RoomPid}=ct_room_sup:get_pid([X,Y]),
	ct_room:enter(RoomPid, State, null),
	{noreply, State};
handle_cast({seen, OtherPlayer}, State) ->
	%%Decide wether to attack or not.
	io:format("~s: has been seen by ~s~n",[State#player_state.name,ct_player:get_name(OtherPlayer)]),
	{noreply,State};
handle_cast({unseen, OtherPlayer}, State) ->
	%%The player left the room
	io:format("~s: no longer see ~s~n",[State#player_state.name,ct_player:get_name(OtherPlayer)]),
	{noreply,State};
handle_cast({entered, RoomPid, RoomExits, RoomName}, State) ->
	NewState=State#player_state{room_exits=RoomExits,room=RoomPid},
	io:format("~s is entering into a ~s ~n", [State#player_state.name,RoomName]),
	io:format("Room exits ~p ~n", [[X || {X,_} <- RoomExits]]),
	{noreply, NewState};
handle_cast({leave_denied},State) ->
	io:format("~s has hit with a wall ~n", [State#player_state.name]),
	{noreply,State};
handle_cast(stop, State) -> {stop, normal, State}.

handle_call({get_handler},_From,State) -> 
	{reply,State,State}.

%% System callbacks
terminate(_Reason, State) -> {ok,State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.