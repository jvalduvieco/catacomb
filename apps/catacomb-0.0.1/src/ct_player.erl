-module(ct_player).
-behaviour(gen_server).

-export([start_link/2,stop/0]).
-export([go/2, set_room/2,seen/2,unseen/2]).
-export([init/1,handle_cast/2,terminate/2,code_change/3,handle_info/2]).

-record(state,{id,
	name,
	life_points,
	room,			
	room_exits,
	params=[]}).

start_link(Name,Params) ->
    gen_server:start_link(?MODULE,{Name,Params}, []).
%% Client API    
go(PlayerPid,Direction) ->
    gen_server:cast(PlayerPid, {go, Direction}).
%% To be called when the user is created
set_room(PlayerPid,[X,Y]) ->
    gen_server:cast(PlayerPid, {set_room, [X,Y]}).
seen(PlayerPid,OtherPlayerPid) ->
	gen_server:cast(PlayerPid, {seen, OtherPlayerPid}).
unseen(PlayerPid,OtherPlayerPid) ->
	gen_server:cast(PlayerPid, {unseen, OtherPlayerPid}).
%% Internal functions
init({Name,Params}) ->
	io:format("ct_player has started (~w)~n", [self()]),
	State=#state{id=1,name=Name,params=Params},
    {ok, State}.
stop() -> gen_server:cast(?MODULE, stop).

%% User Callbacks
handle_cast({go, Direction}, State) ->
    io:format("Going ~p~n", [Direction]),
    case [{Dir,Coords} || {Dir,Coords} <- State#state.room_exits, Direction=:=Dir] of
    	[{_,Coords}] -> 
    		{ok,RoomPid}=ct_room_sup:get_pid(Coords),
    		ct_room:leave_room(State#state.room, self()),
    		ct_room:enter_room(RoomPid, self()),
    		{ok,RoomExits}=ct_room:get_exits(RoomPid),
    		NewState=State#state{room_exits=RoomExits,room=RoomPid},
    		{noreply, NewState};
    	[] ->
			{noreply, State}
		end;
handle_cast({set_room, [X,Y]}, State) ->
	{ok,RoomPid}=ct_room_sup:get_pid([X,Y]),
	ok=ct_room:enter_room(RoomPid, self()),
	{ok,RoomExits}=ct_room:get_exits(RoomPid),
	NewState=State#state{room_exits=RoomExits,room=RoomPid},
	{noreply, NewState};
handle_cast({seen, OtherPlayer}, State) ->
	%%Decide wether to attack or not.
	io:format("~s: has been seen by ~w~n",[State#state.name,OtherPlayer]),
	{noreply,State};
handle_cast({unseen, OtherPlayer}, State) ->
	%%The player left the room
	io:format("~s: no longer see ~w~n",[State#state.name,OtherPlayer]),
	{noreply,State};
handle_cast(stop, State) -> {stop, normal, State}.

%% System callbacks
terminate(_Reason, State) -> {ok,State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.