-module(ct_player).
-behaviour(gen_server).

-export([start_link/2,stop/0]).
-export([get_handler/1,is_player/1,get_pid/1,get_name/1]).
-export([go/2, set_room/2,seen/2,unseen/2,entered/4,leave_denied/1]).
-export([init/1,handle_cast/2,handle_call/3,terminate/2,code_change/3,handle_info/2]).

-record(player_state,{id,
	my_pid,
	client_pid,
	name,
	life_points,
	room,			
	room_exits,
	params=[]}).
%% Accessors
is_player(Player) when is_record(Player,player_state) ->
	true;
is_player(_) ->
	bad_arg.
get_pid(#player_state{my_pid=Pid} = Player) ->
	Pid.
get_name(#player_state{name=Name} = Player) ->
	Name.



start_link(Name, Params) ->
	[ClientPid|_]=Params, 
    gen_server:start_link(?MODULE,{Name,ClientPid, Params}, []).
%% Client API
get_handler(Pid) ->
	gen_server:call(Pid,{get_handler}).
go(PlayerPid,Direction) ->
    gen_server:cast(PlayerPid, {go, Direction}).
%% To be called when the user is created
set_room(PlayerPid,[X,Y]) ->
    gen_server:cast(PlayerPid, {set_room, [X,Y]}).
seen(PlayerPid,OtherPlayerPid) ->
	gen_server:cast(PlayerPid, {seen, OtherPlayerPid}).
unseen(PlayerPid,OtherPlayerPid) ->
	gen_server:cast(PlayerPid, {unseen, OtherPlayerPid}).
leave_denied(PlayerPid) ->
	gen_server:cast(PlayerPid,{leave_denied}).
%% Internal functions
init({Name,ClientPid,Params}) ->
	io:format("ct_player has started (~w)~n", [self()]),
	State=#player_state{id=1,my_pid=self(),name=Name,client_pid=ClientPid, params=Params},
    {ok, State}.
stop() -> gen_server:cast(?MODULE, stop).
entered(PlayerPid, RoomPid, RoomExits, RoomName) ->
	gen_server:cast(PlayerPid, {entered, RoomPid, RoomExits, RoomName}).
%% User Callbacks
handle_cast({go, Direction}, State) ->
    io:format("Going ~p~n", [Direction]),
    ct_room:request_leave(State#player_state.room,Direction,self()),
    {noreply, State};
handle_cast({set_room, [X,Y]}, State) ->
	{ok,RoomPid}=ct_room_sup:get_pid([X,Y]),
	ct_room:enter(RoomPid, self(), null),
	{noreply, State};
handle_cast({seen, OtherPlayer}, State) ->
	%%Decide wether to attack or not.
	io:format("~s: has been seen by ~w~n",[State#player_state.name,OtherPlayer]),
	{noreply,State};
handle_cast({unseen, OtherPlayer}, State) ->
	%%The player left the room
	io:format("~s: no longer see ~w~n",[State#player_state.name,OtherPlayer]),
	{noreply,State};
handle_cast({entered, RoomPid, RoomExits, RoomName}, State) ->
	NewState=State#player_state{room_exits=RoomExits,room=RoomPid},
	io:format("~w is entering into a ~s ~n", [State#player_state.name,RoomName]),
	io:format("Room exits ~p ~n", [[X || {X,_} <- RoomExits]]),
	{noreply, NewState};
handle_cast({leave_denied},State) ->
	io:format("~w has hit with a wall ~n", [State#player_state.name]),
	{noreply,State};
handle_cast(stop, State) -> {stop, normal, State}.

handle_call({get_handler},_From,State) -> 
	{reply,State,State}.

%% System callbacks
terminate(_Reason, State) -> {ok,State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.