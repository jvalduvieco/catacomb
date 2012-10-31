-module(ct_player).
-behaviour(gen_server).

-export([start_link/1,stop/0]).
-export([get_handler/1,is_player/1,get_pid/1,get_name/1,get_max_life_points/1,get_life_points/1,get_client/1,set_client/2,set_feedback_fun/2]).
-export([go/2, set_room/2,seen/2,unseen/3,entered/4,leave_denied/1]).
-export([init/1,handle_cast/2,handle_call/3,terminate/2,code_change/3,handle_info/2]).

-include ("include/ct_character_info.hrl").

%% Accessors
-spec get_handler(_) -> any().
get_handler(Pid) ->
	gen_server:call(Pid,{get_handler}).
-spec is_player(_) -> 'bad_arg' | 'true'.
is_player(Player) when is_record(Player,player_state) ->
	true;
is_player(_) ->
	bad_arg.
-spec get_pid(#player_state{}) -> any().
get_pid(#player_state{my_pid=Pid} = _Player) ->
	Pid.
-spec get_name(#player_state{}) -> any().
get_name(#player_state{name=Name} = _Player) ->
	Name.
-spec get_max_life_points(#player_state{}) -> any().
get_max_life_points(#player_state{max_life_points=MaxLifePoints} = _Player) ->
	MaxLifePoints.
-spec get_life_points(#player_state{}) -> any().
get_life_points(#player_state{life_points=LifePoints} = _Player) ->
	LifePoints.
-spec get_client(#player_state{}) -> any().
get_client(#player_state{client=Client} = _Player) ->
	Client.
-spec start_link(_) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link(CharacterSpecs) ->
    gen_server:start_link(?MODULE,CharacterSpecs, []).

%% Client API
-spec go(#player_state{},_) -> 'ok'.
go(Player,Direction) ->
    gen_server:cast(ct_player:get_pid(Player), {go, Direction}).
%% To be called when the user is created
-spec set_room(#player_state{},[any(),...]) -> 'ok'.
set_room(Player,[X,Y]) ->
    gen_server:cast(ct_player:get_pid(Player), {set_room, [X,Y]}).
-spec set_client(#player_state{},_) -> 'ok'.
set_client(Player,Client) ->
	gen_server:cast(ct_player:get_pid(Player), {set_client, Client}).
-spec set_feedback_fun(#player_state{},_) -> 'ok'.
set_feedback_fun(Player, Fun) ->
	gen_server:cast(ct_player:get_pid(Player), {set_feedback_fun, Fun}).
%% Events
-spec entered(#player_state{},_,_,_) -> 'ok'.
entered(Player, RoomPid, RoomExits, RoomName) ->
	gen_server:cast(ct_player:get_pid(Player), {entered, RoomPid, RoomExits, RoomName}).
-spec seen(#player_state{},_) -> 'ok'.
seen(Player,OtherPlayer) ->
	gen_server:cast(ct_player:get_pid(Player), {seen, OtherPlayer}).
-spec unseen(#player_state{},_,_) -> 'ok'.
unseen(Player,OtherPlayer,Direction) ->
	gen_server:cast(ct_player:get_pid(Player), {unseen, OtherPlayer, Direction}).
-spec leave_denied(#player_state{}) -> 'ok'.
leave_denied(Player) ->
	gen_server:cast(ct_player:get_pid(Player),{leave_denied}).

%% Internal functions
-spec init([{'obj',[any()]},...]) -> {'ok',#player_state{my_pid::pid(),params::[]}}.
init([{obj,CharacterSpecs}]) ->
	State=#player_state{
		id=proplists:get_value(<<"id">>, CharacterSpecs, none),
		my_pid=self(),
		name=proplists:get_value(<<"name">>, CharacterSpecs, none),
		max_life_points=proplists:get_value(<<"max_life_points">>, CharacterSpecs, none),
		life_points=proplists:get_value(<<"life_points">>, CharacterSpecs, none),
		level=proplists:get_value(<<"level">>, CharacterSpecs, none),
		experience_points=proplists:get_value(<<"experience_points">>, CharacterSpecs, none)
		%room=ct_room_sup:get_pid([proplists:get_value(<<"coord_x">>, CharacterSpecs, none),proplists:get_value(<<"coord_y">>, CharacterSpecs, none)])
	},
	io:format("ct_player has started (~w)~n", [self()]),
    {ok, State}.
-spec stop() -> 'ok'.
stop() -> gen_server:cast(?MODULE, stop).
%% User Callbacks
-spec handle_cast('stop' | {'leave_denied'} | {'go',_} | {'seen',#player_state{}} | {'set_client',_} | {'set_feedback_fun',_} | {'set_room',nonempty_string()} | {'unseen',#player_state{},_} | {'entered',_,[any()],_},_) -> {'noreply',_} | {'stop','normal',_}.
handle_cast({go, Direction}, State) ->
	%Feedback=io_lib:format("Going ~p~n", [Direction]),
	%ct_client_command:send_feedback(State,Feedback),
    ct_room:request_leave(State#player_state.room,Direction,State),
    {noreply, State};
handle_cast({set_room, [X,Y]}, State) ->
	{ok,RoomPid}=ct_room_sup:get_pid([X,Y]),
	ct_room:enter(RoomPid, none, State, null),
	{noreply, State};
handle_cast({set_client,Client},State) ->
	NewState=State#player_state{client=Client},
	{noreply,NewState};
handle_cast({set_feedback_fun,Fun},State) ->
	NewState=State#player_state{feedback_fun=Fun},
	{noreply,NewState};
handle_cast({seen, OtherPlayer}, State) ->
	%%Decide wether to attack or not.
	io:format("~s: has been seen by ~s~n",[State#player_state.name,ct_player:get_name(OtherPlayer)]),
	FeedbackFun = State#player_state.feedback_fun,
	FeedbackFun(State,
		{obj,[{"type",<<"seen_by_info">>},
			{"body",{obj,[
				{"name",ct_player:get_name(OtherPlayer)},
				{"player_id",99} % TODO define a way of referring to players, objs, etc..
			]}}
		]}),
	{noreply,State};
handle_cast({unseen, OtherPlayer, Direction}, State) ->
	%%The player left the room
	%io:format("~s: no longer see ~s~n",[State#player_state.name,ct_player:get_name(OtherPlayer)]),
	FeedbackFun = State#player_state.feedback_fun,
	FeedbackFun(State,
		{obj,[{"type",<<"unseen_by_info">>},
			{"body",{obj,[
				{"name",ct_player:get_name(OtherPlayer)},
				{"player_id",99}, % TODO define a way of referring to players, objs, etc..
				{"direction", Direction}
			]}}
		]}),
	{noreply,State};
handle_cast({entered, RoomPid, RoomExits, RoomName}, State) ->
	NewState=State#player_state{room_exits=RoomExits,room=RoomPid},
	%ct_client_command:send_feedback(State,
	FeedbackFun = State#player_state.feedback_fun,
	FeedbackFun(State,
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
	FeedbackFun = State#player_state.feedback_fun,
	FeedbackFun(State,
		{obj,[{"type",<<"game_message_info">>},
			{"body",{obj,[
				{"msg_type",<<"warning">>},
				{"contents",<<"You hit a cold, hard wall">>}
			]}}
		]}),
	{noreply,State};
handle_cast(stop, State) -> {stop, normal, State}.

-spec handle_call({'get_handler'},_,_) -> {'reply',_,_}.
handle_call({get_handler},_From,State) -> 
	{reply,State,State}.

%% System callbacks
-spec terminate(_,_) -> {'ok',_}.
terminate(_Reason, State) -> {ok,State}.

-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec handle_info(_,_) -> {'noreply',_}.
handle_info( _, State) -> {noreply,State}.