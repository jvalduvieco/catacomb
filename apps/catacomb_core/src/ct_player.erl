-module(ct_player).
-behaviour(gen_server).

-export([start_link/1,stop/1]).
-export([get_handler/1,is_player/1,get_pid/1,get_name/1,get_max_life_points/1,get_life_points/1,get_client/1,set_client/2,set_feedback_fun/2,get_public_id/1]).
-export([go/2, set_room/2,seen/2,unseen/3,entered/4,leave_denied/1,talk/2,heard/3]).
-export([init/1,handle_cast/2,handle_call/3,terminate/2,code_change/3,handle_info/2]).

-include ("ct_character_info.hrl").
-include ("ct_player.hrl").

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
get_public_id(#player_state{public_id=PublicId} = _Player) ->
	PublicId.

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
set_feedback_fun(Player, Fun) ->
	gen_server:cast(ct_player:get_pid(Player), {set_feedback_fun, Fun}).
%% Events
entered(Player, RoomPid, RoomExits, RoomName) ->
	gen_server:cast(ct_player:get_pid(Player), {entered, RoomPid, RoomExits, RoomName}).
seen(Player,OtherPlayer) ->
	gen_server:cast(ct_player:get_pid(Player), {seen, OtherPlayer}).
unseen(Player,OtherPlayer,Direction) ->
	gen_server:cast(ct_player:get_pid(Player), {unseen, OtherPlayer, Direction}).
leave_denied(Player) ->
	gen_server:cast(ct_player:get_pid(Player),{leave_denied}).
stop(Player) ->
	gen_server:cast(ct_player:get_pid(Player),stop).
talk(Player, Message) ->
  gen_server:cast(ct_player:get_pid(Player),{talk, Message}).
heard(Player, PlayerWhoTalksName, Message) ->
  gen_server:cast(ct_player:get_pid(Player),{heard, PlayerWhoTalksName, Message}).

%% Internal functions
init([{obj,CharacterSpecs}]) ->
	State=#player_state{
		id=proplists:get_value(<<"id">>, CharacterSpecs, none),
		my_pid=self(),
		name=proplists:get_value(<<"name">>, CharacterSpecs, none),
		max_life_points=proplists:get_value(<<"max_life_points">>, CharacterSpecs, none),
		life_points=proplists:get_value(<<"life_points">>, CharacterSpecs, none),
		level=proplists:get_value(<<"level">>, CharacterSpecs, none),
		experience_points=proplists:get_value(<<"experience_points">>, CharacterSpecs, none),
		public_id=proplists:get_value(<<"public_id">>, CharacterSpecs, none)
		%room=ct_room_sup:get_pid([proplists:get_value(<<"coord_x">>, CharacterSpecs, none),proplists:get_value(<<"coord_y">>, CharacterSpecs, none)])
	},
	lager:info("ct_player has started (~w)~n", [self()]),
    {ok, State}.

%% User Callbacks
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
	lager:debug("~s: has been seen by ~s~n",[State#player_state.name,ct_player:get_name(OtherPlayer)]),
	
	FeedbackFun = State#player_state.feedback_fun,
	FeedbackFun(State,
		{obj,[{"type",<<"seen_by_info">>},
			{"body",{obj,[
				{"name",ct_player:get_name(OtherPlayer)},
				{"public_id",ct_player:get_public_id(OtherPlayer)}
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
				{"public_id",ct_player:get_public_id(OtherPlayer)}, 
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
	lager:debug("~s has hit with a wall ~n", [State#player_state.name]),
	
	FeedbackFun = State#player_state.feedback_fun,
	FeedbackFun(State,
		{obj,[{"type",<<"game_message_info">>},
			{"body",{obj,[
				{"msg_type",<<"warning">>},
				{"contents",<<"You hit a cold, hard wall">>}
			]}}
		]}),
	{noreply,State};
handle_cast({talk, Message},State) ->
	lager:debug("~s say ~s~n", [State#player_state.name, Message]),

  ct_room:chat_talk(State#player_state.room, State#player_state.name, Message),

	{noreply,State};
handle_cast({heard, PlayerWhoTalksName, Message},State) ->
  FeedbackFun = State#player_state.feedback_fun,
  FeedbackFun(State,
    {obj,[{"type",<<"room_chat_talk">>},
      {"body",{obj,[
        {"player_name", PlayerWhoTalksName},
        {"message", Message}
      ]}}
    ]}),
  {noreply,State};
handle_cast(stop, State) ->
	% leave the room
	ct_room:player_left(State#player_state.room,left_game,State),
	{stop, normal, State}.

handle_call({get_handler},_From,State) -> 
	{reply,State,State}.

%% System callbacks
terminate(_Reason, State) -> {ok,State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.
