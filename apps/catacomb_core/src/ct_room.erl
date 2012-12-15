-module(ct_room).
-behaviour(gen_server).

-export([start_link/2,stop/0]).
-export([enter/4, request_leave/3,get_exits/1,player_left/3,add_exit/4,chat_talk/3,add_object/3,pick_object/3]).
-export([relative_coords_to_absolute/5,find_neighbours_entrances/5]). %% REMOVEME When done
-export([init/1, handle_call/3,handle_cast/2,terminate/2,code_change/3,handle_info/2]).

%tmp
-export([print_exits/1]).

-include ("ct_player.hrl").

-record(state,{
	x,
	y,
	room_name,
	exits=[],
	players=[],
	objects=[],
	params=[],
  room_evm_pid,
  chat_players_pid=[]}).


start_link(X,Y) ->
    gen_server:start_link(?MODULE, {X,Y}, []).
%% Client API 
request_leave(RoomPid,Direction,Player)->
	% Cridem ct_room:leave(Direction,Player)
	case is_pid(RoomPid) of
		true ->
			gen_server:cast(RoomPid, {request_leave, Direction, Player});
		false ->
			{badarg,[]}
	end.
player_left(RoomFromPid, Direction, Player)->
	case is_pid(RoomFromPid) of
		true ->
			gen_server:cast(RoomFromPid, {player_left, Player, Direction});
		false ->
			{badarg,[]}
	end.

enter(RoomPid,Direction,Player,RoomFromPid) ->
	case is_pid(RoomPid) of
		true ->
			gen_server:cast(RoomPid, {enter, Player, RoomFromPid, Direction});
		false ->
			{badarg,[]}
	end.

get_exits(RoomPid) ->
	{Result,Data} = case is_pid(RoomPid) of
		true ->
			gen_server:call(RoomPid, {get_exits});
		false->
			{badarg,[]}
		end,
	{Result,Data}.
add_exit(RoomPid,Exit,X,Y)->
	case is_pid(RoomPid) of
		true ->
			gen_server:cast(RoomPid, {add_exit,Exit,X,Y});
		false ->
			{badarg,[]}
	end.
add_object(RoomPid,Player,Object) ->
	gen_server:cast(RoomPid, {add_object,Player,Object}).
pick_object(RoomPid,Player,ObjectId) ->
	gen_server:cast(RoomPid, {pick_object,Player,ObjectId}).
%% chat
chat_talk(RoomPid, PlayerWhoTalksName, Message) ->
  gen_server:cast(RoomPid, {chat_talk, PlayerWhoTalksName, Message}).

%tmp
print_exits(RoomPid)->
	case is_pid(RoomPid) of
		true ->
			gen_server:cast(RoomPid, {print_exits});
		false ->
			{badarg,[]}
	end.

%% Internal functions
init({X,Y}) ->
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A,B,C}),
  MaxX=ct_config_service:get_room_setup_max_x(),
  MaxY=ct_config_service:get_room_setup_max_y(),
	{RoomName, Props}=create_room_properties(),		%% Define Room properties and name.
	{ok,RoomExits}=create_room_exits(X,Y,MaxX,MaxY),

  % start room event manager
  {ok, RoomEventManagerPid} = gen_event:start_link(),

  % state
  State=#state{x=X, y=Y, room_name=RoomName, exits=RoomExits, params=Props, room_evm_pid=RoomEventManagerPid},
	ets:insert(coordToPid,{list_to_atom([X,Y]),self()}),
  {ok, State}.
stop() -> gen_server:cast({global,?MODULE}, stop).

%% Callbacks
%% Tool function to see which exits has a room
handle_call({get_exits},_From, State) ->
	{reply,{ok,State#state.exits},State}.
%% Tool function to add exits to a room
handle_cast({add_exit, Exit,X,Y}, State) ->
  NewExits=lists:sort(lists:append(State#state.exits,[{Exit,[X,Y]}])),
  NewState=State#state{exits=NewExits},
  {noreply, NewState};
%% Tool to print room exits
handle_cast({print_exits}, State) ->
  lager:debug("{~p,~p} :  ~p ~n",[State#state.x,State#state.y,State#state.exits]),
  {noreply, State};

%% A player enters into the room
handle_cast({enter, Player, RoomFromPid, Direction}, State) ->
  % tell the room the player is coming from that the player is no longer there
  case is_pid(RoomFromPid) of	true -> ct_room:player_left(RoomFromPid, Direction, Player); false -> true end,
  % Tell the player that she has entered into this room
  ct_player:entered(Player, self(), State#state.exits, State#state.room_name,State#state.objects),
  % Subscribe the player to room event handler
  HandlerId = {ct_room_events, ct_player:get_public_id(Player)},
  gen_event:add_sup_handler(State#state.room_evm_pid, HandlerId, [ct_player:get_feedback_data(Player)]),
  %% save entering player
  NewState=State#state{players=[Player|State#state.players]},
  % Notify that a player entered into the room
  %% Replace by room event handler?
  lists:map(fun(X) -> ct_player:seen(X,Player) end,State#state.players),
  lists:map(fun(X) -> ct_player:seen(Player,X) end,State#state.players),
  {noreply, NewState};

%% A player leaves the room
handle_cast({player_left, Player, Direction}, State) ->
  % Unsubscribe player from room event handler
  HandlerId = {ct_room_events, ct_player:get_public_id(Player)},
  ok=gen_event:delete_handler(State#state.room_evm_pid, HandlerId, []),
  %% Remove the player from players list
  NewState=State#state{players=[P || P <- State#state.players, ct_player:get_pid(P)=/=ct_player:get_pid(Player)]},
  % Replace by room event handler?
  lists:map(fun(X) -> ct_player:unseen(X,Player,Direction) end,NewState#state.players),
  lists:map(fun(X) -> if Player/=X -> ct_player:unseen(Player,X,none) end end,NewState#state.players),
	{noreply, NewState};

%% Before leaving the room the UI client asks the server for permission to leave
%% Server also checks if there is a door, notifies, the target room, etc..
handle_cast({request_leave, Direction, Player}, State) ->
	% Check that direction exists
	case [{Dir,Coords} || {Dir,Coords} <- State#state.exits, Direction=:=Dir] of
    	[{_,Coords}] -> 
			  % Obtain target room
    		{ok,RoomToPid}=ct_room_sup:get_pid(Coords),
			  % Notify target room that a player is entering
        % FIXME: Move to player?
			  ct_room:enter(RoomToPid,Direction,Player,self());
    	[] ->
    		ct_player:leave_denied(Player),
			true
	end,
	{noreply,State};

%% Somebody talks into the room
handle_cast({chat_talk, PlayerWhoTalksName, Message}, State) ->
  	lager:debug("cast room chat talk: ~p: ~p~n", [PlayerWhoTalksName, Message]),
  	gen_event:notify(State#state.room_evm_pid, {talk, PlayerWhoTalksName, Message}),
  	{noreply, State};

%% An object is dropped into the room by somebody
handle_cast({add_object,Player,Object},State) ->
	lager:debug("{~p,~p} cast add object: ~p ~n", [State#state.x, State#state.y, Object]),
  gen_event:notify(State#state.room_evm_pid, {object_dropped_by_player, Player, Object}),
	NewState=State#state{objects=[{proplists:get_value(id,Object),Object}]++State#state.objects},
	{noreply,NewState};

%% An object is picked from the room
handle_cast({pick_object,Player,ObjectId},State) ->
	Result=proplists:get_value(ObjectId,State#state.objects,none),
	NewState=case Result of 
		none ->
			ct_player:object_picked(Player,object_not_found),
			State;
		Object ->
			ct_player:object_picked(Player,Object),
      gen_event:notify(State#state.room_evm_pid, {object_picked_by_player, Player, Object}),
			State#state{objects=proplists:delete(ObjectId,State#state.objects)}
	end,
	{noreply,NewState};
handle_cast(stop, State) -> {stop, normal, State}.

%% System Callbacks
terminate(_Reason, State) -> {ok,State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.

%% Services
rooms() ->
    [{<<"Dark room">>, [{drop, {<<"Spider Egg">>, 1}}, {experience, 1}]},
     {<<"Lightly illuminated room">>, [{drop, {<<"Pelt">>, 1}}, {experience, 1}]},
     {<<"Dungeon">>, [{drop, {<<"Bacon">>, 1}}, {experience, 1}]},
     {<<"Semi flooded room">>, [{drop, {<<"Tasty Ribs">>, 2}}, {experience, 1}]},
     {<<"Torture room">>, [{drop, {<<"Goblin hair">>, 1}}, {experience, 2}]},
     {<<"Graveyard room">>, [{drop, {<<"Chunks of Metal">>, 3}}, {experience, 2}]},
     {<<"Mood filled room">>, [{drop, {<<"Wrench">>,2}}, {experience,1}]},
     {<<"Blood stained room">>, [{drop, {<<"Cotton Candy">>,1}}, {experience,1}]},
     {<<"Corpse room">>, [{drop, {<<"Wood chips">>, 2}}, {experience, 1}]},
     {<<"Old crypt">>, [{drop, {<<"Shiny things">>, 3}}, {experience, 1}]},
     {<<"Marble room">>, [{drop, {<<"Lizard tail">>, 1}}, {experience, 1}]},
     {<<"Dry rock room">>, [{drop, {<<"Fur">>, 3}}, {experience, 4}]},
     {<<"Stincky room">>, [{drop, {<<"Horseradish">>,1}}, {experience, 2}]},
     {<<"Snake room">>, [{drop, {<<"Spices">>,10}}, {experience, 25}]},
     {<<"Bug room">>, [{drop, {<<"Map">>, 2}}, {experience, 12}]},
     {<<"Waterfall room">>, [{drop, {<<"branch">>,1}}, {experience, 2}]},
     {<<"Smoky room">>, [{drop, {<<"Penguin Egg">>,1}}, {experience, 3}]}].

create_room_properties() ->
    L = rooms(),
    lists:nth(random:uniform(length(L)), L).
relative_coords_to_absolute(X,Y,MaxX,MaxY,Dir) ->
	case Dir of
		n->
			NewY = Y + 1,
			if 
				NewY > MaxY -> null;
				true -> [X,NewY]
			end;
		ne->
			NewY = Y + 1,
			NewX = X + 1,
			if 
				NewY > MaxY -> null;
				NewX > MaxX -> null;
				true -> [NewX,NewY]
			end;
		e->
			NewX = X + 1,
			if
				NewX > MaxX -> null;
				true -> [NewX,Y]
			end;
		se->
			NewY = Y - 1,
			NewX = X + 1,
			if 
				NewY < 1-> null;
				NewX > MaxX -> null;
				true -> [NewX,NewY]
			end;
		s->
			NewY = Y - 1,
			if 
				NewY < 1 -> null;
				true -> [X,NewY]
			end;
		sw->
			NewY = Y - 1,
			NewX = X - 1,
			if 
				NewY < 1 -> null;
				NewX < 1 -> null;
				true -> [NewX,NewY]
			end;
		w->
			NewX = X - 1,
			if
				NewX < 1 -> null;
				true -> [NewX,Y]
			end;
		nw->
			NewY = Y + 1,
			NewX = X - 1,
			if 
				NewY > MaxY -> null;
				NewX < 1 -> null;
				true -> [NewX,NewY]
			end
		end.	
clean_list (List) -> 
	lists:filter(
		fun(Z) -> Z/=null end, List).
clean_exits (List) ->
	lists:filter(
		fun({_,Z}) -> Z/=null end, List).
find_neighbours_entrances(X,Y,MaxX,MaxY,RandomExits) -> 
	%% Ask our neighbours for exits to this rooms. We only ask our left w sw and s neighbours
	%% as map generation is from bottom left to up right.
	CheckNeighList=[w,sw,s,nw],
	NeighExits = lists:map(
		fun(Dir) ->
			case relative_coords_to_absolute(X,Y,MaxX,MaxY,Dir) of
				null -> null;
				Coords ->
					{ok,NeighRoom}=ct_room_sup:get_pid(Coords),
					{ok,NeighExits}=ct_room:get_exits(NeighRoom),
						case Dir of
							w ->
								case lists:member(e,[W||{W,_}<-NeighExits]) of
									true -> w;
									false ->
										case lists:member(w,[Z||Z<-RandomExits]) of
											true -> ct_room:add_exit(NeighRoom,e,X,Y),
												null;
											false ->null
										end
								end;
							sw->
								case lists:member(ne,[W||{W,_}<-NeighExits]) of
									true -> sw;
									false ->
										case lists:member(sw,[Z||Z<-RandomExits]) of
											true -> ct_room:add_exit(NeighRoom,ne,X,Y),
												null;
											false ->null
										end
								end;
							nw->
								case lists:member(se,[W||{W,_}<-NeighExits]) of
									true -> nw;
									false ->
										case lists:member(nw,[Z||Z<-RandomExits]) of
											true -> ct_room:add_exit(NeighRoom,se,X,Y),
												null;
											false ->null
										end
								end;
							s->
								case lists:member(n,[W||{W,_}<-NeighExits]) of
									true -> s;
									false ->
										case lists:member(s,[Z||Z<-RandomExits]) of
											true -> ct_room:add_exit(NeighRoom,n,X,Y),
												null;
											false ->null
										end
								end
						end
					
			end
		end
		, CheckNeighList),
	clean_list (NeighExits).

create_room_exits(X,Y,MaxX,MaxY)->
	PossibleExits=[n,ne,e,se,s,sw,w,nw],
	RandomExits=clean_list (
		lists:map(
			fun(Coord) -> case random:uniform(3) of 1 -> Coord; _-> null end end, 
			PossibleExits) ),
	NeighExits=find_neighbours_entrances(X,Y,MaxX,MaxY,RandomExits),
	%% We have a list of exits cardinal point [s,w,n]
	Exits=clean_exits(translate(X,Y,MaxX,MaxY,lists:umerge(lists:sort(RandomExits),lists:sort(NeighExits)))),
	{ok,Exits}.
translate(_,_,_,_,[]) -> [];
translate(X,Y,MaxX,MaxY,[Element | RoomList]) ->
	[{Element,relative_coords_to_absolute(X,Y,MaxX,MaxY,Element)}|translate(X,Y,MaxX,MaxY,RoomList)].
