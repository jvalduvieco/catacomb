-module(ct_room).
-behaviour(gen_server).

-export([start_link/2,stop/0]).
-export([enter/3, request_leave/3,get_exits/1,player_left/2]).
-export([relative_coords_to_absolute/5,find_neighbours_entrances/4]). %% REMOVEME When done
-export([init/1, handle_call/3,handle_cast/2,terminate/2,code_change/3,handle_info/2]).

-record(state,{
	x,
	y,
	room_name,
	exits=[],
	players=[],
	objects=[],
	params=[]}).


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
player_left(RoomFromPid, Player)->
	case is_pid(RoomFromPid) of
		true ->
			gen_server:cast(RoomFromPid, {player_left, Player});
		false ->
			{badarg,[]}
	end.

enter(RoomPid,Player,RoomFromPid) ->
	case is_pid(RoomPid) of
		true ->
			gen_server:cast(RoomPid, {enter, Player, RoomFromPid});
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
%% Internal functions
init({X,Y}) ->
<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    MaxX=ct_config:get_room_setup_max_x(),
    MaxY=ct_config:get_room_setup_max_y(),
	{RoomName, Props}=create_room_properties(),		%% Define Room properties and name.
	{ok,RoomExits}=create_room_exits(X,Y,MaxX,MaxY),
	State=#state{x=X,y=Y,room_name=RoomName,exits=RoomExits,params=Props},
	ets:insert(coordToPid,{list_to_atom([X,Y]),self()}),
    {ok, State}.
stop() -> gen_server:cast({global,?MODULE}, stop).

%% Callbacks
handle_call({get_exits},_From, State) ->
	{reply,{ok,State#state.exits},State}.
handle_cast({enter, Player, RoomFromPid}, State) ->
    NewState=State#state{players=[Player|State#state.players]},
    case is_pid(RoomFromPid) of	true -> ct_room:player_left(RoomFromPid, Player); false -> true end,
    ct_player:entered(Player, self(), State#state.exits, State#state.room_name),
	% Notificar players de la room que hi ha un nou player
    %lists:map(fun(X) -> io:format("~w is here.~n",[X]) end,State#state.players),
    %% Replace by room event handler?
    lists:map(fun(X) -> ct_player:seen(X,Player) end,State#state.players),
    lists:map(fun(X) -> ct_player:seen(Player,X) end,State#state.players),
    {noreply, NewState};
handle_cast({player_left, Player}, State) ->
	%% Check if player is really in
	NewState=State#state{players=[P || P <- State#state.players, ct_player:get_pid(P)=/=ct_player:get_pid(Player)]},
	%% Replace by room event handler?
    lists:map(fun(X) -> ct_player:unseen(X,Player) end,NewState#state.players),
    lists:map(fun(X) -> ct_player:unseen(Player,X) end,NewState#state.players),
	{noreply, NewState};
handle_cast({request_leave, Direction, Player}, State) ->
	%controlar que es pugui anar en la direcció
	case [{Dir,Coords} || {Dir,Coords} <- State#state.exits, Direction=:=Dir] of
    	[{_,Coords}] -> 
			%obtenir la room destí
    		{ok,RoomToPid}=ct_room_sup:get_pid(Coords),
			%notificar ala habitació desti que el player entra
			ct_room:enter(RoomToPid,Player,self());
    	[] ->
    		ct_player:leave_denied(Player),
			true
	end,
	{noreply,State};
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
find_neighbours_entrances(X,Y,MaxX,MaxY) -> 
	%% Ask our neighbours for exits to this rooms. We only ask our left w sw and s neighbours
	%% as map generation is from bottom left to up right.
	CheckNeighList=[w,sw,s],
	NeighExits = lists:map(
		fun(Dir) ->
			case relative_coords_to_absolute(X,Y,MaxX,MaxY,Dir) of
				null -> null;
				Coords ->
					{ok,NeighRoom}=ct_room_sup:get_pid(Coords),
					{ok,NeighExits}=ct_room:get_exits(NeighRoom),
					case length(NeighExits) of
						0 ->
							null;
						N when N>0 -> 
							case Dir of
								w ->
									case lists:member(e,[W||{W,_}<-NeighExits]) of
										true -> w;
										false ->null
									end;
								sw->
									case lists:member(ne,[W||{W,_}<-NeighExits]) of
										true -> sw;
										false ->null
									end;
								s->
									case lists:member(n,[W||{W,_}<-NeighExits]) of
										true -> s;
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
	NeighExits=find_neighbours_entrances(X,Y,MaxX,MaxY),
	%% We have a list of exits cardinal point [s,w,n]
	%%io:format("Exits (~w): ~w ~n",[[X,Y],lists:umerge(lists:sort(RandomExits),lists:sort(NeighExits))]),
	Exits=clean_exits(translate(X,Y,MaxX,MaxY,lists:umerge(lists:sort(RandomExits),lists:sort(NeighExits)))),
	{ok,Exits}.
translate(_,_,_,_,[]) -> [];
translate(X,Y,MaxX,MaxY,[Element | RoomList]) ->
	[{Element,relative_coords_to_absolute(X,Y,MaxX,MaxY,Element)}|translate(X,Y,MaxX,MaxY,RoomList)].