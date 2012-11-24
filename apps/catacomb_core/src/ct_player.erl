-module(ct_player).
-behaviour(gen_server).

-export([start_link/1,stop/1]).
-export([get_handler/1,is_player/1,get_pid/1,get_name/1,get_max_life_points/1,get_life_points/1,get_client/1,set_client/2,set_feedback_fun/2,get_public_id/1]).
-export([go/2, set_room/2,seen/2,unseen/3,entered/5,leave_denied/1,talk/2,heard/3,hit/4,attack/2,pick_object/2,object_picked/2,drop_object/2,wear/2,unwear/3]).
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
hit(Player,OtherPlayer,HitChance,BattleStats) ->
	gen_server:cast(ct_player:get_pid(Player),{hit, OtherPlayer, HitChance, BattleStats}).
attack(Player,OtherPlayer) ->
	gen_server:cast(ct_player:get_pid(Player),{attack, OtherPlayer}).

%% Events
entered(Player, RoomPid, RoomExits, RoomName,RoomObjects) ->
	gen_server:cast(ct_player:get_pid(Player), {entered, RoomPid, RoomExits, RoomName,RoomObjects}).
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
pick_object(Player, ObjectId) ->
	gen_server:cast(ct_player:get_pid(Player),{pick_object, ObjectId}).
object_picked(Player,Object) ->
	gen_server:cast(ct_player:get_pid(Player),{object_picked, Object}).
drop_object(Player,ObjectId) ->
	gen_server:cast(ct_player:get_pid(Player),{drop_object, ObjectId}).
wear(Player,ObjectId) ->
  gen_server:cast(ct_player:get_pid(Player),{wear, ObjectId}).
unwear(Player,ObjectId,Position) ->
  gen_server:cast(ct_player:get_pid(Player),{unwear, ObjectId,Position}).
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
		experience_points=proplists:get_value(<<"experience_points">>, CharacterSpecs, none),
		public_id=proplists:get_value(<<"public_id">>, CharacterSpecs, none),
    battle_stats=[{total_armor,[{none,0}]},{total_damage,[{none,0}]}]
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
	CleanOtherPlayer=OtherPlayer#player_state{located_players=[]},
	NewState=State#player_state{located_players=[CleanOtherPlayer|State#player_state.located_players]},
	lager:debug("~s: has been seen by ~s~n",[State#player_state.name,ct_player:get_name(OtherPlayer)]),
	
	FeedbackFun = State#player_state.feedback_fun,
	FeedbackFun(State,
		{obj,[{"type",<<"seen_by_info">>},
			{"body",{obj,[
				{"name",ct_player:get_name(OtherPlayer)},
				{"public_id",ct_player:get_public_id(OtherPlayer)}
			]}}
		]}),
	{noreply,NewState};
handle_cast({unseen, OtherPlayer, Direction}, State) ->
	%%The player left the room
	%io:format("~s: no longer see ~s~n",[State#player_state.name,ct_player:get_name(OtherPlayer)]),
	NewState=State#player_state{located_players=[P || P <- State#player_state.located_players, ct_player:get_pid(P)=/=ct_player:get_pid(OtherPlayer)]},
	FeedbackFun = State#player_state.feedback_fun,
	FeedbackFun(State,
		{obj,[{"type",<<"unseen_by_info">>},
			{"body",{obj,[
				{"name",ct_player:get_name(OtherPlayer)},
				{"public_id",ct_player:get_public_id(OtherPlayer)}, 
				{"direction", Direction}
			]}}
		]}),
	{noreply,NewState};
handle_cast({entered, RoomPid, RoomExits, RoomName,RoomObjects}, State) ->
	NewState=State#player_state{room_exits=RoomExits,room=RoomPid},
	%ct_client_command:send_feedback(State,
	FeedbackFun = State#player_state.feedback_fun,
	FeedbackFun(State,
		{obj,[{"type",<<"room_info">>},
			{"body",{obj,[
				{"name",RoomName},
				{"exits",[X || {X,_} <- RoomExits]},
				{"objects",[ {obj,X} || {_,X} <- RoomObjects]} % FIXME create a new message to report objects appeared after entering the room
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
handle_cast({hit, OtherPlayer, _HitChance, BattleStats}, State) ->
  % If player dice is lower than hit chance player hits opponent
	%case random:uniform(100) > HitChance of
	%	false ->
			case random:uniform(100) > State#player_state.dodge_chance of
				true -> 
					Damage=fight_result(BattleStats,State#player_state.battle_stats),
					NewLifePoints = State#player_state.life_points - Damage,
					case NewLifePoints =< 0 of
						true ->
							FeedbackFun = State#player_state.feedback_fun,
							FeedbackFun(State,
								{obj,[{"type",<<"attack_info">>},
									{"body",{obj,[
										{"msg_type",<<"dead">>},
										{"otherplayer",OtherPlayer#player_state.name},
										{"damage",Damage}
									]}}
								]}),
							OtherFeedbackFun = OtherPlayer#player_state.feedback_fun,
							OtherFeedbackFun(OtherPlayer,
								{obj,[{"type",<<"attack_info">>},
									{"body",{obj,[
										{"msg_type",<<"otherdead">>},
										{"otherplayer",State#player_state.name},
										{"damage",Damage}
									]}}
								]}),

							NewState=State#player_state{life_points=NewLifePoints};
							%FALTA eliminar el jugador!!
						false ->
							FeedbackFun = State#player_state.feedback_fun,
							FeedbackFun(State,
								{obj,[{"type",<<"attack_info">>},
									{"body",{obj,[
										{"msg_type",<<"hitted">>},
										{"otherplayer",OtherPlayer#player_state.name},
										{"damage",Damage}
									]}}
								]}),
							OtherFeedbackFun = OtherPlayer#player_state.feedback_fun,
							OtherFeedbackFun(OtherPlayer,
								{obj,[{"type",<<"attack_info">>},
									{"body",{obj,[
										{"msg_type",<<"otherhitted">>},
										{"otherplayer",State#player_state.name},
										{"damage",Damage}
									]}}
								]}),

							NewState=State#player_state{life_points=NewLifePoints}
				    end;
        false ->
					NewState=State,
					%io:format("~p tried to hit ~p but ~p dodged. ~n",[OtherPlayer#player_state.name,State#player_state.name,State#player_state.name]),
					FeedbackFun = State#player_state.feedback_fun,
					FeedbackFun(State,
						{obj,[{"type",<<"attack_info">>},
							{"body",{obj,[
								{"msg_type",<<"dodged">>},
								{"otherplayer",OtherPlayer#player_state.name}
							]}}
						]}),
					OtherFeedbackFun = OtherPlayer#player_state.feedback_fun,
					OtherFeedbackFun(OtherPlayer,
						{obj,[{"type",<<"attack_info">>},
							{"body",{obj,[
								{"msg_type",<<"otherdodged">>},
								{"otherplayer",State#player_state.name}
							]}}
						]})
			end,
		%true ->
		%	NewState=State,
		%	%io:format("~p failed tying to hit ~p. ~n",[OtherPlayer#player_state.name,State#player_state.name]),
		%	FeedbackFun = State#player_state.feedback_fun,
		%	FeedbackFun(State,
%% 				{obj,[{"type",<<"attack_info">>},
%% 					{"body",{obj,[
%% 						{"msg_type",<<"failed">>},
%% 						{"otherplayer",OtherPlayer#player_state.name}
%% 					]}}
%% 				]}),
%% 			OtherFeedbackFun = OtherPlayer#player_state.feedback_fun,
%% 			OtherFeedbackFun(OtherPlayer,
%% 				{obj,[{"type",<<"attack_info">>},
%% 					{"body",{obj,[
%% 						{"msg_type",<<"otherfailed">>},
%% 						{"otherplayer",OtherPlayer#player_state.name}
%% 					]}}
%% 				]})

%%	end,

	{noreply, NewState};
handle_cast({attack, OtherPlayer}, State) ->
  ct_player:hit(OtherPlayer,State,State#player_state.hit_chance,State#player_state.battle_stats),
	{noreply, State};

handle_cast({pick_object,ObjectId},State) ->
	ct_room:pick_object(State#player_state.room,State,ObjectId),
	{noreply,State};
handle_cast({object_picked,Object},State) ->
	NewState=State#player_state{inventory=[{proplists:get_value(id,Object),Object}]++State#player_state.inventory},
	FeedbackFun = State#player_state.feedback_fun,
  	FeedbackFun(State,
    {obj,[{"type",<<"object_picked">>},
      {"body",{obj,Object}}
    ]}),
    {noreply,NewState};
handle_cast({drop_object,ObjectId},State) ->
	FeedbackFun = State#player_state.feedback_fun,
	Result=proplists:get_value(ObjectId,State#player_state.inventory,none),
	NewState=case Result of 
		none ->
			FeedbackFun(State,
				{obj,[{"type",<<"object_dropped">>},
					{"body",object_not_found}]}),
			State;
		Object ->
      ct_room:add_object(State#player_state.room,Object),
			FeedbackFun(State,
				{obj,[{"type",<<"object_dropped">>},
				{"body",{obj,[{"object_id",ObjectId}]}}]}),
			State#player_state{inventory=proplists:delete(ObjectId,State#player_state.inventory)}
	end,
	{noreply,NewState};
handle_cast({wear,ObjectId},State) ->
  FeedbackFun = State#player_state.feedback_fun,
  lager:debug("wear ~p ~p ~p ~p",[ObjectId,State#player_state.worn_objects,State#player_state.inventory]),
  NewState=case proplists:get_value(ObjectId,State#player_state.inventory,none) of
    none ->
      FeedbackFun(State,
        {obj,[{"type",<<"object_worn">>},
          {"body",object_not_found}]}),
      State;
    Object ->
      Wearing=proplists:get_value(wearing,Object),
      % Save object worn in new object position
      OldObject= proplists:get_value(Wearing,State#player_state.worn_objects,[]),
      % remove old object from worn objects
      CleanWornObjects=proplists:delete(Wearing,State#player_state.worn_objects),
      % Add new object to worn objects
      WornObjects=CleanWornObjects ++ [{Wearing,Object}],
      % Remove worn object from inventory
      NewInventory=proplists:delete(ObjectId,State#player_state.inventory),
      % Calculate Damage and Armor
      BattleStats=calculate_battle_stats(WornObjects),
      % Send some feedback to the user
      FeedbackFun(State,
        {obj,[{"type",<<"object_worn">>},
          {"body",{obj,Object}}]}),
      % Add old worn object to inventory and update list of worn objects
      State#player_state{
        worn_objects=WornObjects,
        inventory=OldObject++NewInventory,
        battle_stats=BattleStats}
  end,
  {noreply,NewState};
handle_cast({unwear,ObjectId,Position},State) ->
  lager:debug("unwear ~p ~p ~p ~p",[ObjectId,Position,State#player_state.worn_objects,State#player_state.inventory]),
  FeedbackFun = State#player_state.feedback_fun,
  Result=proplists:get_value(Position,State#player_state.worn_objects,none),
  NewState=case Result of
    none ->
      FeedbackFun(State,
        {obj,[{"type",<<"object_unworn">>},
          {"body",object_not_found}]}),
      State;
    Object ->
      % remove from worn objects
      WornObjects=proplists:delete(Position,State#player_state.worn_objects),
      NewInventory=State#player_state.inventory ++ [{ObjectId,Object}],

      % Calculate Damage and Armor
      BattleStats=calculate_battle_stats(WornObjects),
      % Send some feedback to the user
      FeedbackFun(State,
        {obj,[{"type",<<"object_unworn">>},
          {"body",{obj,Object}}]}),
      % Add old worn object to inventory and update list of worn objects
      State#player_state{worn_objects=WornObjects,inventory=NewInventory,battle_stats=BattleStats}

  end,
  {noreply,NewState};

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

clean_obj(X) when is_tuple(X)->
  {obj,Result}=X,
  Result;
clean_obj(X) ->
  X.
calculate_battle_stats(WornObjects) ->
  % Get all worn objects damage. list of tuples by damage type per object all in one list
  ObjectsDamage=lists:flatten([clean_obj(proplists:get_value(damage,X,{obj,[{none, 0}]}))||{_,X}<-WornObjects]),
  % Get all worn objects armor. list of tuples by armor type per object all in one list
  ObjectsArmor=lists:flatten([clean_obj(proplists:get_value(armor,X,{obj,[{none, 0}]}))||{_,X}<-WornObjects]),
  % Sum all Damage by type. A list of tuples by damage. Only one tuple per damage type
  TotalDamage=[{X,lists:foldl(fun({_X,Value},Sum)-> Sum + Value end,0,proplists:lookup_all(X,ObjectsDamage))}||X<-proplists:get_keys(ObjectsDamage)],
  % Sum all armor by type. The same as damage
  TotalArmor=[{X,lists:foldl(fun({_X,Value},Sum)-> Sum + Value end,0,proplists:lookup_all(X,ObjectsArmor))}||X<-proplists:get_keys(ObjectsArmor)],
  lager:debug("total damage: ~p, total armor: ~p",[TotalDamage,TotalArmor]),
  [{total_armor,TotalArmor},{total_damage,TotalDamage}].

fight_result(AttackerBattleStats, MyBattleStats)->
  MyArmor=proplists:get_value(total_armor,MyBattleStats),
  % Damage - Armor = Damage received
  % Take every Damage attacker can do and substract armor on that type of damage
  DamageList=[Value-proplists:get_value(Type,MyArmor,0)||{Type,Value}<-proplists:get_value(total_damage,AttackerBattleStats)],
  % Sum all positive damages
  lists:foldl(fun(X, Sum) -> case X >0 of true -> X+Sum;_ -> Sum end end,  0, DamageList).