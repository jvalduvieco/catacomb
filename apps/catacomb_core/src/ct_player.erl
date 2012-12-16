-module(ct_player).
-behaviour(gen_server).

-export([start_link/1,stop/1]).
-export([get_handler/1,is_player/1,get_pid/1,get_name/1,get_max_life_points/1,get_life_points/1,get_client/1,set_client/2,set_feedback_data/2,get_feedback_data/1,get_public_id/1,heartbeat/2]).
-export([go/2, set_room/2,seen/2,unseen/3,entered/6,leave_denied/1,talk/2,heard/3,hit/4,attack/2,pick_object/2,object_picked/2,drop_object/2,wear/2,unwear/3]).
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
%% Tooling functions
%% To be called when the user is created
set_room(Player,[X,Y]) ->
  gen_server:cast(ct_player:get_pid(Player), {set_room, [X,Y]}).
%% Link player to the websocket server
set_client(Player,Client) ->
  gen_server:cast(ct_player:get_pid(Player), {set_client, Client}).
%% Set feedback data
set_feedback_data(Player,FeedbackData) ->
  gen_server:cast(ct_player:get_pid(Player), {set_feedback_data, FeedbackData}).
get_feedback_data(#player_state{feedback_data=FeedbackData} = _Player) ->
  FeedbackData.

%% Client API
go(Player,Direction) ->
    gen_server:cast(ct_player:get_pid(Player), {go, Direction}).
hit(Player,OtherPlayer,HitChance,BattleStats) ->
	gen_server:cast(ct_player:get_pid(Player),{hit, OtherPlayer, HitChance, BattleStats}).
attack(Player,OtherPlayer) ->
	gen_server:cast(ct_player:get_pid(Player),{attack, OtherPlayer}).
talk(Player, Message) ->
  gen_server:cast(ct_player:get_pid(Player),{talk, Message}).
pick_object(Player, ObjectId) ->
  gen_server:cast(ct_player:get_pid(Player),{pick_object, ObjectId}).
drop_object(Player,ObjectId) ->
  gen_server:cast(ct_player:get_pid(Player),{drop_object, ObjectId}).
wear(Player,ObjectId) ->
  gen_server:cast(ct_player:get_pid(Player),{wear, ObjectId}).
unwear(Player,ObjectId,Position) ->
  gen_server:cast(ct_player:get_pid(Player),{unwear, ObjectId,Position}).
stop(Player) ->
  gen_server:cast(ct_player:get_pid(Player),stop).

%% Events
entered(Player, RoomPid, RoomExits, RoomName, RoomObjects, RoomPlayers) ->
	gen_server:cast(ct_player:get_pid(Player), {entered, RoomPid, RoomExits, RoomName,RoomObjects,RoomPlayers}).
seen(Player,OtherPlayer) ->
	gen_server:cast(ct_player:get_pid(Player), {seen, OtherPlayer}).
unseen(Player,OtherPlayer,Direction) ->
	gen_server:cast(ct_player:get_pid(Player), {unseen, OtherPlayer, Direction}).
leave_denied(Player) ->
	gen_server:cast(ct_player:get_pid(Player),{leave_denied}).
heard(Player, PlayerWhoTalksName, Message) ->
  gen_server:cast(ct_player:get_pid(Player),{heard, PlayerWhoTalksName, Message}).
heartbeat(Player, LastTimeDiff) ->
  gen_server:cast(ct_player:get_pid(Player),{heartbeat, LastTimeDiff}).
object_picked(Player,Object) ->
	gen_server:cast(ct_player:get_pid(Player),{object_picked, Object}).

%% Internal functions
init([{obj,CharacterSpecs}]) ->
  lager:debug("starting heartbeat process..."),
  HeartbeatPid = case ct_config_service:lookup(heartbeat_enabled) of
    true ->
      {ok,Pid} = ct_player_heartbeat:start_link(#player_state{my_pid=self()}),
      lager:debug("heartbeat process started ~p", [Pid]),
      Pid;
    false ->
      lager:debug("Not starting heartbeat process"),
      none
  end,

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
 		heartbeat_pid=HeartbeatPid,
    battle_stats=[{total_armor,[{none,0}]},{total_damage,[{none,0}]}]
	},

  lager:info("ct_player has started (~w)~n", [self()]),
  {ok, State}.

%% Client API Callbacks
%% Tools
handle_cast({set_room, [X,Y]}, State) ->
	{ok,RoomPid}=ct_room_sup:get_pid([X,Y]),
	ct_room:enter(RoomPid, none, State, null),
	{noreply, State};
handle_cast({set_client,Client},State) ->
	NewState=State#player_state{client=Client},
	{noreply,NewState};

handle_cast({set_feedback_data,FeedbackData},State) ->
  NewState=State#player_state{feedback_data=FeedbackData},
  {noreply,NewState};

%% The user wants to move the player
handle_cast({go, Direction}, State) ->
  ct_room:request_leave(State#player_state.room,Direction,State),
  {noreply, State};

%% The player is seen by another player
handle_cast({seen, OtherPlayer}, State) ->
	CleanOtherPlayer=OtherPlayer#player_state{located_players=[]},
	NewState=State#player_state{located_players=[CleanOtherPlayer|State#player_state.located_players]},
	lager:debug("~s: has been seen by ~s~n",[State#player_state.name,ct_player:get_name(OtherPlayer)]),
	ct_feedback:send(
    {obj,[{"type",<<"seen_by_info">>},
          {"body",{obj,[
            {"name",ct_player:get_name(OtherPlayer)},
            {"public_id",ct_player:get_public_id(OtherPlayer)}
          ]}}
    ]},
    State#player_state.feedback_data),
	{noreply,NewState};

%% Another player does not see this one
handle_cast({unseen, OtherPlayer, Direction}, State) ->
	%%The player left the room
	NewState=State#player_state{located_players=[P || P <- State#player_state.located_players, ct_player:get_pid(P)=/=ct_player:get_pid(OtherPlayer)]},
  ct_feedback:send(
		{obj,[{"type",<<"unseen_by_info">>},
			{"body",{obj,[
				{"name",ct_player:get_name(OtherPlayer)},
				{"public_id",ct_player:get_public_id(OtherPlayer)}, 
				{"direction", Direction}
			]}}
		]},
    State#player_state.feedback_data),
	{noreply,NewState};

%% The player has entered into a room
handle_cast({entered, RoomPid, RoomExits, RoomName, RoomObjects, RoomPlayers}, State) ->
	NewState=State#player_state{room_exits=RoomExits,room=RoomPid},
  ct_feedback:send(
		{obj,[{"type",<<"room_info">>},
			{"body",{obj,[
				{"name",RoomName},
				{"exits",[X || {X,_} <- RoomExits]},
				{"objects",[ {obj,X} || {_,X} <- RoomObjects]},
        {"players",[ {obj,X} || {_,X} <- RoomPlayers]}
			]}}
		]},
    State#player_state.feedback_data),
	{noreply, NewState};

%% Notification, a user tried to leave the room by a direction without door
handle_cast({leave_denied},State) ->
	lager:debug("~s has hit with a wall ~n", [State#player_state.name]),
  ct_feedback:send(
		{obj,[{"type",<<"game_message_info">>},
			{"body",{obj,[
				{"msg_type",<<"warning">>},
				{"contents",<<"You hit a cold, hard wall">>}
			]}}
		]},
  State#player_state.feedback_data),
	{noreply,State};

%% Say something to the room
handle_cast({talk, Message},State) ->
	lager:debug("~s say ~s~n", [State#player_state.name, Message]),
  ct_room:chat_talk(State#player_state.room, State#player_state.name, Message),
	{noreply,State};
%% Receive a message from other player
%handle_cast({heard, PlayerWhoTalksName, Message},State) ->
%  ct_feedback:send(
%    {obj,[{"type",<<"room_chat_talk">>},
%      {"body",{obj,[
%        {"player_name", PlayerWhoTalksName},
%        {"message", Message}
%      ]}}
%    ]},
%    State#player_state.feedback_data),
%  {noreply,State};
%% Collect heartbeat info from client
%% Fixme: Move to session or tooling module

%% Heartbeats, Ui sends heardbeats to the player
handle_cast({heartbeat, LastTimeDiff},State) ->
  lager:debug("heartbeat last time diff: ~p ms~n", [LastTimeDiff]),
  % heartbeat response to cli
  ct_feedback:send(
    {obj,[{"type",<<"heartbeat_response">>},
      {"body",{obj,[]}}
    ]},
    State#player_state.feedback_data
  ),
  % heartbeat
  %ct_player_heartbeat:heartbeat(State#player_state.heartbeat_pid, LastTimeDiff),
  {noreply, State};

%% Attack another player
handle_cast({attack, OtherPlayer}, State) ->
  % Hit target player
  ct_player:hit(OtherPlayer,State,State#player_state.hit_chance,State#player_state.battle_stats),
	{noreply, State};

%% A hit is received
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
          ct_feedback:send(
            {obj,[{"type",<<"attack_info">>},
              {"body",{obj,[
                {"msg_type",<<"dead">>},
                {"otherplayer",OtherPlayer#player_state.name},
                {"damage",Damage}
              ]}}
            ]},
            State#player_state.feedback_data),
          ct_feedback:send(
            {obj,[{"type",<<"attack_info">>},
              {"body",{obj,[
                {"msg_type",<<"otherdead">>},
                {"otherplayer",State#player_state.name},
                {"damage",Damage}
              ]}}
            ]},
            State#player_state.feedback_data),

          NewState=State#player_state{life_points=NewLifePoints};
        %TODO: Kill the player
        false ->
          ct_feedback:send(
            {obj,[{"type",<<"attack_info">>},
              {"body",{obj,[
                {"msg_type",<<"hitted">>},
                {"otherplayer",OtherPlayer#player_state.name},
                {"damage",Damage}
              ]}}
            ]},
          State#player_state.feedback_data),
          ct_feedback:send(
            {obj,[{"type",<<"attack_info">>},
              {"body",{obj,[
                {"msg_type",<<"otherhitted">>},
                {"otherplayer",State#player_state.name},
                {"damage",Damage}
              ]}}
            ]},
          get_feedback_data(OtherPlayer)),

          NewState=State#player_state{life_points=NewLifePoints}
      end;
    false ->
      NewState=State,
      ct_feedback:send(
        {obj,[{"type",<<"attack_info">>},
          {"body",{obj,[
            {"msg_type",<<"dodged">>},
            {"otherplayer",OtherPlayer#player_state.name}
          ]}}
        ]},
        State#player_state.feedback_data),
      ct_feedback:send(
        {obj,[{"type",<<"attack_info">>},
          {"body",{obj,[
            {"msg_type",<<"otherdodged">>},
            {"otherplayer",State#player_state.name}
          ]}}
        ]},
        get_feedback_data(OtherPlayer))
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

%% Ask the room to pick an object, room controls objects to avoid race conditions
handle_cast({pick_object,ObjectId},State) ->
	ct_room:pick_object(State#player_state.room,State,ObjectId),
	{noreply,State};

%% Notification that an object is picked from the room
handle_cast({object_picked,Object},State) ->
	NewInventory= [{proplists:get_value(id,Object),Object}]++State#player_state.inventory,
  ct_feedback:send(
    {obj,[{"type",<<"object_picked">>},
      {"body",{obj,[
      {"object",{obj,Object}}]}}
    ]},
    State#player_state.feedback_data),
  {noreply,State#player_state{inventory=NewInventory}};

%% Drop an object form the inventory to the room
handle_cast({drop_object,ObjectId},State) ->
  % Check if the object is in the inventory
	Result=proplists:get_value(ObjectId,State#player_state.inventory,none),
	NewState=case Result of 
		none ->
      ct_feedback:send(
				{obj,[{"type",<<"object_dropped">>},
					{"body",object_not_found}]},
        State#player_state.feedback_data),
			State;
		Object ->
      %remove the object from the inventory
      NewInventory=proplists:delete(ObjectId,State#player_state.inventory),
      % return the object to the room
      ct_room:add_object(State#player_state.room,State,Object),
      ct_feedback:send(
				{obj,[{"type",<<"object_dropped">>},
				{"body",{obj,[{"object_id",ObjectId}]}}]},
        State#player_state.feedback_data),
			State#player_state{inventory=NewInventory}
	end,
	{noreply,NewState};

%% Wear an object from inventory
handle_cast({wear,ObjectId},State) ->
  lager:debug("wear ~p ~p ~p ~n",[ObjectId,State#player_state.worn_objects,State#player_state.inventory]),
  % Check if the object is in the inventory
  NewState=case proplists:get_value(ObjectId,State#player_state.inventory,none) of
    none ->
      ct_feedback:send(
        {obj,[{"type",<<"object_worn">>},
          {"body",object_not_found}]},
        State#player_state.feedback_data),
      State;
    Object ->
      % Find where the new object is to be worn
      Wearing=proplists:get_value(wearing,Object),
      % Save currently worn object on that position
      OldObject= proplists:get_value(Wearing,State#player_state.worn_objects,[]),
      % Return old worn object to inventory
      NewInventory=[{proplists:get_value(id,OldObject),OldObject}]++State#player_state.inventory,
      % remove object from worn objects
      CleanWornObjects=proplists:delete(Wearing,State#player_state.worn_objects),
      % Add new object to worn objects
      WornObjects=CleanWornObjects ++ [{Wearing,Object}],
      % Remove worn object from inventory
      NewInventory2=proplists:delete(ObjectId,NewInventory),
      % Calculate Damage and Armor
      BattleStats=calculate_battle_stats(WornObjects),
      % Send some feedback to the user
      ct_feedback:send(
        {obj,[{"type",<<"object_worn">>},
          {"body",{obj,[{"worn_object",{obj,Object}},{"unworn_object",{obj,OldObject}}]}}]},
        State#player_state.feedback_data),
      % Add old worn object to inventory and update list of worn objects
      State#player_state{
        worn_objects=WornObjects,
        inventory=NewInventory2,
        battle_stats=BattleStats}
  end,
  {noreply,NewState};

%% Unwears an object and return to inventory
handle_cast({unwear,ObjectId,Position},State) ->
  lager:debug("unwear ~p ~p ~p ~p",[ObjectId,Position,State#player_state.worn_objects,State#player_state.inventory]),
  % find whether we are wearing requested objct
  Result=proplists:get_value(Position,State#player_state.worn_objects,none),
  NewState=case Result of
    none ->
      ct_feedback:send(
        {obj,[{"type",<<"object_unworn">>},
          {"body",object_not_found}]},
      State#player_state.feedback_data),
      State;
    Object ->
      % remove from worn objects
      OldObject=proplists:get_value(Position,State#player_state.worn_objects) ,
      WornObjects=proplists:delete(Position,State#player_state.worn_objects),
      % return object to inventory
      NewInventory=State#player_state.inventory ++ [{ObjectId,Object}],

      % Calculate new Damage and Armor
      BattleStats=calculate_battle_stats(WornObjects),
      % Send some feedback to the user
      ct_feedback:send(
        {obj,[{"type",<<"object_unworn">>},
            {"body",{obj,[{"unworn_object",{obj,OldObject}}]}}]},
        State#player_state.feedback_data),

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

%% Tools to manipulate datastructures.
%% FIXME: Remove the need to use {obj, ...
clean_obj(X) when is_tuple(X)->
  {obj,Result}=X,
  Result;
clean_obj(X) ->
  X.

%% Tools to manipulate character data
%% Calculate total damage and armor from weared objects
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

%% Calculate damage inflinged to a user
fight_result(AttackerBattleStats, MyBattleStats)->
  MyArmor=proplists:get_value(total_armor,MyBattleStats),
  % Damage - Armor = Damage received
  % Take every Damage attacker can do and substract armor on that type of damage
  DamageList=[Value-proplists:get_value(Type,MyArmor,0)||{Type,Value}<-proplists:get_value(total_damage,AttackerBattleStats)],
  % Sum all positive damages
  lists:foldl(fun(X, Sum) -> case X >0 of true -> X+Sum;_ -> Sum end end,  0, DamageList).
