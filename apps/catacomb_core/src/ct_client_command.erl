-module(ct_client_command).
-export([execute/3,client_connected/0,client_disconnected/1]).

-record(ct_client_state,{session_pid=undefined,
	user_id=undefined,
	player_handle=undefined}).

execute(Cmd,#ct_client_state{} = StateFromGateway,FeedbackData) ->
  % State contains State data relevant to this module
	try 
		DecodeResult=ct_translation_tools:from_client(Cmd),
    	case DecodeResult of
      		{ok, DataObj} ->
      			{BoolResult,Result,NewState}=do_command(DataObj,StateFromGateway,FeedbackData),
      			% Encode response in JSON format. Force ok response as an error here probably is a developer bug
      			{ok,EncodedJSON}=ct_translation_tools:to_client(Result),
      			{BoolResult,EncodedJSON,NewState};
      		{error, Error} -> 
        		lager:error("Error when decoding ~p~n",[Error]),
        		Result=list_to_binary("Error when decoding: " ++ atom_to_list(Error)),
        		{error,Result,StateFromGateway};
  			  _ ->
         		lager:error("WTF?~n"),
        		{error,[],StateFromGateway}
    	end
    catch
		What:Why ->
		    Trace=erlang:get_stacktrace(),
		    lager:error("ct_client_command: ~p ~p ~p.\n", [What,Why,Trace]),
		    {stop, Why, StateFromGateway}
  	end.
%% Initialize client state
client_connected() ->
  #ct_client_state{}.

%% Cleanup client data
client_disconnected(#ct_client_state{session_pid=SessionPid,player_handle=PlayerHandle}) ->
	ct_session:stop(SessionPid),
	ct_player:stop(PlayerHandle).

do_command(Cmd,State,FeedbackData) ->
	Command=ct_translation_tools:get_type(Cmd),
  lager:debug("Command: ~p~n", [Command]),
	Result=case Command of
		<<"login_request">> ->
			User=ct_translation_tools:get_value(<<"user">>,Cmd),
			Password=ct_translation_tools:get_value(<<"password">>,Cmd),
			lager:debug("Log in: User: ~p Password: ~p ~n",[User,Password]),
			% Check if we have a session. If there is no session create a new one.
			{ok,SessionPid}=case State#ct_client_state.session_pid of
        undefined ->
					ct_session_sup:get_new_session_pid();
				_ ->
					{ok,State#ct_client_state.session_pid}
			end,

			% Try to login onto the system
			case ct_session:login(SessionPid,User,Password) of 
				{ok,UserId} ->
					NewState=State#ct_client_state{session_pid=SessionPid,user_id=UserId},
					CmdResult={obj,[{"type",<<"login_response">>},{"result",<<"success">>}]},
					{ok,CmdResult,NewState};
				{error, Error} ->
					CmdResult={obj,[{"type",<<"login_response">>},{"result",<<"failure">>},{"body",Error}]},
					{ok,CmdResult,State}
			end;
		<<"get_character_list_request">> ->
			{ok,CharacterList} = ct_character_service:get_character_list(State#ct_client_state.user_id),
			CmdResult = {obj,[{"type",<<"get_character_list_response">>},{"result",<<"success">>},{"body",CharacterList}]},
			{ok,CmdResult,State};
		<<"new_character_request">> ->
			%decode request body
			%NewCharacter = ct_character_service:new_character(Status#status.user_id,...),
			{ok,[],State};
		<<"load_character_request">>->
			% check if there is a player already, if the user has logged in, etc...
			CharacterId = ct_translation_tools:get_value(<<"character_id">>, Cmd),

			{ok, CharacterData} = ct_character_service:get_character_data(State#ct_client_state.user_id,CharacterId),
			{ok, PlayerHandle} = ct_player_sup:start_player(CharacterData,FeedbackData),
			ok = ct_session:set_character(State#ct_client_state.session_pid, CharacterId),
				
			{ok,{obj, [{"type", <<"load_character_response">>}, 
							{"result", <<"success">>},
              {"body",{obj,[
              {"player_public_id",ct_player:get_public_id(PlayerHandle)},
              {"player_name",ct_player:get_name(PlayerHandle)}
              ]}}]},
						State#ct_client_state{player_handle = PlayerHandle}};
		<<"start_game">>->
			%% Unfreeze the character
			%% session change state
			{ok,[],State};
		<<"exit_game">>->
			%% tell the session to kill everyone and get to idle state.
			%% by a call
			{ok,[],State};
		<<"logout">>->
			%% tell the session to die
			%% stop the player
			{ok,[],State};
		<<"player_go_request">>->
			% check if there is a player already, if the user has logged in, etc...
			Direction = list_to_existing_atom(binary_to_list(ct_translation_tools:get_value(<<"direction">>, Cmd))),
			ct_player:go(State#ct_client_state.player_handle,Direction),
			{ok,[],State};
		<<"catch">>->
			%%ct_player:catch(Status#status.player_pid,ObjectId)
			{ok,[],State};
		<<"drop_object">>->
      ObjectId=list_to_integer(binary_to_list(ct_translation_tools:get_value(<<"object_id">>, Cmd))),
			ct_player:drop_object(State#ct_client_state.player_handle,ObjectId),
			{ok,[],State};
    <<"game_info_request">>->
      GameInfo=ct_session:get_game_info(State#ct_client_state.session_pid),
      {ok,{obj,[{"type",<<"game_info_response">>},{"body",{obj,GameInfo}}]},State};
		<<"get_inventory">>->
			%%ct_player:get_inventory(State#ct_client_state.player_handle)
			{ok,[],State};
		<<"info">> ->
			%%ct_player:info(Status#State.ct_client_state)
			{ok,[],State};
		<<"attack">>->
			PlayerId=list_to_integer(binary_to_list(ct_translation_tools:get_value(<<"character_id">>, Cmd))),
			{ok,OtherPlayer}=ct_player_sup:get_handler(PlayerId),
			ct_player:attack(State#ct_client_state.player_handle,OtherPlayer),
			{ok,[],State};
		<<"hit">>->
			%%ct_player:hit(Status#status.player_pid,PlayerPid)
			{ok,[],State};
		<<"player_talk_request">>->
      Message=ct_translation_tools:get_value(<<"message">>,Cmd),
			ct_player:talk(State#ct_client_state.player_handle, Message),
			{ok,[],State};
		<<"heartbeat_request">>->
      LastTimeDiff=ct_translation_tools:get_value(<<"ltd">>,Cmd),
      ct_player:heartbeat(State#ct_client_state.player_handle, LastTimeDiff),
			{ok,[],State};
    <<"pick_object_request">>->
			ObjectId=list_to_integer(binary_to_list(ct_translation_tools:get_value(<<"object_id">>,Cmd))),
			ct_player:pick_object(State#ct_client_state.player_handle,ObjectId),
			{ok,[],State};
    <<"wear_object">>->
      ObjectId=list_to_integer(binary_to_list(ct_translation_tools:get_value(<<"object_id">>,Cmd))),
      ct_player:wear(State#ct_client_state.player_handle,ObjectId),
      {ok,[],State};
    <<"unwear_object">>->
      ObjectId=list_to_integer(binary_to_list(ct_translation_tools:get_value(<<"object_id">>,Cmd))),
      Position=list_to_existing_atom(binary_to_list(ct_translation_tools:get_value(<<"position">>,Cmd))),
      ct_player:unwear(State#ct_client_state.player_handle,ObjectId,Position),
      {ok,[],State};
		InvalidCommand->
			ErrorStr= "Unkown command: "++ binary_to_list(InvalidCommand),
			CmdResult={obj,[{"type",<<"general_response">>},{"result",<<"failure">>},{"body",list_to_binary(ErrorStr)}]},
			{error,CmdResult,State}
		%% Chat commands to be added
		end,
	Result.