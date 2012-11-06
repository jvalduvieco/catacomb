-module(ct_client_command).
-export([execute/2,send_feedback/2,client_disconnected/1]).
-include ("ct_character_info.hrl").

-record(ct_client_state,{session_pid=none,
	user_id=none,
	player_handle=none}).

execute(Cmd,State) -> %% State contains State data relevant to this module
	try 
		DecodeResult=ct_translation_tools:from_client(Cmd),
    	case DecodeResult of
      		{ok, DataObj} ->
      			{BoolResult,Result,NewState}=do_command(DataObj,State),
      			% Encode response in JSON format. Force ok response as an error here probably is a developer bug
      			{ok,EncodedJSON}=ct_translation_tools:to_client(Result),
      			{BoolResult,EncodedJSON,NewState};
      		{error, Error} -> 
        		lager:error("Error when decoding ~p~n",[Error]),
        		Result=list_to_binary("Error when decoding: " ++ atom_to_list(Error)),
        		{error,Result,State};
  			_ -> 	
        		lager:error("WTF?~n"),
        		{error,[],State}
    	end
    catch
		What:Why ->
		    Trace=erlang:get_stacktrace(),
		    lager:error("ct_client_command: ~p ~p ~p.\n", [What,Why,Trace]),
		    {stop, Why, State}
  	end.
client_disconnected(#ct_client_state{session_pid=SessionPid,player_handle=PlayerHandle}) ->
	ct_session:stop(SessionPid),
	ct_player:stop(PlayerHandle).

do_command(Cmd,State) ->
	Command=ct_translation_tools:get_type(Cmd),
  lager:debug("Command: ~p~n", [Command]),
	Result=case Command of
		<<"login_request">> ->
			User=ct_translation_tools:get_value(<<"user">>,Cmd),
			Password=ct_translation_tools:get_value(<<"password">>,Cmd),
			lager:info("Log in: User: ~p Password: ~p ~n",[User,Password]),
			% Check if we have a session. If there is no session create a new one.
			{ok,SessionPid}=case State#ct_client_state.session_pid of
				none ->
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
			{ok, PlayerHandle} = ct_player_sup:start_player(CharacterData),
			ct_player:set_client(PlayerHandle,self()),
			ct_player:set_feedback_fun(PlayerHandle, fun(Player, Feedback) -> ct_client_command:send_feedback(Player, Feedback) end),
			ok = ct_session:set_character(State#ct_client_state.session_pid, CharacterId),
				
			{ok,{obj, [{"type", <<"load_character_response">>}, 
							{"result", <<"success">>}]},
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
			%% suicide ourselves
			{ok,[],State};
		<<"player_go_request">>->
			% check if there is a player already, if the user has logged in, etc...
			Direction = list_to_existing_atom(binary_to_list(ct_translation_tools:get_value(<<"direction">>, Cmd))),
			ct_player:go(State#ct_client_state.player_handle,Direction),
			{ok,[],State};
		<<"catch">>->
			%%ct_player:catch(Status#status.player_pid,ObjectId)
			{ok,[],State};
		<<"drop">>->
			%%ct_player:drop(Status#status.player_pid,ObjectId)
			{ok,[],State};
		<<"get_inventory">>->
			%%ct_player:get_inventory(Status#status.player_pid)
			{ok,[],State};
		<<"info">> ->
			%%ct_player:info(Status#status.player_pid)
			{ok,[],State};
		<<"hit">>->
			%%ct_player:hit(Status#status.player_pid,PlayerPid)
			{ok,[],State};
		<<"player_talk_request">>->
            Message=ct_translation_tools:get_value(<<"message">>,Cmd),
			ct_player:talk(State#ct_client_state.player_handle, Message),
			{ok,[],State};
		InvalidCommand->
			ErrorStr= "Unkown command: "++ binary_to_list(InvalidCommand),
			CmdResult={obj,[{"type",<<"general_response">>},{"result",<<"failure">>},{"body",list_to_binary(ErrorStr)}]},
			{error,CmdResult,State}
		%% Chat commands to be added
		end,
	Result.
	websocket_feedback(Pid,Feedback)->
		{ok,Response}=ct_translation_tools:to_client(Feedback),
		yaws_api:websocket_send(Pid,{text, list_to_binary(Response)}).
	send_feedback(Player,Feedback)->
		ClientPid=ct_player:get_client(Player),
		% Depending on app configuration do a callback
		websocket_feedback(ClientPid,Feedback).

