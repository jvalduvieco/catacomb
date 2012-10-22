-module(ct_client_command).
-export([execute/2]).
-include ("ct_character_info.hrl").

-record(ct_client_state,{session_pid=none,
	user_id=none,
	player_pid=none}).

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
        		io:format("Error when decoding ~p~n",[Error]),
        		Result=list_to_binary("Error when decoding: " ++ atom_to_list(Error)),
        		{error,Result,State};
  			_ -> 	
        		io:format("WTF?~n"),
        		{error,[],State}
    	end
    catch
		What:Why ->
		    Trace=erlang:get_stacktrace(),
		    error_logger:error_msg("ct_client_command: ~p ~p ~p.\n", [What,Why,Trace]),
		    {stop, Why, State}
  	end.

do_command(Cmd,State) ->
	Command = ct_translation_tools:get_type(Cmd),
	io:format("Command: ~p~n", [Command]),
	Result = case Command of
		<<"LoginRequest">> ->
			User=ct_translation_tools:get_value(<<"user">>,Cmd),
			Password=ct_translation_tools:get_value(<<"password">>,Cmd),
			io:format("Log in: User: ~p Password: ~p ~n",[User,Password]),
			% Check if we have a session. If there is no session create a new one.
			{ok,SessionPid}=case State#ct_client_state.session_pid of
				none ->
					ct_session_sup:get_new_session_pid();
				_ ->
					{ok,State#ct_client_state.session_pid}
			end,

			% Try to login onto the system
			LoginResult=ct_session:login(SessionPid,User,Password),
			case LoginResult of 
				{ok,UserId} ->
					NewState=State#ct_client_state{session_pid=SessionPid,user_id=UserId},
					CmdResult={obj,[{"type",<<"LoginResponse">>},{"result",<<"success">>}]},
					{ok,CmdResult,NewState};
				{error, Error} ->
					CmdResult={obj,[{"type",<<"LoginResponse">>},{"result",<<"failure">>},{"body",Error}]},
					{ok,CmdResult,State}
			end;
		<<"GetCharacterList">> ->
			%{ok,CharacterList} = ct_character_service:get_character_list(State#ct_client_state.user_id),
			CharacterList=[{obj,[{<<"id">>,1},
  							{<<"login">>,<<"jordi">>},
							{<<"password">>,<<"1a1dc91c907325c69271ddf0c944bc72">>}]},
							{obj,[{<<"id">>,2},
							{<<"login">>,<<"joan">>},
							{<<"password">>,<<"1a1dc91c907325c69271ddf0c944bc72">>}]},
							{obj,[{<<"id">>,3},
							{<<"login">>,<<"dani">>},
							{<<"password">>,<<"1a1dc91c907325c69271ddf0c944bc72">>}]}],
			%JSONResult=[{"type","LoginResponse"},{"body","OK"}],
			CmdResult={obj,[{"type",<<"GetCharacterListResponse">>},{"result",<<"success">>},{"body",CharacterList}]},
			{ok,CmdResult,State};
		<<"new_character">> ->
			%decode request body
			%NewCharacter = ct_character_service:new_character(Status#status.user_id,...),
			{ok,[],State};
		<<"load_character">>->
			CharacterId=rfc4627:get_field(Cmd,"character_id",<<>>),
			{ok,PlayerHandle}=ct_session:load_character(State#ct_client_state.session_pid,CharacterId),
			JSONResult=rfc4627:from_record(PlayerHandle, ct_character_info, record_info(fields, ct_character_info)),
			{ok,JSONResult,State};
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
		<<"go">>->
			%%ct_player:go(State#ct_client_state.player_pid,Direction),
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
		InvalidCommand->
			ErrorStr= "Unkown command: "++ binary_to_list(InvalidCommand),
			CmdResult={obj,[{"type",<<"GeneralResponse">>},{"result",<<"failure">>},{"body",list_to_binary(ErrorStr)}]},
			{error,CmdResult,State}
		%% Chat commands to be added
		end,
	Result.
