-module(ct_client_command).
-export([execute/2]).

execute(Cmd,State) -> %% State contains State data relevant to this module
	try
	Command = rfc4627:get_field(Cmd,"command",<<>>),
	io:format("Command: ~p~n", [Command]),
	Result = case Command of
		<<"login">> ->
			User=rfc4627:get_field(Cmd,"user",<<>>),
			Password=rfc4627:get_field(Cmd,"password",<<>>),
			io:format("Log in: User: ~p Password: ~p ~n",[User,Password]),
			%% SessionId = ct_session:login(Status#status.session_pid,Login,Password),
			{ok,[]};
		<<"get_character_list">> ->
			%% CharacterList = ct_session:get_character_list(Status#status.session_pid),
			{ok,[]};
		<<"new_character">>->
			%% NewCharacter = ct_session:new_character(Status#status.session_pid,....)
			{ok,[]};
		<<"load_character">>->
			{ok,[]};
		<<"start_game">>->
			{ok,[]};
		<<"exit_game">>->
			{ok,[]};
		<<"logout">>->
			{ok,[]};
		<<"go">>->
			{ok,[]};
		<<"catch">>->
			{ok,[]};
		<<"drop">>->
			{ok,[]};
		<<"get_inventory">>->
			{ok,[]};
		<<"hit">>->
			{ok,[]}
		end,
	{Result}
	catch
		What:Why ->
	    Trace=erlang:get_stacktrace(),
	    error_logger:error_msg("ct_client_command: ~p ~p ~p.\n", [What,Why,Trace]),
	    {stop, Why, State}
  end.