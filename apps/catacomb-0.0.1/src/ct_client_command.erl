-module(ct_client_command).
-export([execute/2]).

execute(Cmd,State)-> %% State contains State data relevant to this module
	Command = rfc4627:get_field(Cmd,"command",<<>>),
	io:format("Command: ~p~n", [Command]),
	Result = case Command of
		login ->
			Login=rfc4627:get_field(Cmd,"login",<<>>),
			Password=rfc4627:get_field(Cmd,"password",<<>>),
			io:format("Log in: User: ~p Password: ~p",[Login,Password]),
			%% SessionId = ct_session:login(),
			{ok,[]};
		get_character_list->
			%% CharacterList = ct_session:get_character_list(),
			{ok,[]};
		new_character->
			{ok,[]};
		exit_game->
			{ok,[]};
		logout->
			{ok,[]}
		end,
	{Result}.