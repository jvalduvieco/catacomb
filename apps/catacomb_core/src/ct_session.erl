-module(ct_session).
-behaviour(gen_server).
-export([start_link/0,stop/0]).
-export([login/3,load_character/2]).
-export([init/1, handle_call/3,handle_cast/2,terminate/2,code_change/3,handle_info/2]).
-record(session_state,{login_state=not_logged}).

start_link() ->
    gen_server:start_link(?MODULE,[], []).

init([]) ->
	io:format("~s has started (~w)~n", [?MODULE,self()]),
    {ok, #session_state{}}.
stop() -> gen_server:cast(?MODULE, stop).

%% Client API
login(SessionPid,User,Password) ->
    Result=gen_server:call(SessionPid, {login, [User, Password]}),
    Result.
load_character(SessionPid,CharacterInfo) ->
    Result=gen_server:call(SessionPid,{load_character,CharacterInfo}),
    Result.

%% User Callbacks
handle_call({login, [User, Password]}, _From, State) ->
	%% Sould connect to DB, etc...
	Result=case State#session_state.login_state of
		not_logged ->
            %% use ct_auth_service to log in
            LoginResult = ct_auth_service:login(User, Password),
            io:format("LoginResult: ~p~n", [LoginResult]),
            %{reply, {ok, Uid}, State}.
            case LoginResult of 
                {ok, Uid} -> {ok,Uid};
                {error, Error} -> {error, Error}
            end;
    		%Uid=33,
    		%{ok,Uid};
    	_->
    		{error,already_logged_in}
    	end,
    %% State handling
    NewState= case Result of
    	{ok,_}->
    		State#session_state{login_state=logged_in};
    	_ ->
    		State
    	end,
    {reply, {ok,Result}, NewState};
handle_call({load_character,CharacterId}, _From, State) ->
    {ok,CharacterInfo}=ct_character_service:get_character(CharacterId),
    % Pass character data to player
    {ok,PlayerHandler}=ct_player_sup:start_player(CharacterInfo),
    ct_player:set_room(PlayerHandler,[3,3]),
    {reply,{ok,PlayerHandler},State}.



%% System Callbacks
terminate(_Reason, State) -> {ok,State}.
handle_cast(stop, State) -> {stop, normal, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.