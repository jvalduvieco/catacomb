-module(ct_session).
-behaviour(gen_server).
-export([start_link/0]).
-export([login/3,set_character/2,stop/1]).
-export([init/1, handle_call/3,handle_cast/2,terminate/2,code_change/3,handle_info/2]).
-record(session_state,{login_state=not_logged,character_id=undefined}).

start_link() ->
    gen_server:start_link(?MODULE,[], []).

init([]) ->
	lager:info("~s has started (~w)~n", [?MODULE,self()]),
    {ok, #session_state{}}.
stop(Session) -> 
    gen_server:cast(Session, stop).

%% Client API
login(SessionPid,User,Password) ->
    Result=gen_server:call(SessionPid, {login, [User, Password]}),
    Result.
set_character(SessionPid,CharacterId) ->
    Result=gen_server:call(SessionPid,{set_character,CharacterId}),
    Result.

%% User Callbacks
handle_call({login, [User, Password]}, _From, State) ->
	%% Sould connect to DB, etc...
	Result=case State#session_state.login_state of
		not_logged ->
            %% use ct_auth_service to log in
            LoginResult = ct_auth_service:login(User, Password),
            lager:debug("LoginResult: ~p~n", [LoginResult]),
            LoginResult;
    	_->
    		{error,already_logged_in}
    	end,
    %% State handling
    NewState=case Result of
    	{ok,_}->
    		State#session_state{login_state=logged_in};
    	_ ->
    		State
    	end,
    {reply, Result, NewState};
handle_call({set_character,CharacterId}, _From, State) ->
    NewState=State#session_state{character_id=CharacterId},
    {reply, ok,NewState}.



%% System Callbacks
terminate(_Reason, State) -> {ok,State}.
handle_cast(stop, State) -> {stop, normal, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.