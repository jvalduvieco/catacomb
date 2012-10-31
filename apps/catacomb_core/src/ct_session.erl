-module(ct_session).
-behaviour(gen_server).
-export([start_link/0,stop/0]).
-export([login/3,set_character/2]).
-export([init/1, handle_call/3,handle_cast/2,terminate/2,code_change/3,handle_info/2]).
-record(session_state,{login_state=not_logged,character_id=undefined}).

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    gen_server:start_link(?MODULE,[], []).

-spec init([]) -> {'ok',#session_state{login_state::'not_logged'}}.
init([]) ->
	io:format("~s has started (~w)~n", [?MODULE,self()]),
    {ok, #session_state{}}.
-spec stop() -> 'ok'.
stop() -> gen_server:cast(?MODULE, stop).

%% Client API
-spec login(_,_,_) -> any().
login(SessionPid,User,Password) ->
    Result=gen_server:call(SessionPid, {login, [User, Password]}),
    Result.
-spec set_character(_,_) -> any().
set_character(SessionPid,CharacterId) ->
    Result=gen_server:call(SessionPid,{set_character,CharacterId}),
    Result.

%% User Callbacks
-spec handle_call({'login',[any(),...]} | {'set_character',_},_,#session_state{}) -> {'reply',_,#session_state{}}.
handle_call({login, [User, Password]}, _From, State) ->
	%% Sould connect to DB, etc...
	Result=case State#session_state.login_state of
		not_logged ->
            %% use ct_auth_service to log in
            LoginResult = ct_auth_service:login(User, Password),
            io:format("LoginResult: ~p~n", [LoginResult]),
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
-spec terminate(_,_) -> {'ok',_}.
terminate(_Reason, State) -> {ok,State}.
-spec handle_cast('stop',_) -> {'stop','normal',_}.
handle_cast(stop, State) -> {stop, normal, State}.
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec handle_info(_,_) -> {'noreply',_}.
handle_info( _, State) -> {noreply,State}.