-module(ct_session).
-behaviour(gen_server).
-export([start_link/0,stop/0]).
-export([login/3]).
-export([init/1, handle_call/3,handle_cast/2,terminate/2,code_change/3,handle_info/2]).

start_link() ->
    gen_server:start_link({global,?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("~s has started (~w)~n", [?MODULE,self()]),
    {ok, []}.
stop() -> gen_server:cast({global,?MODULE}, stop).

%% Client API
login(SessionPid,User,Password) ->
    Result=gen_server:call(SessionPid, {login, [User, Password]}),
    Result.

%% User Callbacks
handle_call({login, [_User, _Password]}, _From, State) ->
	%% Sould connect to DB, etc...
    Uid=33,
    {reply, {ok, Uid}, State}.

%% System Callbacks
terminate(_Reason, State) -> {ok,State}.
handle_cast(stop, State) -> {stop, normal, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.