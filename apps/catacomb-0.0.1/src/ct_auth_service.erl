-module(ct_auth_service).
-behaviour(gen_server).
-export([start_link/0,stop/0]).
-export([login/2]).
-export([init/1, handle_call/3,handle_cast/2,terminate/2,code_change/3,handle_info/2]).
%% Simple version of auth service, for further enhancement consider:
%% https://github.com/JoelPM/gen_server_pool/tree/master/src
%% http://hg.rabbitmq.com/rabbitmq-server/file/default/src/gen_server2.erl

start_link() ->
    gen_server:start_link({global,?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("~s has started (~w)~n", [?MODULE,self()]),
    {ok, []}.
stop() -> gen_server:cast({global,?MODULE}, stop).

%% Client API
login(UserName,Password) ->
    Result=gen_server:call({global,?MODULE}, {login, UserName, Password}),
    Result.

%% User Callbacks
handle_call({login, _UserName, _Password}, _From, State) ->
	%% Sould connect to DB, etc...
    Uid=33,
    {reply, {ok, Uid}, State}.

%% System Callbacks
terminate(_Reason, State) -> {ok,State}.
handle_cast(stop, State) -> {stop, normal, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.