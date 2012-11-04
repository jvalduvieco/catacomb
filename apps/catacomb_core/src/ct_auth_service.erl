 -module(ct_auth_service).
-behaviour(gen_server).
-export([start_link/0,stop/0]).
-export([login/2]).
-export([init/1, handle_call/3,handle_cast/2,terminate/2,code_change/3,handle_info/2]).
%% Simple version of auth service, for further enhancement consider:
%% https://github.com/JoelPM/gen_server_pool/tree/master/src
%% http://hg.rabbitmq.com/rabbitmq-server/file/default/src/gen_server2.erl

-include("../deps/emysql/include/emysql.hrl").
-record(user_record, {id, login, password}).

start_link() ->
    gen_server:start_link({global,?MODULE}, ?MODULE, [], []).

init([]) ->
	lager:info("~s has started (~w)~n", [?MODULE,self()]),
    {ok, []}.
stop() -> gen_server:cast({global,?MODULE}, stop).

%% Client API
login(UserName,Password) ->
    Result=gen_server:call({global,?MODULE}, {login, UserName, Password}),
    Result.

%% User Callbacks
handle_call({login, UserName, Password}, _From, State) ->
	%% Should connect to DB, etc...
	%io:format("username: ~w~n", [UserName]), 
    Result = emysql:execute(ct_auth_pool, "SELECT * FROM user WHERE login=" ++ emysql_util:encode(UserName)),
    %io:format("Result: ~w~n", [Result]),
	case Result of
		#result_packet{rows=[]} ->
			{reply, {error, wrong_login_or_password}, State};
		#result_packet{} ->
			Recs = emysql_util:as_record(Result, user_record, record_info(fields, user_record)),
		    [Rec|_] = Recs,
		    %io:format("Record: ~p~n", [Rec#user_record.login]),
		    %% check password
		    PasswordFromDb = Rec#user_record.password,
		    PasswordFromCli = list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [C]) || <<C>> <= crypto:md5(Password)])),
		    %io:format("PasswordFromDb: ~w~n", [PasswordFromDb]),
		    %io:format("PasswordFromCliMD5: ~w~n", [PasswordFromCli]),
		    %io:format("PasswordFromCli: ~w~n", [Password]),
		    if PasswordFromDb =:= PasswordFromCli ->
				    Uid = Rec#user_record.id,
					{reply, {ok, Uid}, State};
			    true ->
			    	{reply, {error, wrong_login_or_password}, State}
			end;    	
		#ok_packet{} ->
			{reply, {error, sql_error}, State};
		#error_packet{} ->
			{reply, {error, sql_error}, State};
		_ -> {reply, {error, sql_error}, State}
	end.
 %   Uid=33,
 %   {reply, {ok, Uid}, State}.

%% System Callbacks
terminate(_Reason, State) -> {ok,State}.
handle_cast(stop, State) -> {stop, normal, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.