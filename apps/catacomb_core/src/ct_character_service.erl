-module(ct_character_service).
-behaviour(gen_server).
-export([start_link/0,stop/0]).
-export([get_character_list/1,get_character_data/2]).
-export([init/1, handle_call/3,handle_cast/2,terminate/2,code_change/3,handle_info/2]).

-include ("ct_character_info.hrl").
-include("../deps/emysql/include/emysql.hrl").

%% Simple version of character service, for further enhancement consider:
%% https://github.com/JoelPM/gen_server_pool/tree/master/src
%% http://hg.rabbitmq.com/rabbitmq-server/file/default/src/gen_server2.erl

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    gen_server:start_link({global,?MODULE}, ?MODULE, [], []).

-spec init([]) -> {'ok',[]}.
init([]) ->
	io:format("~s has started (~w)~n", [?MODULE,self()]),
    {ok, []}.
-spec stop() -> 'ok'.
stop() -> gen_server:cast({global,?MODULE}, stop).

%% Client API
-spec get_character_list(_) -> any().
get_character_list(UserId) ->
    gen_server:call({global,?MODULE}, {get_character_list, UserId}).
-spec get_character_data(_,_) -> any().
get_character_data(UserId,CharacterId) ->
    gen_server:call({global,?MODULE}, {get_character_data, UserId, CharacterId}).
    

%% User Callbacks
-spec handle_call({'get_character_list',_} | {'get_character_data',_,_},_,_) -> {'reply',{'error','sql_error'} | {'ok',[any()]},_}.
handle_call({get_character_list, UserId}, _From, State) ->
	% User1=#ct_character_info{id=32908230982,name="lazi",max_life_points=32200,life_points=18000},
	% User2=#ct_character_info{id=32908230983,name="GeD",max_life_points=32000,life_points=15000},
	% User3=#ct_character_info{id=32908230984,name="TITO",max_life_points=52000,life_points=100},
 %    {reply, {ok, [User1,User2,User3]}, State};
    Result = emysql:execute(ct_auth_pool, "SELECT * FROM `character` WHERE user_id=" ++ emysql_util:encode(UserId)),
	case Result of
		#result_packet{rows=[]} ->
			{reply, {ok, []}, State};
		#result_packet{} ->
		    List = emysql_util:as_proplists(Result,fun(A)-> {obj,A} end),
			{reply, {ok, List}, State};
		#ok_packet{} ->
			{reply, {error, sql_error}, State};
		#error_packet{} ->
			{reply, {error, sql_error}, State};
		_ -> {reply, {error, sql_error}, State}
	end;	
handle_call({get_character_data,UserId,CharacterId}, _From, State) ->
	%Get character data from DB
	SQL=io_lib:format("SELECT * FROM `character` WHERE id=~s AND user_id=~s", [emysql_util:encode(CharacterId),emysql_util:encode(UserId)]),
    Result = emysql:execute(ct_auth_pool, SQL ),
	case Result of
		#result_packet{rows=[]} ->
			{reply, {ok, []}, State};
		#result_packet{} ->
		    List = emysql_util:as_proplists(Result,fun(A)->{obj,A} end),
			{reply, {ok, List}, State};
		#ok_packet{} ->
			{reply, {error, sql_error}, State};
		#error_packet{} ->
			{reply, {error, sql_error}, State};
		_ -> {reply, {error, sql_error}, State}
	end.
%% System Callbacks
-spec terminate(_,_) -> {'ok',_}.
terminate(_Reason, State) -> {ok,State}.
-spec handle_cast('stop',_) -> {'stop','normal',_}.
handle_cast(stop, State) -> {stop, normal, State}.
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec handle_info(_,_) -> {'noreply',_}.
handle_info( _, State) -> {noreply,State}.