-module(ct_config_service).
-behaviour(gen_server).

-export([start_link/0,stop/0]).
-export([get_room_setup_max_x/0, get_room_setup_max_y/0,set_room_setup_max_x/1, set_room_setup_max_y/1]).
-export([init/1, handle_call/3,handle_cast/2,terminate/2,code_change/3,handle_info/2]).

-record(state,{room_data_max_x=10,
		room_data_max_y=10}). %% TODO: Rewrite with nested records

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    gen_server:start_link({global,?MODULE}, ?MODULE, [], []).

%% Client API    
-spec get_room_setup_max_x() -> any().
get_room_setup_max_x() ->
    gen_server:call({global,?MODULE}, {get_room_setup_max_x}).
-spec get_room_setup_max_y() -> any().
get_room_setup_max_y() ->
    gen_server:call({global,?MODULE}, {get_room_setup_max_y}).
-spec set_room_setup_max_x(_) -> any().
set_room_setup_max_x(Max_x)->
	gen_server:call({global,?MODULE},{set_room_setup_max_x,Max_x}).
-spec set_room_setup_max_y(_) -> any().
set_room_setup_max_y(Max_y)->
	gen_server:call({global,?MODULE},{set_room_setup_max_y,Max_y}).

%% Internal functions
-spec init([]) -> {'ok',#state{room_data_max_x::10,room_data_max_y::10}}.
init([]) ->
	io:format("ct_config_service has started (~w)~n", [self()]),
	State=#state{},
    {ok, State}.
-spec stop() -> 'ok'.
stop() -> gen_server:cast({global,?MODULE}, stop).

%% Callbacks
-spec handle_call({'get_room_setup_max_x'} | {'get_room_setup_max_y'} | {'set_room_setup_max_x',_} | {'set_room_setup_max_y',_},_,#state{}) -> {'reply',_,#state{}}.
handle_call({get_room_setup_max_x}, _From, State) ->
    {reply,State#state.room_data_max_x,State};
handle_call({get_room_setup_max_y}, _From, State) ->
    {reply,State#state.room_data_max_y,State};
handle_call({set_room_setup_max_x, Max_x}, _From, State) ->
	New_state=State#state{room_data_max_x = Max_x},
    {reply, ok,New_state};
handle_call({set_room_setup_max_y, Max_y}, _From, State) ->
	New_state=State#state{room_data_max_y = Max_y},
    {reply, ok,New_state}.

%% System callbacks
-spec terminate(_,_) -> {'ok',_}.
terminate(_Reason, State) -> {ok,State}.
-spec handle_cast('stop',_) -> {'stop','normal',_}.
handle_cast(stop, State) -> {stop, normal, State}.
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
-spec handle_info(_,_) -> {'noreply',_}.
handle_info( _, State) -> {noreply,State}.
