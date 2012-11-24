-module(ct_god).
-behaviour(gen_server).

-export([start_link/0,stop/0]).
-export([init_map/0]).
-export([init/1, handle_call/3,handle_cast/2,terminate/2,code_change/3,handle_info/2]).

-record(state,{}).

start_link() ->
    gen_server:start_link({global,?MODULE}, ?MODULE, [], []).
%% Client API    
init_map() ->
    gen_server:call({global,?MODULE}, {init_map}).

%% Internal functions
init([]) ->
	lager:info("~s has started (~w)~n", [?MODULE,self()]),
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
	State=#state{},
    {ok, State}.
stop() -> gen_server:cast({global,?MODULE}, stop).

%% User Callbacks
handle_call({init_map}, _From, State) ->
    lager:debug("Creating map ~p~n", [_From]),
    X_list=lists:seq(1,ct_config_service:get_room_setup_max_x()),
    Y_list=lists:seq(1,ct_config_service:get_room_setup_max_y()),
    Combined_list=[[X,Y] || X <- X_list, Y <- Y_list],
    lists:foldl( 
        fun(RoomCoords, Sum) -> 
            ct_room_sup:create_room(RoomCoords),
            drop_objects(RoomCoords), 
            Sum+1 
        end
    ,0, Combined_list),
    %lists:foldl( fun(Room_coords, Sum) -> {ok,Pid}=ct_room_sup:get_pid(Room_coords), ct_room:print_exits(Pid), Sum+1 end,0, Combined_list),
    {reply, ok, State}.

objects() ->
    % TODO Id's are hardcoded. Should be generated on runtime for each object dropped. A player must be able to wear two identical objects.
    [[{name,<<"Iron sword">>}, {id,2001},{damage, {obj,[{mechanical,1}]}},{wearing,hand}],
     [{name,<<"Steel sword">>}, {id,2002}, {damage, {obj,[{mechanical,2}]}}, {wearing,hand}],
     [{name,<<"Mithril sword">>}, {id,2003},{damage, {obj,[{mechanical,3}]}}, {wearing,hand}],
     [{name,<<"Iron great helm">>}, {id,2004}, {armor,{obj,[{mechanical, 1}]}},{wearing,head}],
     [{name,<<"Steel great helm">>}, {id,2005}, {armor,{obj,[{mechanical, 1}]}},{wearing,head}],
     [{name,<<"Mithril great helm">>}, {id,2006}, {damage, {obj,[{mechanical,3}]}},{armor,{obj,[{mechanical, 1},{poison, 1}]}},{wearing,head}],
     [{name,<<"Iron cuirass">>}, {id,2007}, {armor,{obj,[{mechanical, 1}]}},{wearing,chest}],
     [{name,<<"Steel cuirass">>}, {id,2008}, {armor,{obj,[{mechanical, 1}]}},{wearing,chest}],
     [{name,<<"Mithril cuirass">>}, {id,2009}, {armor,{obj,[{mechanical, 1}]}},{wearing,chest}]
     ].

get_random_object() ->
    L = objects(),

    lists:nth(random:uniform(length(L)), L).
drop_objects(RoomCoords) ->
    {ok,Room}=ct_room_sup:get_pid(RoomCoords),
    ct_room:add_object(Room,get_random_object()).


%% System Callbacks
terminate(_Reason, State) -> {ok,State}.
handle_cast(stop, State) -> {stop, normal, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.
