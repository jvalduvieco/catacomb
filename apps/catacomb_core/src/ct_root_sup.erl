-module(ct_root_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%% We register it so that it's guaranteed to be unique
start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

%% Using a SOFO strategy because we get to have many
%% supervisees of the same type.
init([]) ->
    lager:info("~s has started (~w)~n", [?MODULE,self()]),
    {ok,
     	{{one_for_one, 1, 60},
			[{{global,ct_auth_service},
                {ct_auth_service, start_link, []},
                permanent, infinity, worker, [ct_auth_service]},
            {{global,ct_character_service},
                {ct_character_service, start_link, []},
                permanent, infinity, worker, [ct_character_service]},
            {{global,ct_session_sup},
                {ct_session_sup, start_link, []},
                permanent, infinity, supervisor, [ct_session_sup]},
            {{global,ct_player_sup},
        		{ct_player_sup, start_link, []},
        		permanent, infinity, supervisor, [ct_player_sup]},
        	{{global,ct_room_sup},
        		{ct_room_sup, start_link, []},
        		permanent, infinity, supervisor, [ct_room_sup]},
            {{global,ct_yaws_sup},
                {ct_yaws_sup,start_link,[]},
                permanent, infinity, supervisor, [ct_yaws_sup]},
        	{{global,ct_config_service},
        		{ct_config_service, start_link, []},
        		permanent, infinity, worker, [ct_config_service]},
            {{global,ct_god},
                {ct_god, start_link, []},
                permanent, infinity, worker, [ct_god]},
    		{{global,ct_ai_sup},
        		{ct_ai_sup, start_link, []},
        		permanent, infinity, worker, [ct_ai_sup]}
        	]
        }}.
