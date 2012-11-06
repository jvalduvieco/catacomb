-module(ct_yaws_start).
-compile(export_all).

-include("yaws.hrl").

start() ->
    {ok, spawn(?MODULE, run, [])}.

run() ->
    lager:info("~s has started (~w)~n", [?MODULE,self()]),
    Id = "embedded",
    GconfList = [{id, Id},
                {logdir, "./log/"}],
    Docroot = "./www/",
    SconfList = [{docroot, Docroot},
                {port, 8081},
                %%{listen, {127,0,0,1}},
                {listen, {0,0,0,0}},
                {appmods, [{"/catacomb_ws_endpoint", ct_yaws_catacomb_ws_endpoint}]}
                ],

    {ok, SCList, GC, ChildSpecs} = yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child({global, ct_yaws_sup}, Ch) || Ch <- ChildSpecs],

    %% now configure Yaws
    yaws_api:setconf(GC, SCList),
    {ok, self()}.