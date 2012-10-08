-module(ct_yaws_start).
-compile(export_all).

start() ->
    {ok, spawn(?MODULE, run, [])}.

run() ->
    Id = "embedded",
    GconfList = [{id, Id}],
    Docroot = "./www/",
    SconfList = [{docroot, Docroot},
                {logdir, "./log/"},
                {port, 8080},
                {listen, {127,0,0,1}},
                {appmods, [{"/catacomb_ws_endpoint", ct_yaws_catacomb_ws_endpoint}]}
                ],

    {ok, SCList, GC, ChildSpecs} = yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),

    [supervisor:start_child({global, ct_yaws_sup}, Ch) || Ch <- ChildSpecs],

    %% now configure Yaws
    yaws_api:setconf(GC, SCList),
    {ok, self()}.