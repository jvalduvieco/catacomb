-module(ct_yaws_catacomb_ws_endpoint).
-export([handle_message/1]).

handle_message({text, Message}) ->
    {reply, {text, Message}}.