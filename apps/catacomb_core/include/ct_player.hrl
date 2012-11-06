%%% Player state

-record(player_state,{id,
                      my_pid,
                      client,
                      name,
                      max_life_points,
                      life_points,
                      level,
                      experience_points,
                      room,
                      room_exits,
                      params=[],
                      feedback_fun=fun(_Player, Feedback) -> io:format("Player default feedback fun: ~p~n", [Feedback]) end,
                      public_id
                     }).
