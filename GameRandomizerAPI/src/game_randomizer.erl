-module(game_randomizer).

-export([start/0]).

-spec start() -> 'ok'.
start() ->
    {ok, _} = application:ensure_all_started(game_randomizer),
    ok.