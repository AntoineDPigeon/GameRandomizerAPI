-module(utils).

-include_lib("game_types.hrl").

-export([get_game/2]).

get_game([], Game) ->
    Game;
get_game([H|T], Game) ->
    {Key, Value} = H,
    Game2 = get_value(Key, Value, Game),
    get_game(T, Game2).

get_value(<<"id">>, _Id, Game) ->
    % For now we ignore incoming ID
    Game;
get_value(<<"name">>, Name, Game) ->
    Game#games{name=Name};
get_value(<<"url">>, Url, Game) ->
    Game#games{url=Url};
get_value(Key, _, Game) ->
    lager:log(warning, ?MODULE, "Unrecognized Key -> ~p", [Key]),
    Game.