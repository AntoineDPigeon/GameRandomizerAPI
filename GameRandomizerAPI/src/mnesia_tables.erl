-module(mnesia_tables).

-include_lib("game_types.hrl").

-export([init/1, add_game/1, get_games/0, game_exists/1]).

init(Nodes) ->
    lager:log(debug, ?MODULE, "Nodes -> ~p", [Nodes]),
    mnesia:create_schema(Nodes),
    application:start(mnesia),
    Result1 = mnesia:create_table(games, [ {attributes, record_info(fields, games)} ]),
    lager:log(debug, ?MODULE, "Results -> ~p", [Result1]).

add_game(Game) ->
    GamesKey = get_games(),
    GamesCount = length(GamesKey),
    FinalGame = Game#games{id = GamesCount},
    Result = mnesia:transaction(fun() ->
        mnesia:write(FinalGame)
    end),
    lager:log(debug, ?MODULE, "add_game : ~p", [Result]),
    FinalGame.

game_exists(Game) ->
    Games = get_games(),
    is_game(Game#games.name, Games, false).

is_game(_, [], Return) ->
    Return;
is_game(Name, [H|T], Return) ->
    case H#games.name == Name of
        true ->
            is_game(Name, [], true);
        false ->
            is_game(Name, T, Return)
    end.


get_games() ->
    Keys = mnesia:dirty_all_keys(games),
    lists:foldl(fun(Key, Acc) ->
        lists:append(Acc, mnesia:dirty_read({games, Key}))
    end, [], Keys).