-module(game_table).

-include_lib("game_types.hrl").

-export([init/1, add_game/1, get_games/0]).

init(Nodes) ->
    lager:log(debug, ?MODULE, "Nodes -> ~p", [Nodes]),
    mnesia:create_schema(Nodes),
    application:start(mnesia),
    Result = mnesia:create_table(games, [ {attributes, record_info(fields, games)} ]),
    lager:log(debug, ?MODULE, "Result -> ~p", [Result]).

add_game(Game) ->
    Result = mnesia:transaction(fun() ->
        mnesia:write(Game)
    end),
    lager:log(debug, ?MODULE, "add_game : ~p", [Result]).

get_games() ->
    Keys = mnesia:dirty_all_keys(games),
    lists:foldl(fun(Key, Acc) ->
        lists:append(Acc, mnesia:dirty_read({games, Key}))
    end, [], Keys).