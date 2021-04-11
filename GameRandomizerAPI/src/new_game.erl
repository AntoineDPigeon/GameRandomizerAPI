-module(new_game).

-include_lib("game_types.hrl").

-export([add/1, delete/1, change/1]).

add(Req) ->
    {ok, GameJson, Req1} = read_body(Req, <<>>),

    { DecodedGame } = jiffy:decode(GameJson),
    lager:log(debug, ?MODULE, "Received json -> ~p", [DecodedGame]),

    Game = utils:get_game(DecodedGame, #games{}),
    FormattedGameName = string:trim(Game#games.name),
    Game2 = Game#games{name = FormattedGameName},
    Exists = mnesia_tables:game_exists(Game2),
    add_game(Exists, Game2, Req1).

change(Req) ->
    {ok, GameJson, Req1} = read_body(Req, <<>>),
    { DecodedGame } = jiffy:decode(GameJson),
    lager:log(debug, ?MODULE, "Received json -> ~p", [DecodedGame]),
    Game = utils:get_game(DecodedGame, #games{}),
    case Game#games.id of
        undefined ->
            cowboy_req:reply(400, #{<<"access-control-allow-origin">> => <<"*">>}, <<"Id is necessary.">>, Req1);
        _ ->
            mnesia_tables:modify_game(Game),
            cowboy_req:reply(200, #{<<"access-control-allow-origin">> => <<"*">>}, <<>>, Req1)
    end.

delete(Req) ->
    Id = binary_to_integer(cowboy_req:binding(id, Req)),
    case mnesia_tables:delete_game_by_id(Id) of
        false ->
            cowboy_req:reply(404, #{<<"access-control-allow-origin">> => <<"*">>}, <<>>, Req);
        true ->
            cowboy_req:reply(200, #{<<"access-control-allow-origin">> => <<"*">>}, <<>>, Req)
    end.

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

add_game(false, Game, Req1) ->
    %  ONly add games 
    lager:log(debug, ?MODULE, "received a new game name -> ~p", [Game#games.name]),
    FinalGame = mnesia_tables:add_game(Game),

    {ok, S} = file:open("games.txt", [append]),
    file:write(S, iolist_to_binary(["{\"id\":", integer_to_binary(FinalGame#games.id),",\"name\":\"", FinalGame#games.name,"\",\"url\":\"", FinalGame#games.url,"\"}\n"])),
    cowboy_req:reply(200, #{<<"access-control-allow-origin">> => <<"*">>}, <<>>, Req1);

add_game(true, Game, Req1) ->
    lager:log(debug, ?MODULE, "Game already exists -> ~p", [Game#games.name]),
    cowboy_req:reply(409, #{<<"access-control-allow-origin">> => <<"*">>}, <<>>, Req1).
