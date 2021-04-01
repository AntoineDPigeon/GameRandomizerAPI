-module(game_handler).
-behavior(cowboy_handler).

-include_lib("game_types.hrl").

%% REST Callbacks
-export([init/2]).

%% Callback Callbacks
-export([add_new_game/1, get_all_games/1]).

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Req2 = case Method of
        <<"GET">> ->
            get_all_games(Req);
        <<"PUT">> ->
            add_new_game(Req);
        <<"DELETE">> ->
            Req
    end,
    {ok, Req2, State}.

add_new_game(Req) ->
    {ok, Game, Req1} = read_body(Req, <<>>),
    lager:log(debug, ?MODULE, "received a new game name -> ~p", [Game]),
    game_table:add_game(#games{name=Game}),
    cowboy_req:reply(200, Req1).

get_all_games(Req) ->
    GameList = game_table:get_games(),
    lager:log(debug, ?MODULE, "sending back all games -> ~p", [GameList]),
    EncodedList = case GameList of
        <<>> -> <<>>;
        [] -> [];
        _ ->
            % PreparedBinary = erlang:iolist_to_binary(io_lib:format("~p", [Binary])),
            StringConverted = [ X || #games{name=X} <- GameList ],
            jiffy:encode(StringConverted)
    end,
    lager:log(debug, ?MODULE, "Encoded list -> ~p", [EncodedList]),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, EncodedList, Req).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.