-module(game_handler).
-behavior(cowboy_handler).

-include_lib("game_types.hrl").

%% REST Callbacks
-export([init/2]).

%% Callback Callbacks
-export([get_all_games/1]).

init(Req, State) ->
    Method = cowboy_req:method(Req),
    lager:log(debug, ?MODULE, "incoming req -> ~p", [Method]),
    Req3 = case Method of
        <<"GET">> ->
            get_all_games(Req);
        <<"POST">> ->
            new_game:add(Req);
        <<"PUT">> ->
            new_game:change(Req);
        <<"OPTIONS">> ->
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>, <<"access-control-allow-methods">> => <<"GET,OPTIONS,PUT">>, <<"access-control-allow-origin">> => <<"*">>}, <<>>, Req);
        <<"DELETE">> ->
            new_game:delete(Req)
    end,
    {ok, Req3, State}.

get_all_games(Req) ->
    GameList = mnesia_tables:get_games(),
    lager:log(debug, ?MODULE, "sending back all games -> ~p", [GameList]),
    EncodedList = case GameList of
        <<>> -> <<>>;
        [] -> [];
        _ ->
            % PreparedBinary = erlang:iolist_to_binary(io_lib:format("~p", [Binary])),
            
            % StringConverted = [ iolist_to_binary(["{\"id\":", integer_to_binary(A),",\"name\":\"", B,"\",\"url\":\"", C,"\"}"]) || #games{id=A, name=B, url=C} <- GameList ],
            StringConverted = [ #{id=>A, name=>B, url=>C} || #games{id=A, name=B, url=C} <- GameList ],
            iolist_to_binary(jiffy:encode(StringConverted))
            % <<"["/binary, StringConverted/binary, "]"/binary>>
    end,
    lager:log(debug, ?MODULE, "Encoded list2 -> ~s", [EncodedList]),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>, <<"access-control-allow-origin">> => <<"*">>}, EncodedList, Req).