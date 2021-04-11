-module(game_info).

-export([init/0, fetch_access_token/0]).

init() ->
    ets:new(game_db, [named_table]).

fetch_access_token() ->
    ClientID = application:get_env(game_randomizer, clientID, ""),
    ClientSecret = application:get_env(game_randomizer, clientSecret, ""),
    Url = iolist_to_binary(["https://id.twitch.tv/oauth2/token?client_id=", ClientID, "&client_secret=", ClientSecret, "&grant_type=client_credentials"]),
    Request = {Url, []},
    Result = httpc:request(post, Request, [], []),
    lager:log(debug, ?MODULE, "Received ~p", Result).