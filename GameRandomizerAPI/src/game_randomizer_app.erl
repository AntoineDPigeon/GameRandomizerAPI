%%%-------------------------------------------------------------------
%% @doc GameRandomizerAPI public API
%% @end
%%%-------------------------------------------------------------------

-module(game_randomizer_app).

-behaviour(application).

-include_lib("game_types.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:set_env(mnesia, dir, <<".">>),
    lager:start(),
    lager:set_loglevel(lager_console_backend, debug),
    mnesia_tables:init([node()]),
    Dispatch = cowboy_router:compile([         
            {'_', [
                {"/", game_handler, []},
                {"/:id", game_handler, []}
            ]}     
    ]),
    KeyPath = code:priv_dir(game_randomizer) ++ "/server.key",
    CertPath = code:priv_dir(game_randomizer) ++ "/server.crt",
    {ok, _} = cowboy:start_clear(my_http_listener,
        [
            {port, 8089}
        ],
        #{
            env => #{dispatch => Dispatch}
        }
    ),
    % {ok, _} = cowboy:start_tls(my_http_listener,
    %     [
    %         {port, 8089},
    %         {keyfile, KeyPath},
    %         {certfile, CertPath}
    %     ],
    %     #{
    %         env => #{dispatch => Dispatch},
    %         middlewares => [digest_middleware, cowboy_router, cowboy_handler]
    %     }
    % ),
    lager:log(debug, ?MODULE, "Started HTTP server"),

    Result = file:read_file("games.txt"),
    case Result of
        {ok, <<>>} ->
            ok;
        {error, enoent} ->
            ok;
        {ok, Games} ->
            SplittedGames = binary:split(Games, <<"\n">>, [global]),
            SplittedGames2 = [ binary_to_list(X) || X <- SplittedGames],
            add_games(SplittedGames)
    end,
    game_randomizer_sup:start_link().
    
    % game_info:init(),
    % game_info:fetch_access_token().

add_games([]) ->
    ok;
add_games([<<>>|T]) ->
    add_games(T);
add_games([H|T]) ->
    lager:log(debug, ?MODULE, "~p", [H]),
    {DecodedGame} = jiffy:decode(H),
    lager:log(debug, ?MODULE, "~p", [DecodedGame]),
    Game = utils:get_game(DecodedGame, #games{}),
    mnesia_tables:add_game(Game),
    add_games(T).

stop(_State) ->
    ok.

%% internal functions
