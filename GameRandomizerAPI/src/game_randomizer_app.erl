%%%-------------------------------------------------------------------
%% @doc GameRandomizerAPI public API
%% @end
%%%-------------------------------------------------------------------

-module(game_randomizer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:set_env(mnesia, dir, <<".">>),
    lager:start(),
    lager:set_loglevel(lager_console_backend, debug),
    game_table:init([node()]),
    Dispatch = cowboy_router:compile([         
            {'_', [
                {"/", game_handler, []}
                % {"/get", get_game_handler, []},
                % {"/delete", delete_game_handler, []}
            ]}     
    ]),     
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}
        % middlewares => [cowboy_router, session_cowboy_middleware, cowboy_handler]
    }),
    lager:log(debug, ?MODULE, "Started HTTP server"),
    game_randomizer_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
