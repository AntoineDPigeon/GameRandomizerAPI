-module(digest_middleware).

-export([execute/2]).

-behaviour(cowboy_middleware).

-define(MAXNOONCE, 30000).

execute(Req, State) ->
    case isPublic(cowboy_req:path(Req)) of
      true ->
        {ok, Req, State};
      false ->
          digestAuthentication(Req, State)
    end.

digestAuthentication(Req, State) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {digest, Props} when is_list(Props) ->
            Realm = proplists:get_value(<<"realm">>, Props),
            Nonce = proplists:get_value(<<"nonce">>, Props),
            Method = cowboy_req:method(Req),
            RealUri = getDigestUri(Req), %% the authentication header has a property that describes the uri, but we don't trust the user
            Response = erlang:binary_to_list(proplists:get_value(<<"response">>, Props)),
            PropOpaque = proplists:get_value(<<"opaque">>, Props),
            Opaque = case PropOpaque of
                undefined -> "";
                <<>> -> "";
                _ -> erlang:binary_to_list(PropOpaque)
            end,
            case digestHash(getUsername(), Realm, getPassword(), Method, RealUri, Nonce) of
                Response -> 
                    Req1 = setDigestHeader(Req, Opaque),
                    {ok, Req1, State};
                _ ->
                    Req1 = setDigestHeader(Req, Opaque),
                    {stop, cowboy_req:reply(401, Req1)}
            end; 
	    _ ->
	        Req1 = setDigestHeader(Req, erlang:integer_to_list(rand:uniform(?MAXNOONCE), 16)),
            {stop, cowboy_req:reply(401, Req1)}
            
    end.

setDigestHeader(Req, Opaque) ->
    Nonce = rand:uniform(?MAXNOONCE),
    Str = erlang:integer_to_list(Nonce, 16),
    Bin =  erlang:list_to_binary(["digest realm=\"cowboy\", nonce=\"", Str, "\", opaque=\"",Opaque,"\""]),
    cowboy_req:set_resp_header(<<"www-authenticate">>, Bin, Req).

getDigestUri(Req) ->
    A = cowboy_req:path(Req),
    case cowboy_req:qs(Req) of
        <<>> ->
            A;
        B when is_binary(B) ->
            <<A/binary, "?", B/binary>>
    end.

isPublic(_) ->
    false.

digestHash(Username, Realm, Password, Method, DigestUri, Nonce) -> 
    HA1 = toHex(erlang:md5([Username, ":", Realm, ":", Password])),
    HA2 = toHex(erlang:md5([Method, ":", DigestUri])),
    toHex(erlang:md5(([HA1, ":", Nonce, ":", HA2]))).

toHex(Bin) when is_binary(Bin) ->
    [hexChar(X) || <<X:4>> <= Bin].

hexChar(Num) when Num < 10 andalso Num >= 0-> 
    $0 + Num;
hexChar(Num) when Num < 16 -> 
    $a + Num - 10.

getUsername() ->
    application:get_env(game_randomizer, username, <<>>).

getPassword() ->
    application:get_env(game_randomizer, password, <<>>).