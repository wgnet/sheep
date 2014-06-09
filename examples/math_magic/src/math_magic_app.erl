-module(math_magic_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Routes = [
              {'_', [
                     {<<"/math_magic">>, math_magic, []}
                    ]}
             ],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_http(http, 10, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
    math_magic_sup:start_link().

stop(_State) ->
    ok.
