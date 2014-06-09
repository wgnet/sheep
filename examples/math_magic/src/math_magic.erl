-module(math_magic).

-export([init/3]).
-export([error/5, read/3]).

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep, Req, []}.

error(_Req, Tag, Message, Error, Stacktrace) ->
    lager:error("~p: ~p ~p ~p", [Tag, Message, Error, Stacktrace]).

read(_, QueryParams, _) ->
    X = sheep:param(QueryParams, <<"x">>, integer),
    Y = sheep:param(QueryParams, <<"y">>, integer),
    Operation = sheep:param(QueryParams, <<"operation">>,
                            fun(O) ->
                                    O == <<"plus">> orelse O == <<"minus">>
                            end),
    Result = case Operation of
                 <<"plus">> -> X + Y;
                 <<"minus">> -> X - Y
             end,
    {ok, {[{<<"result">>, Result}]}}.
