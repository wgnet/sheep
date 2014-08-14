-module(math_magic).

-export([init/3]).
-export([error_handler/6, read/3]).

init(_Transport, Req, _Opts) ->
    {upgrade, protocol, sheep, Req, []}.

error_handler(_Req, Tag, _Code, Message, Error, StackTrace) ->
    lager:error("~p: ~p ~p ~p", [Tag, Message, Error, StackTrace]),
    {[{<<"error">>, Message}]}.

param_error_handler(missing, Name) ->
    throw({sheep, math_magic, 400, <<<<"param ">>/binary, Name/binary, <<" is mandatory">>/binary>>});

param_error_handler(wrong, Name) ->
    throw({sheep, math_magic, 400, <<<<"param ">>/binary, Name/binary, <<" is in wrong format">>/binary>>}).

read(_, QueryParams, _) ->
    X = sheep:param(QueryParams, x, integer, fun param_error_handler/2),
    Y = sheep:param(QueryParams, y, integer, fun param_error_handler/2),
    Operation = sheep:param(QueryParams, operation, fun(O) -> O == <<"plus">> orelse O == <<"minus">> end, fun param_error_handler/2),
    Result = case Operation of
                 <<"plus">> -> X + Y;
                 <<"minus">> -> X - Y
             end,
    {ok, {[{<<"result">>, Result}]}}.
