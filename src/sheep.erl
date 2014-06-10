-module(sheep).

-behaviour(cowboy_sub_protocol).

-export([upgrade/4]).
-export([param/4, param/3, param/2, validate/3, filter_params/2, parse_payload/2, generate_payload/2]).

upgrade(Req, Env, Handler, HandlerOpts) ->
    {ContentType, Req1} = cowboy_req:header(<<"content-type">>, Req, <<"application/json">>),
    {AcceptContentType, Req2} = cowboy_req:header(<<"accept">>, Req1, <<"application/json">>),
    try
        {ok, HandlerFun, Req3} = handler_fun(Req2, Handler, HandlerOpts),
        {ok, BindingsParams, QueryParams, BodyParams, Req4} = slurp_request(Req3, ContentType),
        {ok, Response} = Handler:HandlerFun(BindingsParams, QueryParams, BodyParams),
        {ok, Req5} = spit_response(Req4, 200, Response, AcceptContentType),
        {ok, Req5, Env}
    catch
        throw:{sheep, Tag, Code, Message} ->
            {ok, ReqE} = process_error(Req, AcceptContentType, Handler, Tag, Code, Message, null),
            {ok, ReqE, Env};
        _:Error ->
            {ok, ReqE} = process_error(Req, AcceptContentType, Handler, sheep, 500, <<"Unexpected error">>, Error),
            {ok, ReqE, Env}
    end.

handler_fun(Req, Handler, HandlerOpts) ->
    {Method, Req1} = cowboy_req:method(Req),
    case lists:keyfind(Method, 1, HandlerOpts ++ [{<<"POST">>, create}, {<<"GET">>, read}, {<<"PUT">>, update}, {<<"DELETE">>, delete}]) of
        false -> throw({sheep, sheep, 405, <<"Unknown request">>});
        {Method, HandlerFun} ->
            case erlang:function_exported(Handler, HandlerFun, 3) of
                true -> {ok, HandlerFun, Req1};
                false -> throw({sheep, sheep, 405, <<"Unknown request">>})
            end
    end.

process_error(Req, AcceptContentType, Handler, Tag, Code, Message, Error) ->
    case erlang:function_exported(Handler, error, 5) of
        true -> Handler:error(Req, Tag, Message, Error, erlang:get_stacktrace());
        false -> ok
    end,
    {ok, ReqE} = spit_response(Req, Code, {[{<<"tag">>, Tag}, {<<"message">>, Message}]}, AcceptContentType),
    {ok, ReqE}.

param(Params, Name, Type, Default) when is_atom(Name) ->
    param(Params, atom_to_binary(Name, utf8), Type, Default);

param(Params, Name, Type, Default) when is_list(Name) ->
    param(Params, list_to_binary(Name), Type, Default);

param({Params}, Name, Type, Default) ->
    case lists:keyfind(Name, 1, Params) of
        false -> Default;
        {Name, Value} ->
            ok = validate(Name, Value, Type),
            Value
    end.

param(Params, Name, Type) when is_atom(Name) ->
    param(Params, atom_to_binary(Name, utf8), Type);

param(Params, Name, Type) when is_list(Name) ->
    param(Params, list_to_binary(Name), Type);

param({Params}, Name, Type) ->
    case lists:keyfind(Name, 1, Params) of
        false -> throw({sheep, sheep, 400, <<<<"Param ">>/binary, Name/binary, <<" is mandatory">>/binary>>});
        {Name, Value} ->
            ok = validate(Name, Value, Type),
            Value
    end.

param(Params, Name) when is_atom(Name) ->
    param(Params, atom_to_binary(Name, utf8));

param(Params, Name) when is_list(Name) ->
    param(Params, list_to_binary(Name));

param({Params}, Name) ->
    case lists:keyfind(Name, 1, Params) of
        false -> throw({sheep, sheep, 400, <<<<"Param ">>/binary, Name/binary, <<" is mandatory">>/binary>>});
        {Name, Value} -> Value
    end.

validate(Name, Value, Type) when is_atom(Name) ->
    validate(atom_to_binary(Name, utf8), Value, Type);

validate(Name, Value, Type) when is_list(Name) ->
    validate(list_to_binary(Name), Value, Type);

validate(Name, Value, Fun) when is_function(Fun) ->
    ValidateResult = Fun(Value),
    if
        ValidateResult == false -> throw({sheep, sheep, 400, <<<<"Param ">>/binary, Name/binary, <<" is in wrong format">>/binary>>});
        true -> ok
    end;

validate(_Name, _Value, any) ->
    ok;

validate(Name, Value, binary) ->
    ok = validate(Name, Value, fun is_binary/1);

validate(Name, Value, integer) ->
    ok = validate(Name, Value, fun is_integer/1);

validate(Name, Value, float) ->
    ok = validate(Name, Value, fun is_float/1);

validate(Name, Value, boolean) ->
    ok = validate(Name, Value, fun is_boolean/1);

validate(Name, Value, {KeyType, ValueType}) ->
    case Value of
        {Key, Value} ->
            ok = validate(Name, Key, KeyType),
            ok = validate(Name, Value, ValueType);
        true -> throw({sheep, sheep, 400, <<<<"Param ">>/binary, Name/binary, <<" is in wrong format">>/binary>>})
    end;

validate(Name, Value, [Type]) ->
    ok = validate(Name, Value, fun is_list/1),
    lists:foreach(fun(SubValue) -> ok = validate(Name, SubValue, Type) end, Value).

filter_params(Fun, {Params}) ->
    {lists:filter(fun({Name, _}) -> Fun(Name) end, Params)}.

slurp_request(Req, ContentType) ->
    {BindingsParams, Req1} = bindings_params(Req),
    {QueryParams, Req2} = query_params(Req1),
    {BodyParams, Req3} = body_params(Req2, ContentType),
    {ok, BindingsParams, QueryParams, BodyParams, Req3}.

spit_response(Req, Code, Response, ContentType) ->
    Body = generate_payload(Response, ContentType),
    cowboy_req:reply(Code, [{<<"content-type">>, ContentType}], Body, Req).

bindings_params(Req) ->
    {Bindings, Req1} = cowboy_req:bindings(Req),
    NormalizedBindings = normalize_params({Bindings}),
    {NormalizedBindings, Req1}.

query_params(Req) ->
    {QueryParams, Req1} = cowboy_req:qs_vals(Req),
    NormalizedQueryParams = normalize_params({QueryParams}),
    {NormalizedQueryParams, Req1}.

body_params(Req, ContentType) ->
    case cowboy_req:has_body(Req) of
        true ->
            {ok, Body, Req1} = cowboy_req:body(Req),
            NormalizedBodyParams = parse_payload(Body, ContentType),
            {NormalizedBodyParams, Req1};
        false -> {{[]}, Req}
    end.

generate_payload(Data, ContentType) ->
    case ContentType of
        <<"*/*">> -> jiffy:encode(Data);
        <<"application/json">> -> jiffy:encode(Data);
        <<"application/x-msgpack">> -> msgpack:pack(Data, [{format, jiffy}])
    end.

parse_payload(Payload, ContentType) ->
    Params = case ContentType of
                 <<"application/json">> ->
                     try
                         jiffy:decode(Payload)
                     catch
                         _:_ -> throw({sheep, sheep, 500, <<"Can't parse JSON payload">>})
                     end;
                 <<"application/x-msgpack">> ->
                     try
                         {ok, ParamsMsgPack} = msgpack:unpack(Payload, [{format, jiffy}]),
                         ParamsMsgPack
                     catch
                         _:_ -> throw({sheep, sheep, 500, <<"Can't parse MsgPack payload">>})
                     end
             end,
    normalize_params(Params).

normalize_params({Tuples}) when is_list(Tuples) ->
    {lists:map(fun({Key, Value}) ->
                       {normalize_key(Key), normalize_params(Value)}
               end, Tuples)};

normalize_params(Value) when is_list(Value) ->
    lists:map(fun(V) -> normalize_params(V) end, Value);

normalize_params(Value) when is_binary(Value) ->
    SValue = binary_to_list(Value),
    case string:to_integer(SValue) of
        {IntValue, []} -> IntValue;
        _ -> case string:to_float(SValue) of
                 {FloatValue, []} -> FloatValue;
                 _ -> Value
             end
    end;

normalize_params(Value) -> Value.

normalize_key(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);

normalize_key(Key) when is_list(Key) ->
    list_to_binary(Key);

normalize_key(Key) -> Key.
