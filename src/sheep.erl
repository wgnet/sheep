-module(sheep).

-behaviour(cowboy_sub_protocol).

-export([upgrade/4]).
-export([param/5, param/4, params/4, validate/4, nullable/2, parse_payload/2, generate_payload/2, normalize_params/1]).

upgrade(Req, Env, Handler, HandlerOpts) ->
    {ContentType, Req1} = cowboy_req:header(<<"content-type">>, Req, <<"application/json">>),
    {AcceptContentType, Req2} = cowboy_req:header(<<"accept">>, Req1, <<"application/json">>),
    try
        {ok, HandlerFun, Req3} = handler_fun(Req2, Handler, HandlerOpts),
        {ok, BindingsParams, QueryParams, BodyParams, Req4} = slurp_request(Req3, ContentType),
        {ok, CodeAndResponse} = Handler:HandlerFun(BindingsParams, QueryParams, BodyParams),
        {Code, Response} = parse_code_and_response(CodeAndResponse, 200),
        {ok, Req5} = spit_response(Req4, Code, Response, AcceptContentType),
        {ok, Req5, Env}
    catch
        throw:{sheep, Tag, CodeE, Message} ->
            {ok, ReqE} = process_error(Req, AcceptContentType, Handler, Tag, CodeE, Message, null),
            {ok, ReqE, Env};
        _:Error ->
            {ok, ReqE} = process_error(Req, AcceptContentType, Handler, sheep, 500, <<"unexpected error">>, Error),
            {ok, ReqE, Env}
    end.

handler_fun(Req, Handler, HandlerOpts) ->
    {Method, Req1} = cowboy_req:method(Req),
    case lists:keyfind(Method, 1, HandlerOpts ++ [{<<"POST">>, create}, {<<"GET">>, read}, {<<"PUT">>, update}, {<<"DELETE">>, delete}]) of
        false -> throw({sheep, sheep, 405, <<"unknown request">>});
        {Method, HandlerFun} ->
            case erlang:function_exported(Handler, HandlerFun, 3) of
                true -> {ok, HandlerFun, Req1};
                false -> throw({sheep, sheep, 405, <<"unknown request">>})
            end
    end.

process_error(Req, AcceptContentType, Handler, Tag, Code, Message, Error) ->
    CodeAndResponse = case erlang:function_exported(Handler, error_handler, 5) of
                          true -> Handler:error_handler(Req, Tag, Code, Message, Error);
                          false -> {[{<<"tag">>, Tag}, {<<"message">>, Message}]}
                      end,
    {FinalCode, Response} = parse_code_and_response(CodeAndResponse, Code),
    spit_response(Req, FinalCode, Response, AcceptContentType).

parse_code_and_response({Code, Response}, _DefaultCode) ->
    {Code, Response};

parse_code_and_response(Response, DefaultCode) ->
    {DefaultCode, Response}.

param({Params}, Name, Type, Default, ErrorFun) ->
    NormalizedName = normalize_key(Name),
    case lists:keyfind(NormalizedName, 1, Params) of
        false -> Default;
        {NormalizedName, Value} ->
            ok = validate(NormalizedName, Value, Type, ErrorFun),
            Value
    end.

param({Params}, Name, Type, ErrorFun) ->
    NormalizedName = normalize_key(Name),
    case lists:keyfind(NormalizedName, 1, Params) of
        false -> ErrorFun(missing, NormalizedName);
        {NormalizedName, Value} ->
            ok = validate(NormalizedName, Value, Type, ErrorFun),
            Value
    end.

params({Params}, FilterFun, Type, ErrorFun) ->
    FilteredParams = lists:filter(fun({Name, _}) -> FilterFun(Name) end, Params),
    lists:foreach(fun({Name, Value}) -> validate(Name, Value, Type, ErrorFun) end, FilteredParams),
    {FilteredParams}.

nullable(ValidateFun, Value) ->
    if
        Value == null -> ok;
        true -> ValidateFun(Value)
    end.

validate(Name, Value, ValidateFun, ErrorFun) when is_function(ValidateFun) ->
    ValidateResult = ValidateFun(Value),
    if
        ValidateResult == false -> ErrorFun(wrong, normalize_key(Name));
        true -> ok
    end;

validate(_Name, _Value, any, _ErrorFun) ->
    ok;

validate(Name, Value, binary, ErrorFun) ->
    ok = validate(Name, Value, fun is_binary/1, ErrorFun);

validate(Name, Value, nullable_binary, ErrorFun) ->
    ok = validate(Name, Value, fun(V) -> nullable(fun is_binary/1, V) end, ErrorFun);

validate(Name, Value, integer, ErrorFun) ->
    ok = validate(Name, Value, fun is_integer/1, ErrorFun);

validate(Name, Value, nullable_integer, ErrorFun) ->
    ok = validate(Name, Value, fun(V) -> nullable(fun is_integer/1, V) end, ErrorFun);

validate(Name, Value, float, ErrorFun) ->
    ok = validate(Name, Value, fun is_float/1, ErrorFun);

validate(Name, Value, nullable_float, ErrorFun) ->
    ok = validate(Name, Value, fun(V) -> nullable(fun is_float/1, V) end, ErrorFun);

validate(Name, Value, boolean, ErrorFun) ->
    ok = validate(Name, Value, fun is_boolean/1, ErrorFun);

validate(Name, Value, nullable_boolean, ErrorFun) ->
    ok = validate(Name, Value, fun(V) -> nullable(fun is_boolean/1, V) end, ErrorFun);

validate(Name, Value, {KeyType, ValueType}, ErrorFun) ->
    case Value of
        {Key, Value} ->
            ok = validate(Name, Key, KeyType, ErrorFun),
            ok = validate(Name, Value, ValueType, ErrorFun);
        true -> ErrorFun(wrong, normalize_key(Name))
    end;

validate(Name, Value, [Type], ErrorFun) ->
    ok = validate(Name, Value, fun is_list/1, ErrorFun),
    lists:foreach(fun(SubValue) -> ok = validate(Name, SubValue, Type, ErrorFun) end, Value).

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
        <<"application/json">> -> jiffy:encode(Data);
        <<"application/x-msgpack">> -> msgpack:pack(Data, [{format, jiffy}]);
        _AnyOtherContentType -> jiffy:encode(Data)
    end.

parse_payload(Payload, ContentType) ->
    case ContentType of
        <<"application/json">> ->
            try
                jiffy:decode(Payload)
            catch
                _:_ -> throw({sheep, sheep, 500, <<"can't parse JSON payload">>})
            end;
        <<"application/x-msgpack">> ->
            try
                {ok, ParamsMsgPack} = msgpack:unpack(Payload, [{format, jiffy}]),
                         ParamsMsgPack
            catch
                _:_ -> throw({sheep, sheep, 500, <<"can't parse MsgPack payload">>})
            end;
        _ ->
            try
                normalize_params({cow_qs:parse_qs(Payload)})
            catch
                _:_ -> Payload
            end
    end.

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
    unicode:characters_to_binary(Key, utf8);

normalize_key(Key) -> Key.
