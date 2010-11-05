% Copyright (c) 2010, Devin Torres <devin@devintorres.com>

-module(fresh_server).
-export([start/0, start/2, stop/0, stop/1, loop/1]).

-define(SERVER, "Fresh/0.1 (you know it's fresh)").

start() ->
    Port = case application:get_env(fresh, port) of
        {ok, P} -> P;
        _       -> 7235
    end,
    mochiweb_http:start([{name, ?MODULE}, {loop, fun loop/1}, {port, Port}]).
start(_Type, _Args) ->
    start().

stop() ->
    mochiweb_http:stop(?MODULE).
stop(_State) ->
    stop(),
    ok.

loop(Req) ->
    Method = Req:get(method),
    Parts = string:tokens(Req:get(path), "/"),
    Reply = fresh_registry:dispatch(Method, Parts, Req),
    case Reply of
        {ContentType, Resp} ->
            case ContentType of
                json -> json(200, Resp, Req);
                html -> html(200, Resp, Req);
                text -> text(200, Resp, Req);
                data -> data(200, Resp, Req);
                _ ->    respond(200, ContentType, Resp, Req)
            end;
        {Status, ContentType, Resp} ->
            case ContentType of
                json -> json(Status, Resp, Req);
                html -> html(Status, Resp, Req);
                text -> text(Status, Resp, Req);
                data -> data(Status, Resp, Req);
                _ ->    respond(Status, ContentType, Resp, Req)
            end;
        {Status, ContentType, Headers, Resp} ->
            case ContentType of
                json -> json(Status, Headers, Resp, Req);
                html -> html(Status, Headers, Resp, Req);
                text -> text(Status, Headers, Resp, Req);
                data -> data(Status, Headers, Resp, Req);
                _ ->    respond(Status, ContentType, Headers, Resp, Req)
            end;
        Resp when is_list(Reply) orelse is_binary(Reply) ->
            html(200, Resp, Req);
        ok -> ok
    end.

json(Status, Resp, Req) ->
    json(Status, [], Resp, Req).

json(Status, Headers, Resp, Req) ->
    Json = mochijson2:encode(Resp),
    respond(Status, "application/json", Headers, Json, Req).

html(Status, Resp, Req) ->
    html(Status, [], Resp, Req).

html(Status, Headers, Resp, Req) ->
    respond(Status, "text/html", Headers, Resp, Req).

text(Status, Resp, Req) ->
    text(Status, [], Resp, Req).

text(Status, Headers, Resp, Req) ->
    respond(Status, "text/plain", Headers, Resp, Req).

data(Status, Resp, Req) ->
    data(Status, [], Resp, Req).

data(Status, Headers, Resp, Req) ->
    respond(Status, "application/octet-stream", Headers, Resp, Req).

respond(Status, ContentType, Resp, Req) ->
    respond(Status, ContentType, [], Resp, Req).

respond(Status, ContentType, Headers, Resp, Req) ->
    Req:respond({Status, [{"Server", ?SERVER},
                          {"Content-Type", ContentType} | Headers], Resp}).
