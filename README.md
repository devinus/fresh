Fresh - The freshest Erlang web framework
=========================================

## What's so fresh about it?

Fresh is a Sinatra inspired Erlang web framework
based on Mochiweb that supports registering multiple
"web handlers" that are dispatched to when you access
a Fresh instance. This allows you to theoretically
serve sections of your website from multiple physically
separate computers. For instance, your main pages could
serve from a server in Hong Kong while your user pages
are served from Marfa, TX.

## What's it look like?

### example_web_handler.erl

    -module(example_web_handler).
    -behaviour(fresh_handler).

    -export([dispatch_rules/0, 'GET'/2]).

    dispatch_rules() ->
        fun('GET', ["fresh"], _Req) -> true;
           ('POST', ["fresh"], _Req) -> true;
           (_, _, _) -> false end.

    'GET'(["fresh"], _Req) ->
        "So fresh!".

    'POST'(["fresh"], Req) ->
        Body = Req:recv_body(),
        {json, [{ok, Body}]}.

### example.erl

    -module(example).
    -behaviour(application).
    -behaviour(supervisor).

    -export([start/0, stop/0]).
    -export([init/1, start/2, stop/1]).

    start() -> application:start(?MODULE).
    stop()  -> application:stop(?MODULE).

    start(_Type, _Args) ->
        supervisor:start_link({local, example_sup}, ?MODULE, []).

    stop(_State) -> ok.

    init([]) ->
        WebHandler = {example_web_handler,
                      {fresh_handler, start_link, [example_web_handler]},
                       permanent, 5000, worker, [fresh_handler]},
        {ok, {{one_for_one, 10, 10}, [WebHandler]}}.

### example.app

    {application, example, [
        {description, "A Fresh example app"},
        {vsn, "0.1"},
        {applications, [kernel, stdlib, sasl]},
        {modules, [example, example_web_handler]},
        {registered, [example]},
        {mod, {example, []}}
    ]}.

## Options

- `port`: port number, default: 7235

## TODO

- Allow custom error handlers (easy)
- Re-register dispatch rules during hot OTP
  upgrade (harder)
- Statically analyze handler AST during registration
  to create preliminary dispatch rules (hardest)
