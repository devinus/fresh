% Copyright (c) 2010, Devin Torres <devin@devintorres.com>

-module(fresh).
-behaviour(application).
-behavior(supervisor).

-export([start/0, start/2, stop/0, stop/1, init/1]).

start() -> application:start(fresh).
stop()  -> application:stop(fresh).

start(_Type, _Args) ->
    supervisor:start_link({local, fresh_sup}, ?MODULE, []).

stop(_State) -> ok.

init([]) ->
    Server   = {fresh_server, {fresh_server, start, []},
                permanent, 5000, worker, [fresh_server]},
    Registry = {fresh_registry, {fresh_registry, start, []},
                permanent, 5000, worker, [fresh_registry]},
    {ok, {{one_for_one, 10, 10}, [Registry, Server]}}.
