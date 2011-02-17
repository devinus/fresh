% Fresh by Devin Torres <devin@devintorres.com>

-module(fresh_registry).
-behavior(gen_server).

-export([start/0, stop/0, handlers/0, add_handler/2, remove_handler/1,
         dispatch/3]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2,
         code_change/3]).

-include_lib("stdlib/include/qlc.hrl").

-record(handler, {pid, rules}).

start() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call({global, ?MODULE}, stop).

add_handler(Pid, Rules) ->
    gen_server:cast({global, ?MODULE},
                    {add_handler, #handler{pid=Pid, rules=Rules}}).

remove_handler(Pid) ->
    gen_server:cast({global, ?MODULE}, {remove_handler, Pid}).

handlers() ->
    gen_server:call({global, ?MODULE}, handlers).

dispatch(Method, Parts, Req) ->
    gen_server:call({global, ?MODULE}, {dispatch, Method, Parts, Req}).

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, ets:new(?MODULE, [])}.

handle_cast({add_handler, #handler{pid=Pid, rules=Rules}}, Tab) ->
    link(Pid),
    ets:insert(Tab, {Pid, Rules}),
    {noreply, Tab};
handle_cast({remove_handler, Pid}, Tab) ->
    unlink(Pid),
    ets:delete(Tab, Pid),
    {noreply, Tab};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call(handlers, _From, Tab) ->
    {reply, ets:tab2list(Tab), Tab};
handle_call({dispatch, Method, Parts, Req}, From, Tab) ->
    spawn(fun() ->
        Registry = ets:table(Tab, {n_objects, 1}),
        Q = qlc:q([Pid || {Pid, Rules} <- Registry, Rules(Method, Parts, Req)]),
        Reply = case qlc:e(Q) of
            [] ->
                {404, "text/plain", "Not Found"};
            [Pid | _] ->
                fresh_handler:handle(Pid, Method, Parts, Req)
        end,
        gen_server:reply(From, Reply)
    end),
    {noreply, Tab};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info({'EXIT', From, _Reason}, Tab) ->
    ets:delete(Tab, From),
    {noreply, Tab}.

terminate(_Reason, _State) -> ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
