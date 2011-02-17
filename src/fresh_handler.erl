% Fresh by Devin Torres <devin@devintorres.com>

-module(fresh_handler).
-behaviour(gen_server).

-export([handle/4]).
-export([behaviour_info/1, start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(ERROR(Msg), error_logger:error_msg(Msg)).

-record(state, {handler}).

behaviour_info(callbacks) -> [{dispatch_rules, 0}];
behaviour_info(_Other) -> undefined.

handle(Handler, Method, Parts, Req) ->
    gen_server:call(Handler, {handle, Method, Parts, Req}).

start_link(Handler) ->
    gen_server:start_link(?MODULE, Handler, []).

init(Handler) ->
    fresh_registry:add_handler(self(), Handler:dispatch_rules()),
    {ok, #state{handler=Handler}}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call({handle, Method, Parts, Req}, From, #state{handler=Handler}=State) ->
    spawn(fun() ->
        try
            Reply = Handler:Method(Parts, Req),
            gen_server:reply(From, Reply)
        catch
            Class:Reason ->
                Format = "Exception: ~p:~p~nStacktrace:~n~p~n",
                Stacktrace = erlang:get_stacktrace(),
                Msg = io_lib:format(Format, [Class, Reason, Stacktrace]),
                ?ERROR(Msg),
                Req:respond({500, [{"Content-Type", "text/plain"}], Msg}),
                gen_server:reply(From, ok)
        end
    end),
    {noreply, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
