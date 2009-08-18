-module(least_supervisor).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Args) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, Args).

init([TrackerPort, RestPort]) ->
	process_flag(trap_exit, true),
	io:format("~p (~p) starting...~n", [?MODULE, self()]),
	Resolver= {least_resolver, {least_resolver, start_link, []},
			permanent, 5000, worker, [least_resolver]},
	Web =	{least_web, {least_web, start, []},
			permanent, 5000, worker, [least_web]},
	{ok, {{one_for_one, 5, 30}, [Resolver, Web]}}.

