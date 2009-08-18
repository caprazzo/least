-module(least).

%% Application root
-behaviour(application).
-export([start/0, start/2, stop/1, stop/0]).

start() ->
	application:start(?MODULE).

start(_Type, _Args) ->
	least_supervisor:start_link(_Args).

stop() ->
	application:stop(?MODULE).

stop(_State) ->
	ok.