-module(last_fm_similars).

-behaviour(gen_server).

%% gen_server callbacks
%% LRU Least Recently Used
-export([init/1, handle_call/3, terminate/2]).
	 
-export([start/2, fetch/1]).


start(MaxSize, MaxAge) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [MaxSize, MaxAge], [?MODULE]).

stop() ->
	gen_server:cast(?MODULE, stop).

terminate(_Reason, Cache) ->
    Cache:delete(),
	ok.

init([MaxSize, MaxAge]) ->
	{ok, cache:new(?MODULE, MaxSize, MaxAge)}.
fetch(Artist) ->
	gen_server:call(?MODULE, {fetch, Artist}).

handle_call({fetch, Artist}, _From, Cache) ->
	case Cache:get(Artist) of
		miss ->
			Similar = fermal:artist_similar(Artist),
			Cache:put(Artist, Similar),
			Similar;
		{hit, Similar} ->
			Similar
	end.
	
