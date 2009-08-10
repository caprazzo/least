-module(lru_cache_obj, [DataTab, UsageTab, MaxSize, MaxAge]).


-export([get/1, put/2, delete/0]).

%% @doc gets an object from the cache
%% Expired objects will not be returned
get(Key) ->
	case ets:lookup(UsageTab, Key) of
		[] -> miss;	
		[{Key, InsertTime, _LastHit}] ->
			case timer:now_diff(erlang:now(), InsertTime) > MaxAge of
				true ->
					miss;
				false ->
					ets:update_element(UsageTab, Key, {3, erlang:now()}),
					[{Key, Value}] = ets:lookup(DataTab, Key),
					{hit, Value}
			end
	end.

%% @doc Store an object
put(Key, Value) ->
	ets:insert(DataTab, {Key, Value}),
	Now = erlang:now(),
	ets:insert(UsageTab, {Key, Now, Now}),
	cache_purge().

delete() ->
	ets:delete(UsageTab),
	ets:delete(DataTab).
	
%% @doc removes all cache entries older that MaxAge,
%% plus enough (Least Recently Used) elements to reduce
%% cache size to TargetSize
cache_purge() ->
	{Dead, Alive} = cache_purge_expired(UsageTab, DataTab, MaxAge),
	case Alive > MaxSize of
		true ->
			D = cache_purge_least_used(UsageTab, DataTab, MaxSize - Alive),
			{Dead+D, Alive-D};
		false ->
			{Dead, Alive}
	end.

%% @doc deletes all cache entries older than MaxAge
%% returns {NumDead, NumAlive}
cache_purge_expired(UsageTab, DataTab, MaxAge) ->
	Now = erlang:now(),
	PurgeDeadFun = fun({Key, InsertTime, _LastHit}, {Dead, Alive}) ->
		Age = timer:now_diff(Now, InsertTime),
		case Age > MaxAge of
			true ->
				ets:delete(UsageTab, Key),
				ets:delete(DataTab, Key),
				{Dead+1, Alive};
			false ->
			   {Dead, Alive+1}
		end
	end,
	ets:foldl(PurgeDeadFun, {0,0}, UsageTab).

%% @doc deletes at most N Least Recently Used objects
%% in UsageTab and DataTab
cache_purge_least_used(UsageTab, DataTab, N) ->
	
	LRUTree = ets:foldl(fun({Key, _InsertTime, LastHit}, Tree) ->
		gb_trees:insert(LastHit, Key, Tree)
	end, gb_trees:new(), UsageTab),
	
	LRUValues = gb_trees:values(LRUTree),
	ToRemove = lists:sublist(LRUValues, N), 
	
	lists:foreach(fun(Key) ->
		ets:delete(UsageTab, Key),
		ets:delete(DataTab, Key)
	end, ToRemove),
	length(ToRemove).

