-module(lru_cache).

-export([new/3]).

new(Name, MaxSize, MaxAge) ->
	DataTabName = list_to_atom(atom_to_list(Name) ++ "_data"),
	UsageTabName = list_to_atom(atom_to_list(Name) ++ "_usage"),
	DataTab = ets:new(DataTabName, [set, private]),
	UsageTab = ets:new(UsageTabName, [set, private]),
	lru_cache_obj:new(DataTab, UsageTab, MaxSize, MaxAge * 1000 * 1000).