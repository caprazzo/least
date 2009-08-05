-module(cache).

-behaviour(gen_server).

%% gen_server callbacks
%% LRU Least Recently Used
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
	 
-export([start/0, stop/0, put/2, get/1]).

-export([dump/0, test/0]).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], [?MODULE]).

stop() ->
	gen_server:cast(?MODULE, stop).

terminate(_Reason, _State) ->
    ok.

init([]) ->
	DataTab = ets:new(data, [set, private]),
	UsageTab = ets:new(usage, [ordered_set, private]),
	MaxSize = 4,
	{ok, DataTab}.
	
test() ->
	cache:put(x,"xxxx"),
	cache:put(y,"yyyyy"),
	cache:put(z,"zzzzz").

put(Key, Value) ->
	gen_server:cast(?MODULE, {put, Key, Value}).

get(Key) ->
	gen_server:call(?MODULE, {get, Key}).

dump() ->
	gen_server:call(?MODULE, dump).

handle_call({get, Key}, _From, DataTab) ->
	case ets:lookup(DataTab, Key) of
		[{Key, Value}] -> {reply, {hit, Value}, DataTab};
		[] -> {reply, miss, DataTab}
	end;

handle_call(dump, _From, DataTab) ->
	{reply, [ets:tab2list(DataTab)], DataTab}.

handle_cast(stop,  State) ->
	{stop, normal, State};

handle_cast({put, Key, Value}, {DataTab, UsageTab, _Max}=State) ->
	ets:insert(DataTab, {Key, Value}),
	case ets:lookup(UsageTab, Key) of
		[{Key,_Hits}] -> nil;
		[] -> ets:insert(UsageTab, {Key,1})
	end,
	{noreply, State}.

purge(Tab) ->
	ets:foldl(Function, Acc0, Tab)

