-module(least_resolver).

-behaviour(gen_server).

-export([start_link/0, resolve/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	process_flag(trap_exit, true),
	fermal:start(),
	MaxCount = 100,
	MaxAge = 6000,
	{ok, simple_cache:new([{max_count,MaxCount}, {max_age, MaxAge}])}.

resolve(Expression) ->
	io:format("~p:resolve(~p)~n", [?MODULE, Expression]),
	gen_server:call(?MODULE, {expression, Expression}).

terminate(_Reason, _State) ->
    ok.

handle_call({expression, Expression}, _From, Cache) ->
	{reply, calc(Expression, Cache), Cache}. 

calc({calc, A, B, C}, Cache) ->
	oper(calc(A, Cache), calc(B, Cache), C);
	
calc({calc, A}, Cache) ->
	{data, A, fetch(A, Cache)}.
	
oper({data, T1, D1}, {data, T2, D2}, {oper, "+"=O}) ->
	{data, "("++T1++O++T2++")", union(D1,D2) };

oper({data, T1, D1}, {data, T2, D2}, {oper, "/"=O}) ->
	io:format(" INTERSECTION ~p / ~p~n",[T1,T2]),
	{data, "("++T1++O++T2++")", intersection(D1, D2)};

oper({data, T1, D1}, {data, T2, D2}, {oper, "-"=O}) ->
	{data, "("++T1++O++T2++")", disjoint(D1, D2)}.

%% union uses an ets set to build a list of unique 
%% artists. Not best approach, but quick and dirty.
union(D1, D2) ->
	Ex = ets:new(union, [set,private]),
	ets:insert(Ex, D1 ++ D2),
	R = ets:tab2list(Ex),
	ets:delete(Ex),
	R.

intersection(D1, D2) ->
	{Names2, _} = lists:unzip(D2),
	lists:filter(fun({Name, _Data}) -> lists:member(Name, Names2) end, D1).

disjoint(D1, D2) ->
	{Names2, _} = lists:unzip(D2),
	lists:filter(fun({Name, _Data}) -> case lists:member(Name, Names2) of true -> false; false-> true end end, D1).

fetch(Artist, Cache) ->
	case Cache:get(Artist) of
		miss ->
			Similar = index(lists:sublist(fermal:artist_similar( edoc_lib:escape_uri(Artist)), 3),[]),
			Cache:put(Artist, Similar),
			Similar;
		{hit, Similar} ->
			Similar
	end.

index([], Acc) -> Acc;
index([{artist, Props}|Artists], Acc) ->
	index(Artists, [{proplists:get_value("name", Props), {artist, Props}}|Acc]).
