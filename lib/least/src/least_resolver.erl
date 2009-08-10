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
	MaxSize = 100,
	MaxAge = 6000,
	{ok, lru_cache:new(?MODULE, MaxSize, MaxAge)}.

resolve(Expression) ->
	io:format("~p:resolve(~p)", [?MODULE, Expression]),
	gen_server:call(?MODULE, {expression, Expression}).

terminate(_Reason, _State) ->
    ok.

handle_call({expression, Expression}, _From, Cache) ->
	{reply, calc(Expression, Cache), Cache}. 

calc({calc, A, B, C}, Cache) ->
	oper(calc(A, Cache), calc(B, Cache), C);
	
calc({calc, A}, Cache) ->
	{data, A, fetch(A, Cache)}.
	
oper({data, T1, D1},{data, T2, D2},{oper, O}) ->
	{data, "("++T1++O++T2++")", D1++D2}.

oper({data, T1, D1}, {data, T2, D2}, {oper, "+"}) ->
	{data, "("++T1++O++T2++")", D1++D2}.

oper({data, T1, D1}, {data, T2, D2}, {oper, "-"}) ->
	{data, "("++T1++O++T2++")", D1--D2}.

as_names([], Acc) ->
	lists:reverse(Acc);
as_names([{artist, Proplist}|Artists], Acc) ->
	as_names(Artists, [proplists:get_value("name", Proplist)|Acc]).

fetch(Artist, Cache) ->
	case Cache:get(Artist) of
		miss ->
			Similar = fermal:artist_similar(Artist),
			Cache:put(Artist, Similar),
			Similar;
		{hit, Similar} ->
			Similar
	end.

get_artist_list([], Acc) ->
  	lists:reverse(Acc);
get_artist_list([{artist, Proplist}|Artists], Acc) ->
	io:format("Proplist: ~p~n",[Proplist]),
	get_artist_list(Artists, [
			{struct, [
				{"name", proplists:get_value("name", Proplist)},
			 	{"match",proplists:get_value("match", Proplist)},
				{"url", proplists:get_value("url", Proplist)}
			]}|Acc]).

