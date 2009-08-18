-module(least_parse).

-export([make/0, string/1, explain/1, fetch/1]).

make() ->
	yecc:yecc("least", "least_yecc"),
	{ok, least_yecc} = c:c(least_yecc),
	leex:file(least_lex),
	%leex:file(least)
	{ok, least_lex} = c:c(least_lex).
	
string(S) ->
	V = least_lex:string(S),
	io:format("Lexer -- S: ~p V: ~p~n",[S, V]),
	{ok, Tokens, 1} = V, 
	least_yecc:parse(Tokens).

explain(S) ->
	{ok, [R]} = string(S),
	io:format("~n~n~p~n~n",[R]),
	calc(R).

calc({calc, A, B, C}) ->
	oper(calc(A), calc(B), C);
	
calc({calc, A}) ->
	{data, A, fetch(A)}.
	
oper({data, T1, D1},{data, T2, D2},{oper, O}) ->
	{data, "("++T1++O++T2++")", D1++D2}.
	
fetch(Artist) ->
	case cache:get(Artist) of
		miss ->
			Similar = fermal:artist_similar(Artist),
			cache:put(Artist, Similar),
			{miss, []};
		{hit, Similar} ->
			{hit, []}
	end.
