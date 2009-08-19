-module(least_parse).

-export([string/1]).
	
string(S) ->
	V = least_lex:string(S),
	io:format("Lexer -- S: ~p V: ~p~n",[S, V]),
	{ok, Tokens, 1} = V, 
	least_yecc:parse(Tokens).
