-module(least_parse).

-export([make/0, file/1, explain/1]).

make() ->
	yecc:yecc("least", "least_yecc"),
	{ok, least_yecc} = c:c(least_yecc),
	leex:gen(least, least_lex),
	{ok, least_lex} = c:c(least_lex).
	
file(F) ->
    io:format("Parsing ~s.least~n", [F]),
    {ok, Stream} = file:open(F ++ ".least", read),
    Parse = handle(Stream, 1, [], 0),
    file:close(Stream),
    Parse.

explain(F) ->
	{ok, [R]} = file(F),
	io:format("~n~n~p~n~n",[R]),
	calc(R).

calc({calc, A, B, C}) ->
	oper(calc(A), calc(B), C);
	
calc({calc, A}) ->
	{data, A, fetch(A)}.
	
oper({data, T1, D1},{data, T2, D2},{oper, O}) ->
	{data, "("++T1++O++T2++")", D1++D2}.
	
fetch("radiohead") ->
	["foo","bar","baz"];
fetch("queen") ->
	["oof","rab","zab"];
fetch("mj") ->
	["one","two","three"];
fetch("pink") ->
	["eno","otw","eerht"];
fetch("floyd") ->
	["aaa","bbb","ccc"];
	
fetch(Artist) ->
	case cache:get(Artist) of
		miss ->
			Similar = fermal:artist_similar(Artist),
			cache:put(Artist, Similar);
		{hit, Similar} ->
			Similar
	end.

handle(Stream, LineNo, L, NErrors) ->
    handle1(io:requests(Stream, [{get_until,foo,least_lex,
			  tokens,[LineNo]}]), Stream, L, NErrors).

handle1({ok, Toks, Next}, Stream, L, Nerrs) ->
	io:format("Next: ~p~n", [Next]),
	Rt = least_yecc:parse(Toks),
	io:format("Toks: ~p ~n PARSED: ~p", [Toks, Rt]),
    case  Rt of
	{ok, Parse} ->
		io:format("Parse: ~p~n", [Parse]),
	    handle(Stream, Next, [Parse|L], Nerrs);
	{error, {Line, Mod, What}} ->
	    Str = apply(Mod, format_error, [What]),
	    io:format("** ~w ~s~n", [Line, Str]),
	    handle(Stream, Next, L, Nerrs+1);
	Other ->
	    io:format("Bad_parse:~p\n", [Other]),
	    handle(Stream, Next, L, Nerrs+1)
    end;
handle1({eof, _}, Stream, L, 0) ->
    {ok, lists:reverse(L)};
handle1({eof, _}, Stream, L, N) ->
    {error, N};
handle1(What, Stream, L, Nerrs) ->
    io:format("Here:~p\n", [What]),
    handle(Stream, 1, L, Nerrs+1).

first([H]) -> [];
first([H|T]) -> [H|first(T)];
first([]) -> [].