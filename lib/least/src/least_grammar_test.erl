-module(least_grammar_test).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

simple_union_test() ->
	{ok, {calc, {calc,"radiohead"}, {calc,"queen"}, {oper,"*"}}} =
	OK = least_parse:string("radiohead * queen"),
	OK = least_parse:string("(radiohead * queen)"),
	OK = least_parse:string("(radiohead*queen)").

composed_names_union_test() ->
	{ok, {calc, {calc,"radio head"}, {calc,"pink floyd"},{oper,"*"}}} = 
	OK = least_parse:string("radio head * pink floyd"),
	OK = least_parse:string("(radio head * pink floyd)"),
	OK = least_parse:string("(radio head*pink floyd)").

three_way_union_test() ->
	{ok,
		{calc, 
			{calc, {calc, "radiohead"}, {calc, "queen"}, {oper, "*"} },
		 	{calc, "pink floyd"},
		 	{oper, "*"}
		}
	} = 
	OK = least_parse:string("(radiohead * queen) * pink floyd"),
	OK = least_parse:string("radiohead * queen * pink floyd").
		
three_way_union_inverse_test() ->
	{ok,
		{calc, 
		 	{calc, "pink floyd"},
			{calc, {calc, "radiohead"}, {calc, "queen"}, {oper, "*"} },
		 	{oper, "*"}
		}
	 } = OK = least_parse:string("(pink floyd) * (radiohead * queen)"),
	OK = least_parse:string("pink floyd * (radiohead * queen)"),
	OK = least_parse:string("(pink floyd * (radiohead * queen))").

dot_in_the_name_test() ->
	{ok, {calc, {calc,"R.E.M"}, {calc, "U2"}, {oper,"*"}}} = least_parse:string("R.E.M * U2").