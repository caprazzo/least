Nonterminals list elements element.
Terminals calc fetch artist atom oper '(' ')'.

Rootsymbol list.

elements -> '(' ')' : io:format("list ( ):~p ~p~n",['$1','$2']), '$2'.

list -> '(' elements ')' : '$2'.
elements -> element : '$1'.
elements -> element element elements : {calc, min('$1'), min('$2'), min('$3')}.
%elements -> '$empty' : empty.
%% element -> atom atom atom : mak('$1', '$2', '$3').
element -> atom : {atom,_,V}='$1', V.
element -> list : '$1'.
element -> oper : '$1'.
%% element -> artist oper artist : mak('$1','$2','$3').
element -> artist oper artist : xa('$1','$2','$3').
element -> fetch : ax('$1').

list -> '(' calc oper calc ')' : {calc, min('$2'), min('$3'), min('$4')}.

%{ok,[[{calc,{{calc,1,"queen"},
%             {oper,1,"-"},
%             {calc,1,"mickael jackson"}}},
%      [{oper,1,"+"},
%       {calc,{{calc,1,"radiohead"},
%              {oper,1,"-"},
%              {calc,1,"beck"}}}]]]}

%list -> '(' elements ')': io:format("list -> '(' elements ')': ~p~p~p ~n",['$1','$2','$3']), '$2'.

%[
%	['queen', 'micheal jackson', '-']
%	['radiohead', 'beck', '-],
%	'+'
%]

%{calc, 
%	[
%		{calc, [{artist,'queen'}, {artist, 'micheal jackson'}, {op, '-'}]},
%		{calc, ['radiohead', 'beck', '-']},
%		{op, '+'}
%	]}

% PARSE GOAL:
% {ok,[	
%		{calc,
%			{calc, {calc,"queen"}, {oper,"-"}, {calc,"mickael jackson"}},
%			{oper,"+"},
%			{calc, {calc,"radiohead"}, {oper,"-"}, {calc,"beck"}}
%		}
%	]
%}

%elements -> element : io:format("elements -> element: $1:~p $2:~p ~n", ['$1','2']), '$1'.

%elements -> element elements : io:format("elements -> element elements: |~p|~p|~n",['$1','$2']), '$1'.

%element -> list : io:format("element -> list :~p~n",['$1']), '$1'.  

%element -> atom : io:format("element -> atom :~p~n~n", ['$1']), '$1'.

%element -> oper : io:format("element -> oper :~p~n~n",['$1']), '$1'.

%element -> js : io:format("js~n"), $1. 

%list -> '(' atom oper atom ')': mak('$2','$3','$4').

%list -> js oper js: io:format("js oper js~n"), vacca.

%list -> '(' atom ')' : io:format("list -> '(' atom ')': ~p~p~p ~n",['$1','$2','$3']), '$2'.

%elements -> atom oper atom element: mak('$1','$2','$3').

Erlang code.


min({calc, _, V}) -> {calc, V};
min({oper, _, V}) -> {oper, V};
min(V) -> V.

xa({artist, A1},{oper, _, O},{artist, A2}) ->
	{fetch, 1, [A1, O, A2]}.

ax({fetch, _, Artist}) ->
	{artist, Artist}.

mak({fetch,_,Name1}=A1,{oper,_,Op}=O,{fetch,_,Name2}=A2) ->
	R = {fetch, "['" ++ Name1 ++ "','" ++ Name2 ++ "'," ++ Op ++ "]"},
	io:format("[element -> fetch oper fetch ~p~p~p -> ~p ~n", [A1,O,A2,R]),
	R.
	
unwrap({_,_,V}=Any) -> io:format("Unwrap: ~p~n",[Any]), V.
foo({_,_,V}=Any) -> io:format("Foo: ~p~n",[Any]), V.
%simplify({Tag,A,nil}=S) -> io:format("S:~p~n", [S]), A;
%simplify(X) -> io:format("X:~p~n", [X]),X.

%form -> author : io:format("match"), '$1'.
%form -> '-' : io:format("fu~n"), '$1'.
%'-' -> author : io:format("fa~n"), '$1'.
%% form -> author : io:format("xxxx").

%% form -> author	: unwrap('$1').
%% form -> oper	: {author, foo('$1')}.
%% oper ->	author	: {author, foo('$1')}.
%oper -> form : author.
%form -> 'COMPILER' atom			: {compiler, unwrap('$2')}.
%form -> 'CHARACTERS' char_prods    	: {characters, '$2'}.
%form -> 'COMMENTS' 'FROM' string 
%	          'TO' string nested 	: {comments,unwrap('$3'),unwrap('$5'),
%						    '$6'}.
%form -> 'TOKENS' syntax		        : {tokens, '$2'}.
%form -> 'IGNORE' ignore 	        : {ignore, '$2'}.
%form -> 'PRODUCTIONS' syntax 		: {syntax, '$2'}.
%form -> 'END' atom			: {theend, '$2'}.
%form -> comment.

%nested -> 'NESTED'			: nested.
%nested -> 'EOL'                         : eol.
%nested -> '$empty'			: not_nested.

%% Character syntax

%char_prods -> charline ';' char_prods	: ['$1'|'$3'].
%char_prods -> charline			: ['$1'].

%charline -> atom '=' char_rhs           : {unwrap('$1'), '$3'}.

% char_rhs -> char_prim '+' char_rhs	: {plus, '$1', '$3'}.
%% char_rhs -> char_prim '-' char_rhs	: {minus, '$1', '$3'}.
% char_rhs -> char_prim			: '$1'.

% char_prim -> 'CHR' '(' integer ')'	: {chr, unwrap('$3')}.
% char_prim -> string			: {string, unwrap('$1')}.
% char_prim -> quote			: {string, unwrap('$1')}.
% char_prim -> atom			: {atom, unwrap('$1')}.
% char_prim -> 'ANY'			: any.

% ignore -> var moreignore		: [unwrap('$1')|'$2'].

% moreignore -> '+' ignore		: '$2'.
% moreignore -> '$empty'			: [].

%% The following deifinitions are taken from  [WIR82]
%% WIR82 Programming in Modular2
%% Springer Verlag 1982

%% statement	: A syntactic  form
%% expression	: A list of alternatives
%% term		: A concatination of factors
%% factor	: A single syntactoc entity or a parenthesized expression

%% Construct
%% =========
%% [ A ]      = zero or more A's
%% { A }      = any number of A's
%% "A"        = a string parse tree
%% A | B      = A or B parse tree
%% A  B       = sequence of A followed by B
%% identifier = a name

%% syntax     = {production} 
%% production = id "=" expr ";"		
%% expr       = term {"|" term}        
%% term       = factor {factor}        
%% factor     = id | string "{" expr "}

%% syntax -> production ';' syntax 	: ['$1'|'$3'].
%% syntax -> production		  	: ['$1'].

% production -> lhs '=' expr 	 	: {prod, '$1', '$3'}.

% lhs -> var 				: unwrap('$1').
% lhs -> atom 				: unwrap('$1').
% lhs -> author				: foo('$1').
% lhs ->	oper				: foo('$1').

% expr -> author				: '$1'.
% expr ->	oper				: '$1'.

% expr -> term	 			: '$1'.
% expr -> term '|' expr			: {alt, '$1', '$3'}.

% term -> factor  			: '$1'.
% term -> factor term			: {seq, '$1', '$2'}.

% factor -> author			: {nt, unwrap('$1')}.
% factor -> oper				: {ts, foo('$1')}.
%factor -> atom 				: {nt, unwrap('$1')}.
%factor -> var  				: {ta, unwrap('$1')}.
%factor -> string 			: {ts, unwrap('$1')}.
%factor -> quote 			: {tq, unwrap('$1')}.
%factor -> '[' expr ']' 			: {one, '$2'}.
%factor -> '{' expr '}' 			: {star, '$2'}.
%factor -> '(' expr ')' 			: {bracket, '$2'}.


