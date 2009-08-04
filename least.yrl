Nonterminals list element.
Terminals calc artist oper '(' ')'.

Rootsymbol list.

%list -> '(' elements ')' : '$2'.
%elements -> element : '$1'.
%elements -> element element elements : {calc, 2, {min('$1'), min('$2'), min('$3')}}.
%element -> list : '$1'.
%element -> oper : '$1'.

list -> '(' list ')' :
	log("list->(list)",['$1','$2','$3'], {calc, 5, '$2'}).

list -> element :
	log("list->element", ['$1'], '$1').

list -> element list:
	log("list->element list", ['$1','$2'], ['$1'|'$2']).

%list -> element element list:
%	log("list->element list", ['$1','$2','$3'],['$1','$2','$3']).

list -> list list :
	log("list->list list", ['$1','$2'], ['$1','$2']).

element -> calc : '$1'.
element -> oper : '$1'.

%list -> '(' element element element ')' :
%	R = {calc, 2, min('$2'), min('$3'), min('$4')},
%	log("list->(c o c)",['$1','$2','$3','$4','$5'], R),
%	R.

%list -> calc oper calc :
%	R = {calc, 2, min('$1'), min('$2'), min('$3')},
%	log("list->c o c", ['$1','$2','$3'], R),
%	R.


Erlang code.

%min({calc, _N, V}) -> {calc, _N, V};
%min({oper, _N, V}) -> {oper, _N, V};
min(V) -> V.

log(Desc, In, Out) ->
	io:format("~p ~p -->~n \t ~p~n--------------~n",[Desc, In, Out]),
	Out.

swrap([{calc,_,_}=C1,{oper,_,_}=O,{calc,_,_}=C2]) ->
	{calc, 1, C1, O, C2}.



