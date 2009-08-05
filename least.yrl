Nonterminals list element.
Terminals calc artist oper '(' ')'.

Rootsymbol list.

list -> '(' calc oper calc ')':
	log("list->(calc oper calc)", ['$2','$3','$4'], {calc, min('$2'), min('$4'), min('$3')}).
list -> calc oper calc :
	log("list->calc oper calc", ['$1','$2','$3'], {calc, min('$1'), min('$3'), min('$2')}).
	
element -> oper :
	log("element->oper",'$1',min('$1')).

list -> element :
	log("list->element", '$1', min('$1')).

element -> calc :
	log("element->calc",'$1',min('$1')).	

list -> '(' list ')' :
	log("list->(list)",['$1','$2','$3'], '$2').

list -> list list list :
	log("list->list list list", ['$1','$2'], {calc, '$1','$3','$2'}).


Erlang code.

min({calc, _N, V}) -> {calc, V};
min({oper, _N, V}) -> {oper, V};
min(V) -> V.

log(Desc, In, Out) ->
	io:format("~p ~p -->~n \t ~p~n--------------~n",[Desc, In, Out]),
	Out.

swrap([{calc,_,_}=C1,{oper,_,_}=O,{calc,_,_}=C2]) ->
	{calc, 5, C1, O, C2}.



