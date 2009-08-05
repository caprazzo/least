Nonterminals list element.
Terminals calc artist oper '(' ')'.

Rootsymbol list.

list -> '(' calc oper calc ')':
	log("list->(calc oper calc)", ['$2','$3','$4'], {calc, 2, '$2', '$3', '$4'}).
list -> calc oper calc :
	log("list->calc oper calc", ['$1','$2','$3'], {calc, 2, '$1', '$2', '$3'}).
	
element -> oper :
	log("element->oper",'$1','$1').

list -> element :
	log("list->element", '$1', '$1').

element -> calc :
	log("element->calc",'$1','$1').	

list -> '(' list ')' :
	log("list->(list)",['$1','$2','$3'], '$2').

list -> list list list :
	log("list->list list list", ['$1','$2'], {calc, 3, '$1','$2','$3'}).


Erlang code.

%min({calc, _N, V}) -> {calc, _N, V};
%min({oper, _N, V}) -> {oper, _N, V};
min(V) -> V.

log(Desc, In, Out) ->
	io:format("~p ~p -->~n \t ~p~n--------------~n",[Desc, In, Out]),
	Out.

swrap([{calc,_,_}=C1,{oper,_,_}=O,{calc,_,_}=C2]) ->
	{calc, 5, C1, O, C2}.



