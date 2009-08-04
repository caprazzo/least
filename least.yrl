Nonterminals list elements element.
Terminals calc fetch artist atom oper '(' ')'.

Rootsymbol list.

% elements -> '(' ')' : io:format("list ( ):~p ~p~n",['$1','$2']), '$2'.

list -> '(' elements ')' : '$2'.
elements -> element : '$1'.
elements -> element element elements : {calc, min('$1'), min('$2'), min('$3')}.
element -> atom : {atom,_,V}='$1', V.
element -> list : '$1'.
element -> oper : '$1'.
element -> artist oper artist : min('$1','$2','$3').
element -> fetch : min('$1').
list -> '(' calc oper calc ')' : {calc, min('$2'), min('$3'), min('$4')}.

Erlang code.

min({calc, _, V}) -> {calc, V};
min({oper, _, V}) -> {oper, V};
min({fetch, _, Artist}) -> {artist, Artist};
min(V) -> V.

min({artist, A1},{oper, _, O},{artist, A2}) -> {fetch, 1, [A1, O, A2]}.


