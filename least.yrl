Nonterminals list elements element.
Terminals calc artist oper '(' ')'.

Rootsymbol list.

list -> '(' elements ')' : '$2'.
elements -> element : '$1'.
elements -> element element elements : {calc, min('$1'), min('$2'), min('$3')}.
element -> list : '$1'.
element -> oper : '$1'.

list -> '(' calc oper calc ')' : {calc, min('$2'), min('$3'), min('$4')}.

Erlang code.

min({calc, _, V}) -> {calc, V};
min({oper, _, V}) -> {oper, V};
min(V) -> V.



