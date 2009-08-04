-module(least_yecc).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("least.yrl", 75).


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



-file("c:/PROGRA~1/ERL57~1.2/lib/parsetools-2.0/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type(yecc_ret() :: {'error', _} | {'ok', _}).

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec(parse_and_scan/1 ::
      ({function() | {atom(), atom()}, [_]} | {atom(), atom(), [_]}) ->
            yecc_ret()).
parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{{M, F}, A}, no_line}, 0, [], []).

-spec(format_error/1 :: (any()) -> [char() | list()]).
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser
% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec(return_error/2 :: (integer(), any()) -> no_return()).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                {syntax_error, Token} ->
                    yeccerror(Token);
                {missing_in_goto_table=Tag, Symbol, State} ->
                    Desc = {Symbol, State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,[State,_,_,_,Token,_,_]} | _]) ->
    case atom_to_list(F) of
        "yeccpars2" ++ _ ->
            {syntax_error, Token};
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            {missing_in_goto_table, Symbol, State}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
	    yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try
        {text, Str} = erl_scan:token_info(Token, text),
        {line, Line} = erl_scan:token_info(Token, line),
        Parts = re:split(Str, "\n"),
        Dline = length(Parts) - 1,
        Yline = Line + Dline,
        case erl_scan:token_info(Token, column) of
            {column, Column} ->
                Col = byte_size(lists:last(Parts)),
                {Yline, Col + if Dline =:= 0 -> Column; true -> 1 end};
            undefined ->
                Yline
        end
    catch _:_ ->
        yecctoken_location(Token)
    end.

yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

yecctoken_to_string(Token) ->
    case catch erl_scan:token_info(Token, text) of
        {text, Txt} -> Txt;
        _ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    case catch erl_scan:token_info(Token, location) of
        {location, Loc} -> Loc;
        _ -> element(2, Token)
    end.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_unicode_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:write(Val);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write(Other);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("least_yecc.erl", 309).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.3",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 2, Ss, Stack, T, Ts, Tzr).

yeccpars2_1(_S, '$end', _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)}.

yeccpars2_2(S, calc, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_4(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr).

yeccpars2_5(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 2, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, artist, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, fetch, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, oper, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_elements(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_6(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, calc, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_7(S, oper, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccgoto_element(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_9(S, oper, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccgoto_element(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_12(S, calc, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr).

yeccpars2_13(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_14_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_15(S, artist, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_16_(Stack),
 yeccgoto_element(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_17_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_18(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, artist, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, fetch, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, oper, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_19_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_20_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_element(2, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(5, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_elements(2, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_list(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(6=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_8_/1}).
-file("least.yrl", 9).
yeccpars2_8_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , _ , V } = __1 , V
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-file("least.yrl", 14).
yeccpars2_10_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ax ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_14_/1}).
-file("least.yrl", 16).
yeccpars2_14_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { calc , min ( __2 ) , min ( __3 ) , min ( __4 ) }
  end | __Stack].

-compile({inline,yeccpars2_16_/1}).
-file("least.yrl", 13).
yeccpars2_16_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   xa ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_17_/1}).
-file("least.yrl", 2).
yeccpars2_17_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   io : format ( "list ( ):~p ~p~n" , [ __1 , __2 ] ) , __2
  end | __Stack].

-compile({inline,yeccpars2_19_/1}).
-file("least.yrl", 6).
yeccpars2_19_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { calc , min ( __1 ) , min ( __2 ) , min ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_20_/1}).
-file("least.yrl", 4).
yeccpars2_20_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].


-file("least.yrl", 198).
