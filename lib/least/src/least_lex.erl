-file("c:/PROGRA~1/ERL57~1.2/lib/parsetools-2.0/include/leexinc.hrl", 0).
%% The source of this file is part of leex distribution, as such it
%% has the same Copyright as the other files in the leex
%% distribution. The Copyright is defined in the accompanying file
%% COPYRIGHT. However, the resultant scanner generated by leex is the
%% property of the creator of the scanner and is not covered by that
%% Copyright.

-module(least_lex).

-export([string/1,string/2,token/2,token/3,tokens/2,tokens/3]).
-export([format_error/1]).

%% User code. This is placed here to allow extra attributes.
-file("src/least_lex.xrl", 67).

string_gen([$\\|Cs]) ->
    string_escape(Cs);
string_gen([C|Cs]) ->
    [C|string_gen(Cs)];
string_gen([]) -> [].

string_escape([O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    [(O1*8 + O2)*8 + O3 - 73*$0|string_gen(S)];
string_escape([$^,C|Cs]) ->
    [C band 31|string_gen(Cs)];
string_escape([C|Cs]) when C >= 0, C =< $  ->
    string_gen(Cs);
string_escape([C|Cs]) ->
    [escape_char(C)|string_gen(Cs)].

escape_char($n) -> $\n;				%\n = LF
escape_char($r) -> $\r;				%\r = CR
escape_char($t) -> $\t;				%\t = TAB
escape_char($v) -> $\v;				%\v = VT
escape_char($b) -> $\b;				%\b = BS
escape_char($f) -> $\f;				%\f = FF
escape_char($e) -> $\e;				%\e = ESC
escape_char($s) -> $ ;				%\s = SPC
escape_char($d) -> $\d;				%\d = DEL
escape_char(C) -> C.

remove_brackets([_,_|T]) ->
	[_,_|T1] = lists:reverse(T),
	lists:reverse(T1).

special("COMPILER", Line) -> {'COMPILER', Line};
special("CHARACTERS", Line) -> {'CHARACTERS', Line};
special("COMMENTS", Line) -> {'COMMENTS', Line};
special("FROM", Line) -> {'FROM', Line};
special("TO", Line) -> {'TO', Line};
special("TOKENS", Line) -> {'TOKENS', Line};
special("IGNORE", Line) -> {'IGNORE', Line};
special("PRODUCTIONS", Line) -> {'PRODUCTIONS', Line};
special("END", Line) -> {'END', Line};
special("NESTED", Line) -> {'NESTED', Line};
special("EOL", Line) -> {'EOL', Line};
special("CHR", Line) -> {'CHR', Line};
special("ANY", Line) -> {'ANY', Line};
special(Other, Line) -> {var, Line, Other}.





-file("c:/PROGRA~1/ERL57~1.2/lib/parsetools-2.0/include/leexinc.hrl", 14).

format_error({illegal,S}) -> ["illegal characters ",io_lib:write_string(S)];
format_error({user,S}) -> S.

string(String) -> string(String, 1).

string(String, Line) -> string(String, Line, String, []).

%% string(InChars, Line, TokenChars, Tokens) ->
%% {ok,Tokens,Line} | {error,ErrorInfo,Line}.
%% Note the line number going into yystate, L0, is line of token
%% start while line number returned is line of token end. We want line
%% of token start.

string([], L, [], Ts) ->                     % No partial tokens!
    {ok,yyrev(Ts),L};
string(Ics0, L0, Tcs, Ts) ->
    case yystate(yystate(), Ics0, L0, 0, reject, 0) of
        {A,Alen,Ics1,L1} ->                  % Accepting end state
            string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L0), Ts);
        {A,Alen,Ics1,L1,_S1} ->              % Accepting transistion state
            string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L0), Ts);
        {reject,_Alen,Tlen,_Ics1,L1,_S1} ->  % After a non-accepting state
            {error,{L0,?MODULE,{illegal,yypre(Tcs, Tlen+1)}},L1};
        {A,Alen,_Tlen,_Ics1,L1,_S1} ->
            string_cont(yysuf(Tcs, Alen), L1, yyaction(A, Alen, Tcs, L0), Ts)
    end.

%% string_cont(RestChars, Line, Token, Tokens)
%% Test for and remove the end token wrapper. Push back characters
%% are prepended to RestChars.

string_cont(Rest, Line, {token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, {token,T,Push}, Ts) ->
    NewRest = Push ++ Rest,
    string(NewRest, Line, NewRest, [T|Ts]);
string_cont(Rest, Line, {end_token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, {end_token,T,Push}, Ts) ->
    NewRest = Push ++ Rest,
    string(NewRest, Line, NewRest, [T|Ts]);
string_cont(Rest, Line, skip_token, Ts) ->
    string(Rest, Line, Rest, Ts);
string_cont(Rest, Line, {skip_token,Push}, Ts) ->
    NewRest = Push ++ Rest,
    string(NewRest, Line, NewRest, Ts);
string_cont(_Rest, Line, {error,S}, _Ts) ->
    {error,{Line,?MODULE,{user,S}},Line}.

%% token(Continuation, Chars) ->
%% token(Continuation, Chars, Line) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept. The continuation is:
%% {token,State,CurrLine,TokenChars,TokenLen,TokenLine,AccAction,AccLen}

token(Cont, Chars) -> token(Cont, Chars, 1).

token([], Chars, Line) ->
    token(yystate(), Chars, Line, Chars, 0, Line, reject, 0);
token({token,State,Line,Tcs,Tlen,Tline,Action,Alen}, Chars, _) ->
    token(State, Chars, Line, Tcs ++ Chars, Tlen, Tline, Action, Alen).

%% token(State, InChars, Line, TokenChars, TokenLen, TokenLine,
%% AcceptAction, AcceptLen) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.
%% The argument order is chosen to be more efficient.

token(S0, Ics0, L0, Tcs, Tlen0, Tline, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
        %% Accepting end state, we have a token.
        {A1,Alen1,Ics1,L1} ->
            token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline));
        %% Accepting transition state, can take more chars.
        {A1,Alen1,[],L1,S1} ->                  % Need more chars to check
            {more,{token,S1,L1,Tcs,Alen1,Tline,A1,Alen1}};
        {A1,Alen1,Ics1,L1,_S1} ->               % Take what we got
            token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline));
        %% After a non-accepting state, maybe reach accept state later.
        {A1,Alen1,Tlen1,[],L1,S1} ->            % Need more chars to check
            {more,{token,S1,L1,Tcs,Tlen1,Tline,A1,Alen1}};
        {reject,_Alen1,Tlen1,eof,L1,_S1} ->     % No token match
            %% Check for partial token which is error.
            Ret = if Tlen1 > 0 -> {error,{Tline,?MODULE,
                                          %% Skip eof tail in Tcs.
                                          {illegal,yypre(Tcs, Tlen1)}},L1};
                     true -> {eof,L1}
                  end,
            {done,Ret,eof};
        {reject,_Alen1,Tlen1,Ics1,L1,_S1} ->    % No token match
            Error = {Tline,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}},
            {done,{error,Error,L1},Ics1};
        {A1,Alen1,_Tlen1,_Ics1,L1,_S1} ->       % Use last accept match
            token_cont(yysuf(Tcs, Alen1), L1, yyaction(A1, Alen1, Tcs, Tline))
    end.

%% token_cont(RestChars, Line, Token)
%% If we have a token or error then return done, else if we have a
%% skip_token then continue.

token_cont(Rest, Line, {token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, {token,T,Push}) ->
    NewRest = Push ++ Rest,
    {done,{ok,T,Line},NewRest};
token_cont(Rest, Line, {end_token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, {end_token,T,Push}) ->
    NewRest = Push ++ Rest,
    {done,{ok,T,Line},NewRest};
token_cont(Rest, Line, skip_token) ->
    token(yystate(), Rest, Line, Rest, 0, Line, reject, 0);
token_cont(Rest, Line, {skip_token,Push}) ->
    NewRest = Push ++ Rest,
    token(yystate(), NewRest, Line, NewRest, 0, Line, reject, 0);
token_cont(Rest, Line, {error,S}) ->
    {done,{error,{Line,?MODULE,{user,S}},Line},Rest}.

%% tokens(Continuation, Chars, Line) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.
%% Must be careful when re-entering to append the latest characters to the
%% after characters in an accept. The continuation is:
%% {tokens,State,CurrLine,TokenChars,TokenLen,TokenLine,Tokens,AccAction,AccLen}
%% {skip_tokens,State,CurrLine,TokenChars,TokenLen,TokenLine,Error,AccAction,AccLen}

tokens(Cont, Chars) -> tokens(Cont, Chars, 1).

tokens([], Chars, Line) ->
    tokens(yystate(), Chars, Line, Chars, 0, Line, [], reject, 0);
tokens({tokens,State,Line,Tcs,Tlen,Tline,Ts,Action,Alen}, Chars, _) ->
    tokens(State, Chars, Line, Tcs ++ Chars, Tlen, Tline, Ts, Action, Alen);
tokens({skip_tokens,State,Line,Tcs,Tlen,Tline,Error,Action,Alen}, Chars, _) ->
    skip_tokens(State, Chars, Line, Tcs ++ Chars, Tlen, Tline, Error, Action, Alen).

%% tokens(State, InChars, Line, TokenChars, TokenLen, TokenLine, Tokens,
%% AcceptAction, AcceptLen) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.

tokens(S0, Ics0, L0, Tcs, Tlen0, Tline, Ts, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
        %% Accepting end state, we have a token.
        {A1,Alen1,Ics1,L1} ->
            tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Ts);
        %% Accepting transition state, can take more chars.
        {A1,Alen1,[],L1,S1} ->                  % Need more chars to check
            {more,{tokens,S1,L1,Tcs,Alen1,Tline,Ts,A1,Alen1}};
        {A1,Alen1,Ics1,L1,_S1} ->               % Take what we got
            tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Ts);
        %% After a non-accepting state, maybe reach accept state later.
        {A1,Alen1,Tlen1,[],L1,S1} ->            % Need more chars to check
            {more,{tokens,S1,L1,Tcs,Tlen1,Tline,Ts,A1,Alen1}};
        {reject,_Alen1,Tlen1,eof,L1,_S1} ->     % No token match
            %% Check for partial token which is error, no need to skip here.
            Ret = if Tlen1 > 0 -> {error,{Tline,?MODULE,
                                          %% Skip eof tail in Tcs.
                                          {illegal,yypre(Tcs, Tlen1)}},L1};
                     Ts == [] -> {eof,L1};
                     true -> {ok,yyrev(Ts),L1}
                  end,
            {done,Ret,eof};
        {reject,_Alen1,Tlen1,_Ics1,L1,_S1} ->
            %% Skip rest of tokens.
            Error = {L1,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}},
            skip_tokens(yysuf(Tcs, Tlen1+1), L1, Error);
        {A1,Alen1,_Tlen1,_Ics1,L1,_S1} ->
            Token = yyaction(A1, Alen1, Tcs, Tline),
            tokens_cont(yysuf(Tcs, Alen1), L1, Token, Ts)
    end.

%% tokens_cont(RestChars, Line, Token, Tokens)
%% If we have an end_token or error then return done, else if we have
%% a token then save it and continue, else if we have a skip_token
%% just continue.

tokens_cont(Rest, Line, {token,T}, Ts) ->
    tokens(yystate(), Rest, Line, Rest, 0, Line, [T|Ts], reject, 0);
tokens_cont(Rest, Line, {token,T,Push}, Ts) ->
    NewRest = Push ++ Rest,
    tokens(yystate(), NewRest, Line, NewRest, 0, Line, [T|Ts], reject, 0);
tokens_cont(Rest, Line, {end_token,T}, Ts) ->
    {done,{ok,yyrev(Ts, [T]),Line},Rest};
tokens_cont(Rest, Line, {end_token,T,Push}, Ts) ->
    NewRest = Push ++ Rest,
    {done,{ok,yyrev(Ts, [T]),Line},NewRest};
tokens_cont(Rest, Line, skip_token, Ts) ->
    tokens(yystate(), Rest, Line, Rest, 0, Line, Ts, reject, 0);
tokens_cont(Rest, Line, {skip_token,Push}, Ts) ->
    NewRest = Push ++ Rest,
    tokens(yystate(), NewRest, Line, NewRest, 0, Line, Ts, reject, 0);
tokens_cont(Rest, Line, {error,S}, _Ts) ->
    skip_tokens(Rest, Line, {Line,?MODULE,{user,S}}).

%%skip_tokens(InChars, Line, Error) -> {done,{error,Error,Line},Ics}.
%% Skip tokens until an end token, junk everything and return the error.

skip_tokens(Ics, Line, Error) ->
    skip_tokens(yystate(), Ics, Line, Ics, 0, Line, Error, reject, 0).

%% skip_tokens(State, InChars, Line, TokenChars, TokenLen, TokenLine, Tokens,
%% AcceptAction, AcceptLen) ->
%% {more,Continuation} | {done,ReturnVal,RestChars}.

skip_tokens(S0, Ics0, L0, Tcs, Tlen0, Tline, Error, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
        {A1,Alen1,Ics1,L1} ->                  % Accepting end state
            skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Error);
        {A1,Alen1,[],L1,S1} ->                 % After an accepting state
            {more,{skip_tokens,S1,L1,Tcs,Alen1,Tline,Error,A1,Alen1}};
        {A1,Alen1,Ics1,L1,_S1} ->
            skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Error);
        {A1,Alen1,Tlen1,[],L1,S1} ->           % After a non-accepting state
            {more,{skip_tokens,S1,L1,Tcs,Tlen1,Tline,Error,A1,Alen1}};
        {reject,_Alen1,_Tlen1,eof,L1,_S1} ->
            {done,{error,Error,L1},eof};
        {reject,_Alen1,Tlen1,_Ics1,L1,_S1} ->
            skip_tokens(yysuf(Tcs, Tlen1+1), L1, Error);
        {A1,Alen1,_Tlen1,_Ics1,L1,_S1} ->
            Token = yyaction(A1, Alen1, Tcs, Tline),
            skip_cont(yysuf(Tcs, Alen1), L1, Token, Error)
    end.

%% skip_cont(RestChars, Line, Token, Error)
%% Skip tokens until we have an end_token or error then return done
%% with the original rror.

skip_cont(Rest, Line, {token,_T}, Error) ->
    skip_tokens(yystate(), Rest, Line, Rest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {token,_T,Push}, Error) ->
    NewRest = Push ++ Rest,
    skip_tokens(yystate(), NewRest, Line, NewRest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {end_token,_T}, Error) ->
    {done,{error,Error,Line},Rest};
skip_cont(Rest, Line, {end_token,_T,Push}, Error) ->
    NewRest = Push ++ Rest,
    {done,{error,Error,Line},NewRest};
skip_cont(Rest, Line, skip_token, Error) ->
    skip_tokens(yystate(), Rest, Line, Rest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {skip_token,Push}, Error) ->
    NewRest = Push ++ Rest,
    skip_tokens(yystate(), NewRest, Line, NewRest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {error,_S}, Error) ->
    skip_tokens(yystate(), Rest, Line, Rest, 0, Line, Error, reject, 0).

yyrev(List) -> lists:reverse(List).
yyrev(List, Tail) -> lists:reverse(List, Tail).
yypre(List, N) -> lists:sublist(List, N).
yysuf(List, N) -> lists:nthtail(N, List).

%% yystate() -> InitialState.
%% yystate(State, InChars, Line, CurrTokLen, AcceptAction, AcceptLen) ->
%% {Action, AcceptLen, RestChars, Line} |
%% {Action, AcceptLen, RestChars, Line, State} |
%% {reject, AcceptLen, CurrTokLen, RestChars, Line, State} |
%% {Action, AcceptLen, CurrTokLen, RestChars, Line, State}.
%% Generated state transition functions. The non-accepting end state
%% return signal either an unrecognised character or end of current
%% input.

-file("src/least_lex.erl", 326).
yystate() -> 36.

yystate(39, [42|Ics], Line, Tlen, Action, Alen) ->
    yystate(37, Ics, Line, Tlen+1, Action, Alen);
yystate(39, [41|Ics], Line, Tlen, Action, Alen) ->
    yystate(29, Ics, Line, Tlen+1, Action, Alen);
yystate(39, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(25, Ics, Line+1, Tlen+1, Action, Alen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(25, Ics, Line, Tlen+1, Action, Alen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 40 ->
    yystate(25, Ics, Line, Tlen+1, Action, Alen);
yystate(39, [C|Ics], Line, Tlen, Action, Alen) when C >= 43 ->
    yystate(25, Ics, Line, Tlen+1, Action, Alen);
yystate(39, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,39};
yystate(38, Ics, Line, Tlen, _, _) ->
    {17,Tlen,Ics,Line};
yystate(37, [42|Ics], Line, Tlen, Action, Alen) ->
    yystate(39, Ics, Line, Tlen+1, Action, Alen);
yystate(37, [41|Ics], Line, Tlen, Action, Alen) ->
    yystate(33, Ics, Line, Tlen+1, Action, Alen);
yystate(37, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(13, Ics, Line+1, Tlen+1, Action, Alen);
yystate(37, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(37, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 40 ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(37, [C|Ics], Line, Tlen, Action, Alen) when C >= 43 ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(37, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,37};
yystate(36, [125|Ics], Line, Tlen, Action, Alen) ->
    yystate(32, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [124|Ics], Line, Tlen, Action, Alen) ->
    yystate(28, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [123|Ics], Line, Tlen, Action, Alen) ->
    yystate(24, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [95|Ics], Line, Tlen, Action, Alen) ->
    yystate(38, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [96|Ics], Line, Tlen, Action, Alen) ->
    yystate(38, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [94|Ics], Line, Tlen, Action, Alen) ->
    yystate(19, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [93|Ics], Line, Tlen, Action, Alen) ->
    yystate(20, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [92|Ics], Line, Tlen, Action, Alen) ->
    yystate(38, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [91|Ics], Line, Tlen, Action, Alen) ->
    yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [61|Ics], Line, Tlen, Action, Alen) ->
    yystate(12, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [60|Ics], Line, Tlen, Action, Alen) ->
    yystate(38, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [59|Ics], Line, Tlen, Action, Alen) ->
    yystate(8, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [58|Ics], Line, Tlen, Action, Alen) ->
    yystate(4, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [47|Ics], Line, Tlen, Action, Alen) ->
    yystate(38, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [46|Ics], Line, Tlen, Action, Alen) ->
    yystate(11, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [45|Ics], Line, Tlen, Action, Alen) ->
    yystate(19, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [44|Ics], Line, Tlen, Action, Alen) ->
    yystate(38, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [43|Ics], Line, Tlen, Action, Alen) ->
    yystate(23, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [42|Ics], Line, Tlen, Action, Alen) ->
    yystate(19, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [41|Ics], Line, Tlen, Action, Alen) ->
    yystate(27, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [40|Ics], Line, Tlen, Action, Alen) ->
    yystate(31, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [39|Ics], Line, Tlen, Action, Alen) ->
    yystate(9, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [34|Ics], Line, Tlen, Action, Alen) ->
    yystate(14, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(38, Ics, Line+1, Tlen+1, Action, Alen);
yystate(36, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(38, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 33 ->
    yystate(38, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [C|Ics], Line, Tlen, Action, Alen) when C >= 35, C =< 38 ->
    yystate(38, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [C|Ics], Line, Tlen, Action, Alen) when C >= 48, C =< 57 ->
    yystate(0, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [C|Ics], Line, Tlen, Action, Alen) when C >= 62, C =< 64 ->
    yystate(38, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [C|Ics], Line, Tlen, Action, Alen) when C >= 65, C =< 90 ->
    yystate(0, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [C|Ics], Line, Tlen, Action, Alen) when C >= 97, C =< 122 ->
    yystate(0, Ics, Line, Tlen+1, Action, Alen);
yystate(36, [C|Ics], Line, Tlen, Action, Alen) when C >= 126 ->
    yystate(38, Ics, Line, Tlen+1, Action, Alen);
yystate(36, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,36};
yystate(35, [42|Ics], Line, Tlen, Action, Alen) ->
    yystate(39, Ics, Line, Tlen+1, Action, Alen);
yystate(35, [41|Ics], Line, Tlen, Action, Alen) ->
    yystate(21, Ics, Line, Tlen+1, Action, Alen);
yystate(35, [40|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(35, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(13, Ics, Line+1, Tlen+1, Action, Alen);
yystate(35, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(35, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 40 ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(35, [C|Ics], Line, Tlen, Action, Alen) when C >= 43 ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(35, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,35};
yystate(34, [92|Ics], Line, Tlen, Action, Alen) ->
    yystate(22, Ics, Line, Tlen+1, Action, Alen);
yystate(34, [34|Ics], Line, Tlen, Action, Alen) ->
    yystate(30, Ics, Line, Tlen+1, Action, Alen);
yystate(34, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line+1, Tlen+1, Action, Alen);
yystate(34, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(34, Ics, Line, Tlen+1, Action, Alen);
yystate(34, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 33 ->
    yystate(34, Ics, Line, Tlen+1, Action, Alen);
yystate(34, [C|Ics], Line, Tlen, Action, Alen) when C >= 35, C =< 91 ->
    yystate(34, Ics, Line, Tlen+1, Action, Alen);
yystate(34, [C|Ics], Line, Tlen, Action, Alen) when C >= 93 ->
    yystate(34, Ics, Line, Tlen+1, Action, Alen);
yystate(34, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,34};
yystate(33, [41|Ics], Line, Tlen, _, _) ->
    yystate(25, Ics, Line, Tlen+1, 4, Tlen);
yystate(33, Ics, Line, Tlen, _, _) ->
    {4,Tlen,Ics,Line,33};
yystate(32, Ics, Line, Tlen, _, _) ->
    {9,Tlen,Ics,Line};
yystate(31, [42|Ics], Line, Tlen, _, _) ->
    yystate(35, Ics, Line, Tlen+1, 13, Tlen);
yystate(31, Ics, Line, Tlen, _, _) ->
    {13,Tlen,Ics,Line,31};
yystate(30, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line};
yystate(29, Ics, Line, Tlen, _, _) ->
    {4,Tlen,Ics,Line};
yystate(28, Ics, Line, Tlen, _, _) ->
    {15,Tlen,Ics,Line};
yystate(27, Ics, Line, Tlen, _, _) ->
    {14,Tlen,Ics,Line};
yystate(26, [92|Ics], Line, Tlen, _, _) ->
    yystate(22, Ics, Line, Tlen+1, 2, Tlen);
yystate(26, [34|Ics], Line, Tlen, _, _) ->
    yystate(30, Ics, Line, Tlen+1, 2, Tlen);
yystate(26, [10|Ics], Line, Tlen, _, _) ->
    yystate(34, Ics, Line+1, Tlen+1, 2, Tlen);
yystate(26, [C|Ics], Line, Tlen, _, _) when C >= 0, C =< 9 ->
    yystate(34, Ics, Line, Tlen+1, 2, Tlen);
yystate(26, [C|Ics], Line, Tlen, _, _) when C >= 11, C =< 33 ->
    yystate(34, Ics, Line, Tlen+1, 2, Tlen);
yystate(26, [C|Ics], Line, Tlen, _, _) when C >= 35, C =< 91 ->
    yystate(34, Ics, Line, Tlen+1, 2, Tlen);
yystate(26, [C|Ics], Line, Tlen, _, _) when C >= 93 ->
    yystate(34, Ics, Line, Tlen+1, 2, Tlen);
yystate(26, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,26};
yystate(25, [42|Ics], Line, Tlen, Action, Alen) ->
    yystate(39, Ics, Line, Tlen+1, Action, Alen);
yystate(25, [41|Ics], Line, Tlen, Action, Alen) ->
    yystate(21, Ics, Line, Tlen+1, Action, Alen);
yystate(25, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(13, Ics, Line+1, Tlen+1, Action, Alen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 40 ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(25, [C|Ics], Line, Tlen, Action, Alen) when C >= 43 ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(25, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,25};
yystate(24, Ics, Line, Tlen, _, _) ->
    {10,Tlen,Ics,Line};
yystate(23, Ics, Line, Tlen, _, _) ->
    {6,Tlen,Ics,Line};
yystate(22, [94|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(22, [93|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line, Tlen+1, Action, Alen);
yystate(22, [92|Ics], Line, Tlen, Action, Alen) ->
    yystate(22, Ics, Line, Tlen+1, Action, Alen);
yystate(22, [34|Ics], Line, Tlen, Action, Alen) ->
    yystate(26, Ics, Line, Tlen+1, Action, Alen);
yystate(22, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line+1, Tlen+1, Action, Alen);
yystate(22, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(34, Ics, Line, Tlen+1, Action, Alen);
yystate(22, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 33 ->
    yystate(34, Ics, Line, Tlen+1, Action, Alen);
yystate(22, [C|Ics], Line, Tlen, Action, Alen) when C >= 35, C =< 91 ->
    yystate(34, Ics, Line, Tlen+1, Action, Alen);
yystate(22, [C|Ics], Line, Tlen, Action, Alen) when C >= 95 ->
    yystate(34, Ics, Line, Tlen+1, Action, Alen);
yystate(22, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,22};
yystate(21, [41|Ics], Line, Tlen, Action, Alen) ->
    yystate(25, Ics, Line, Tlen+1, Action, Alen);
yystate(21, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,21};
yystate(20, Ics, Line, Tlen, _, _) ->
    {12,Tlen,Ics,Line};
yystate(19, Ics, Line, Tlen, _, _) ->
    {1,Tlen,Ics,Line};
yystate(18, [92|Ics], Line, Tlen, Action, Alen) ->
    yystate(22, Ics, Line, Tlen+1, Action, Alen);
yystate(18, [34|Ics], Line, Tlen, Action, Alen) ->
    yystate(26, Ics, Line, Tlen+1, Action, Alen);
yystate(18, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(34, Ics, Line+1, Tlen+1, Action, Alen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(34, Ics, Line, Tlen+1, Action, Alen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 33 ->
    yystate(34, Ics, Line, Tlen+1, Action, Alen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= 35, C =< 91 ->
    yystate(34, Ics, Line, Tlen+1, Action, Alen);
yystate(18, [C|Ics], Line, Tlen, Action, Alen) when C >= 93 ->
    yystate(34, Ics, Line, Tlen+1, Action, Alen);
yystate(18, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,18};
yystate(17, [42|Ics], Line, Tlen, Action, Alen) ->
    yystate(39, Ics, Line, Tlen+1, Action, Alen);
yystate(17, [41|Ics], Line, Tlen, Action, Alen) ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(17, [40|Ics], Line, Tlen, Action, Alen) ->
    yystate(17, Ics, Line, Tlen+1, Action, Alen);
yystate(17, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(13, Ics, Line+1, Tlen+1, Action, Alen);
yystate(17, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(17, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 40 ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(17, [C|Ics], Line, Tlen, Action, Alen) when C >= 43 ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(17, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,17};
yystate(16, Ics, Line, Tlen, _, _) ->
    {11,Tlen,Ics,Line};
yystate(15, Ics, Line, Tlen, _, _) ->
    {18,Tlen,Ics,Line};
yystate(14, [92|Ics], Line, Tlen, _, _) ->
    yystate(22, Ics, Line, Tlen+1, 17, Tlen);
yystate(14, [34|Ics], Line, Tlen, _, _) ->
    yystate(30, Ics, Line, Tlen+1, 17, Tlen);
yystate(14, [10|Ics], Line, Tlen, _, _) ->
    yystate(34, Ics, Line+1, Tlen+1, 17, Tlen);
yystate(14, [C|Ics], Line, Tlen, _, _) when C >= 0, C =< 9 ->
    yystate(34, Ics, Line, Tlen+1, 17, Tlen);
yystate(14, [C|Ics], Line, Tlen, _, _) when C >= 11, C =< 33 ->
    yystate(34, Ics, Line, Tlen+1, 17, Tlen);
yystate(14, [C|Ics], Line, Tlen, _, _) when C >= 35, C =< 91 ->
    yystate(34, Ics, Line, Tlen+1, 17, Tlen);
yystate(14, [C|Ics], Line, Tlen, _, _) when C >= 93 ->
    yystate(34, Ics, Line, Tlen+1, 17, Tlen);
yystate(14, Ics, Line, Tlen, _, _) ->
    {17,Tlen,Ics,Line,14};
yystate(13, [42|Ics], Line, Tlen, Action, Alen) ->
    yystate(39, Ics, Line, Tlen+1, Action, Alen);
yystate(13, [41|Ics], Line, Tlen, Action, Alen) ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(13, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(13, Ics, Line+1, Tlen+1, Action, Alen);
yystate(13, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(13, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 40 ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(13, [C|Ics], Line, Tlen, Action, Alen) when C >= 43 ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(13, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,13};
yystate(12, Ics, Line, Tlen, _, _) ->
    {5,Tlen,Ics,Line};
yystate(11, [32|Ics], Line, Tlen, _, _) ->
    yystate(15, Ics, Line, Tlen+1, 17, Tlen);
yystate(11, [9|Ics], Line, Tlen, _, _) ->
    yystate(15, Ics, Line, Tlen+1, 17, Tlen);
yystate(11, [10|Ics], Line, Tlen, _, _) ->
    yystate(15, Ics, Line+1, Tlen+1, 17, Tlen);
yystate(11, Ics, Line, Tlen, _, _) ->
    {17,Tlen,Ics,Line,11};
yystate(10, [92|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(10, [39|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(10, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(10, Ics, Line+1, Tlen+1, Action, Alen);
yystate(10, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(10, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 38 ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(10, [C|Ics], Line, Tlen, Action, Alen) when C >= 40, C =< 91 ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(10, [C|Ics], Line, Tlen, Action, Alen) when C >= 93 ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(10, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,10};
yystate(9, [92|Ics], Line, Tlen, _, _) ->
    yystate(1, Ics, Line, Tlen+1, 17, Tlen);
yystate(9, [39|Ics], Line, Tlen, _, _) ->
    yystate(6, Ics, Line, Tlen+1, 17, Tlen);
yystate(9, [10|Ics], Line, Tlen, _, _) ->
    yystate(10, Ics, Line+1, Tlen+1, 17, Tlen);
yystate(9, [C|Ics], Line, Tlen, _, _) when C >= 0, C =< 9 ->
    yystate(10, Ics, Line, Tlen+1, 17, Tlen);
yystate(9, [C|Ics], Line, Tlen, _, _) when C >= 11, C =< 38 ->
    yystate(10, Ics, Line, Tlen+1, 17, Tlen);
yystate(9, [C|Ics], Line, Tlen, _, _) when C >= 40, C =< 91 ->
    yystate(10, Ics, Line, Tlen+1, 17, Tlen);
yystate(9, [C|Ics], Line, Tlen, _, _) when C >= 93 ->
    yystate(10, Ics, Line, Tlen+1, 17, Tlen);
yystate(9, Ics, Line, Tlen, _, _) ->
    {17,Tlen,Ics,Line,9};
yystate(8, Ics, Line, Tlen, _, _) ->
    {8,Tlen,Ics,Line};
yystate(7, [46|Ics], Line, Tlen, Action, Alen) ->
    yystate(7, Ics, Line, Tlen+1, Action, Alen);
yystate(7, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(7, Ics, Line+1, Tlen+1, Action, Alen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(7, Ics, Line, Tlen+1, Action, Alen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 32 ->
    yystate(7, Ics, Line, Tlen+1, Action, Alen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= 48, C =< 57 ->
    yystate(3, Ics, Line, Tlen+1, Action, Alen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= 65, C =< 90 ->
    yystate(3, Ics, Line, Tlen+1, Action, Alen);
yystate(7, [C|Ics], Line, Tlen, Action, Alen) when C >= 97, C =< 122 ->
    yystate(3, Ics, Line, Tlen+1, Action, Alen);
yystate(7, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,7};
yystate(6, Ics, Line, Tlen, _, _) ->
    {3,Tlen,Ics,Line};
yystate(5, [92|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(5, [39|Ics], Line, Tlen, Action, Alen) ->
    yystate(2, Ics, Line, Tlen+1, Action, Alen);
yystate(5, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(10, Ics, Line+1, Tlen+1, Action, Alen);
yystate(5, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(5, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 38 ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(5, [C|Ics], Line, Tlen, Action, Alen) when C >= 40, C =< 91 ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(5, [C|Ics], Line, Tlen, Action, Alen) when C >= 93 ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(5, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,5};
yystate(4, Ics, Line, Tlen, _, _) ->
    {16,Tlen,Ics,Line};
yystate(3, [46|Ics], Line, Tlen, _, _) ->
    yystate(7, Ics, Line, Tlen+1, 0, Tlen);
yystate(3, [10|Ics], Line, Tlen, _, _) ->
    yystate(7, Ics, Line+1, Tlen+1, 0, Tlen);
yystate(3, [C|Ics], Line, Tlen, _, _) when C >= 0, C =< 9 ->
    yystate(7, Ics, Line, Tlen+1, 0, Tlen);
yystate(3, [C|Ics], Line, Tlen, _, _) when C >= 11, C =< 32 ->
    yystate(7, Ics, Line, Tlen+1, 0, Tlen);
yystate(3, [C|Ics], Line, Tlen, _, _) when C >= 48, C =< 57 ->
    yystate(3, Ics, Line, Tlen+1, 0, Tlen);
yystate(3, [C|Ics], Line, Tlen, _, _) when C >= 65, C =< 90 ->
    yystate(3, Ics, Line, Tlen+1, 0, Tlen);
yystate(3, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 122 ->
    yystate(3, Ics, Line, Tlen+1, 0, Tlen);
yystate(3, Ics, Line, Tlen, _, _) ->
    {0,Tlen,Ics,Line,3};
yystate(2, [92|Ics], Line, Tlen, _, _) ->
    yystate(1, Ics, Line, Tlen+1, 3, Tlen);
yystate(2, [39|Ics], Line, Tlen, _, _) ->
    yystate(6, Ics, Line, Tlen+1, 3, Tlen);
yystate(2, [10|Ics], Line, Tlen, _, _) ->
    yystate(10, Ics, Line+1, Tlen+1, 3, Tlen);
yystate(2, [C|Ics], Line, Tlen, _, _) when C >= 0, C =< 9 ->
    yystate(10, Ics, Line, Tlen+1, 3, Tlen);
yystate(2, [C|Ics], Line, Tlen, _, _) when C >= 11, C =< 38 ->
    yystate(10, Ics, Line, Tlen+1, 3, Tlen);
yystate(2, [C|Ics], Line, Tlen, _, _) when C >= 40, C =< 91 ->
    yystate(10, Ics, Line, Tlen+1, 3, Tlen);
yystate(2, [C|Ics], Line, Tlen, _, _) when C >= 93 ->
    yystate(10, Ics, Line, Tlen+1, 3, Tlen);
yystate(2, Ics, Line, Tlen, _, _) ->
    {3,Tlen,Ics,Line,2};
yystate(1, [94|Ics], Line, Tlen, Action, Alen) ->
    yystate(5, Ics, Line, Tlen+1, Action, Alen);
yystate(1, [93|Ics], Line, Tlen, Action, Alen) ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(1, [92|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(1, [39|Ics], Line, Tlen, Action, Alen) ->
    yystate(2, Ics, Line, Tlen+1, Action, Alen);
yystate(1, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(10, Ics, Line+1, Tlen+1, Action, Alen);
yystate(1, [C|Ics], Line, Tlen, Action, Alen) when C >= 0, C =< 9 ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(1, [C|Ics], Line, Tlen, Action, Alen) when C >= 11, C =< 38 ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(1, [C|Ics], Line, Tlen, Action, Alen) when C >= 40, C =< 91 ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(1, [C|Ics], Line, Tlen, Action, Alen) when C >= 95 ->
    yystate(10, Ics, Line, Tlen+1, Action, Alen);
yystate(1, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,1};
yystate(0, [46|Ics], Line, Tlen, _, _) ->
    yystate(7, Ics, Line, Tlen+1, 17, Tlen);
yystate(0, [10|Ics], Line, Tlen, _, _) ->
    yystate(7, Ics, Line+1, Tlen+1, 17, Tlen);
yystate(0, [C|Ics], Line, Tlen, _, _) when C >= 0, C =< 9 ->
    yystate(7, Ics, Line, Tlen+1, 17, Tlen);
yystate(0, [C|Ics], Line, Tlen, _, _) when C >= 11, C =< 32 ->
    yystate(7, Ics, Line, Tlen+1, 17, Tlen);
yystate(0, [C|Ics], Line, Tlen, _, _) when C >= 48, C =< 57 ->
    yystate(3, Ics, Line, Tlen+1, 17, Tlen);
yystate(0, [C|Ics], Line, Tlen, _, _) when C >= 65, C =< 90 ->
    yystate(3, Ics, Line, Tlen+1, 17, Tlen);
yystate(0, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 122 ->
    yystate(3, Ics, Line, Tlen+1, 17, Tlen);
yystate(0, Ics, Line, Tlen, _, _) ->
    {17,Tlen,Ics,Line,0};
yystate(S, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,S}.

%% yyaction(Action, TokenLength, TokenChars, TokenLine) ->
%% {token,Token} | {end_token, Token} | skip_token | {error,String}.
%% Generated action function.

yyaction(0, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yy_0_(TokenChars, TokenLine);
yyaction(1, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yy_1_(TokenChars, TokenLine);
yyaction(2, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yy_2_(TokenChars, TokenLine);
yyaction(3, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yy_3_(TokenChars, TokenLine);
yyaction(4, _, _, _) -> skip_token;
yyaction(5, _, _, TokenLine) ->
    yy_5_(TokenLine);
yyaction(6, _, _, TokenLine) ->
    yy_6_(TokenLine);
yyaction(7, _, _, TokenLine) ->
    yy_7_(TokenLine);
yyaction(8, _, _, TokenLine) ->
    yy_8_(TokenLine);
yyaction(9, _, _, TokenLine) ->
    yy_9_(TokenLine);
yyaction(10, _, _, TokenLine) ->
    yy_10_(TokenLine);
yyaction(11, _, _, TokenLine) ->
    yy_11_(TokenLine);
yyaction(12, _, _, TokenLine) ->
    yy_12_(TokenLine);
yyaction(13, _, _, TokenLine) ->
    yy_13_(TokenLine);
yyaction(14, _, _, TokenLine) ->
    yy_14_(TokenLine);
yyaction(15, _, _, TokenLine) ->
    yy_15_(TokenLine);
yyaction(16, _, _, TokenLine) ->
    yy_16_(TokenLine);
yyaction(17, _, _, _) ->
    yy_17_();
yyaction(18, _, _, TokenLine) ->
    yy_18_(TokenLine);
yyaction(_, _, _, _) -> error.

-compile({inline,yy_0_/2}).
-file("src/least_lex.xrl", 15).
yy_0_(TokenChars, TokenLine) ->
     { token , { calc , TokenLine , TokenChars } } .

-compile({inline,yy_1_/2}).
-file("src/least_lex.xrl", 16).
yy_1_(TokenChars, TokenLine) ->
     { token , { oper , TokenLine , TokenChars } } .

-compile({inline,yy_2_/2}).
-file("src/least_lex.xrl", 21).
yy_2_(TokenChars, TokenLine) ->
     S = lists : sublist ( TokenChars , 2 , length ( TokenChars ) - 2 ) ,
     { token , { calc , TokenLine , string_gen ( S ) } } .

-compile({inline,yy_3_/2}).
-file("src/least_lex.xrl", 25).
yy_3_(TokenChars, TokenLine) ->
     S = lists : sublist ( TokenChars , 2 , length ( TokenChars ) - 2 ) ,
     { token , { calc , TokenLine , string_gen ( S ) } } .

-compile({inline,yy_5_/1}).
-file("src/least_lex.xrl", 48).
yy_5_(TokenLine) ->
     { token , { '=' , TokenLine } } .

-compile({inline,yy_6_/1}).
-file("src/least_lex.xrl", 49).
yy_6_(TokenLine) ->
     { token , { '+' , TokenLine } } .

-compile({inline,yy_7_/1}).
-file("src/least_lex.xrl", 50).
yy_7_(TokenLine) ->
     { token , { '-' , TokenLine } } .

-compile({inline,yy_8_/1}).
-file("src/least_lex.xrl", 51).
yy_8_(TokenLine) ->
     { token , { ';' , TokenLine } } .

-compile({inline,yy_9_/1}).
-file("src/least_lex.xrl", 52).
yy_9_(TokenLine) ->
     { token , { '}' , TokenLine } } .

-compile({inline,yy_10_/1}).
-file("src/least_lex.xrl", 53).
yy_10_(TokenLine) ->
     { token , { '{' , TokenLine } } .

-compile({inline,yy_11_/1}).
-file("src/least_lex.xrl", 54).
yy_11_(TokenLine) ->
     { token , { '[' , TokenLine } } .

-compile({inline,yy_12_/1}).
-file("src/least_lex.xrl", 55).
yy_12_(TokenLine) ->
     { token , { ']' , TokenLine } } .

-compile({inline,yy_13_/1}).
-file("src/least_lex.xrl", 56).
yy_13_(TokenLine) ->
     { token , { '(' , TokenLine } } .

-compile({inline,yy_14_/1}).
-file("src/least_lex.xrl", 57).
yy_14_(TokenLine) ->
     { token , { ')' , TokenLine } } .

-compile({inline,yy_15_/1}).
-file("src/least_lex.xrl", 58).
yy_15_(TokenLine) ->
     { token , { '|' , TokenLine } } .

-compile({inline,yy_16_/1}).
-file("src/least_lex.xrl", 59).
yy_16_(TokenLine) ->
     { token , { ':' , TokenLine } } .

-compile({inline,yy_17_/0}).
-file("src/least_lex.xrl", 61).
yy_17_() ->
     skip_token .

-compile({inline,yy_18_/1}).
-file("src/least_lex.xrl", 63).
yy_18_(TokenLine) ->
     { end_token , { '$end' , TokenLine } } .

-file("c:/PROGRA~1/ERL57~1.2/lib/parsetools-2.0/include/leexinc.hrl", 282).
