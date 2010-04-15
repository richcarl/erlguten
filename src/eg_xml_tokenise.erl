%%==========================================================================
%% Copyright (C) 2003 Joe Armstrong
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% Author: Joe Armstrong <joe@sics.se>
%% Purpose: XML tokeniser
%%==========================================================================

-module(eg_xml_tokenise).


%% get_next_token(Str, Line) -> {done, Token, Str1, Line1}
%%                |  {error, Line, Why}
%%                |  {more, Cont}
%% continue(Cont, Str) -> ..

%%   Still not perfect
%%   1)  dump_token not correct
%%      (should be token2string)

%% 1) No DOCTYPE parsing
%% 2) Input form is not assumed complete
%% 3) Line number is reported on first error
%% 4) Abort at first error
%% 5) Recognised entities are expanded
%% 6) CDATA is recognised are returned and parsed as raw
%% 7) Only ISO Latin 1 is recognised
%% 8) Unrecognised Enties are left unchanged


-export([dump_token/2, 
	 get_next_token/2, 
	 continue/2, 
	 test/0
	]).


-define(in(X,Low,Hi), Low =< X, X =< Hi).
-define(SPACE,32).
-define(NEWLINE,$\n).
-define(TAB,$\t).
-define(RETURN,$\r).

-define( white(X), X == ?SPACE ; X == ?NEWLINE ; X == ?TAB ; X == ?RETURN ).

%% ============================================================================

get_next_token(Str, Ln) ->
    catch get_next_token1(Str, Ln).

continue(Cont, Str) ->
    catch Cont(Str).

%% test cases

test() ->
    lists:foreach(fun(I) -> reent_test(I) end, ok_tags()),
    lists:foreach(fun(I) ->
			  case get_next_token(I, 1) of
			      {error, Ln, Term} ->
				  io:format("~s fails:~p good~n",[I,{Ln,Term}]),
				  ok;
			      Other ->
				  io:format("Should fail!:~s ~p~n",
					    [I, Other])
			  end
		  end, bad_tags()).

ok_tags() ->
    ["<![CDATA[123]]>aaa",
     "<!--hi-->aaa",
     "<a>","<abc>","<a a='1' b='2'>",
     "<aa aa=\"val\">","asdasd<aaa", "</aaa>", "<aaa/>", "<aa abc='123'/>"].

bad_tags() ->
    ["<aa\\>","< as>","<asb a>", "<aaa a=>"].

%%----------------------------------------------------------------------
%% Implementation

get_next_token1("<" ++ T, Ln) -> get_next_token2(T, Ln);
get_next_token1(S, Ln)        -> get_raw(S, Ln).

get_next_token2("?" ++ T, Ln) -> get_pi(T, [], Ln);
get_next_token2("!" ++ T, Ln) -> gab(T, [], Ln);
get_next_token2([], Ln)       -> {more, fun(S) -> get_next_token2(S, Ln) end};
get_next_token2(X, Ln)        -> get_tag(X, Ln).

%% <!DOCTYPE ... > Skip nested < > and skip over " "
%%                 and '..'
%% <!--      .. -->
%% <![CDATA[ .. ]]>


%% This code fails *immediately*
%% and is reentrant ...
%% gab= get after bang i.e. after <! ... :-)

%% DOCTYPE
gab("E"++T, "PYTCOD", Ln)  -> get_doctype(T, Ln);
gab("P"++T, S="YTCOD", Ln) -> gab(T,[$P|S], Ln);
gab("Y"++T, S="TCOD", Ln)  -> gab(T,[$Y|S], Ln);
gab("T"++T, S="COD", Ln)   -> gab(T,[$T|S], Ln);
gab("C"++T, S="OD", Ln)    -> gab(T,[$C|S], Ln);
gab("O"++T, S="D", Ln)     -> gab(T,[$O|S], Ln);
gab("D"++T, "", Ln)        -> gab(T,"D", Ln);
%% [CDATA[
gab("["++T, "ATADC[", Ln)  -> get_cdata(T, [], Ln);
gab("A"++T, S="TADC[", Ln) -> gab(T,[$A|S], Ln);
gab("T"++T, S="ADC[", Ln)  -> gab(T,[$T|S], Ln);
gab("A"++T, S="DC[", Ln)   -> gab(T,[$A|S], Ln);
gab("D"++T, S="C[", Ln)    -> gab(T,[$D|S], Ln);
gab("C"++T, S="[", Ln)     -> gab(T,[$C|S], Ln);
gab("["++T, "", Ln)        -> gab(T,"[", Ln);
%% <!--
gab("-"++T, "-", Ln)       -> get_comment(T, [], Ln);
gab("-"++T, "", Ln)        -> gab(T, "-", Ln);
gab([], L, Ln)             -> {more, fun(Str) -> gab(Str, L, Ln) end};
gab(Str, _, Ln) ->
    {error, {line,Ln,"expecting comment, DOCTYPE of CDATA"}}.
    
%%----------------------------------------------------------------------
%% PI's cannot contain >? (even inside quotes)

get_pi(">"++T, "?"++L, Ln) -> {done, {pi, Ln, lists:reverse(L)}, T, Ln};
get_pi([$\n|T], L, Ln)     -> get_pi(T, [$\n|L], Ln+1);
get_pi([H|T], L, Ln)       -> get_pi(T, [H|L], Ln);
get_pi([], L, Ln)          -> {more, fun(S) -> get_pi(S, L, Ln) end}.
    
%%----------------------------------------------------------------------
%% Comments 

get_comment(">"++T, "--"++L,Ln) -> {done, {comment, Ln, lists:reverse(L)}, 
				    T, Ln};
get_comment([$\n|T], L, Ln)     -> get_comment(T, [$\n|L], Ln+1);
get_comment([H|T], L, Ln)       -> get_comment(T, [H|L], Ln);
get_comment([], L, Ln)          -> {more, fun(S) -> get_comment(S, L, Ln) end}.
    
%%----------------------------------------------------------------------
%% CDATA 

get_cdata(">" ++ T, "]]"++L, Ln) -> {done, {cdata, Ln,lists:reverse(L)}, T, Ln};
get_cdata([$\n|T], L, Ln)        -> get_cdata(T, [$\n|L], Ln+1);
get_cdata([H|T], L, Ln)          -> get_cdata(T, [H|L], Ln);
get_cdata([], L, Ln)             -> {more, fun(S) -> get_cdata(S, L, Ln) end}.

%%----------------------------------------------------------------------
%% DOICTYPW
%%   To fetch a doctype we skip " " (always)
%%   and nest naked "<" ">" 's

get_doctype(Str, Ln) ->
    get_doctype(Str, [], 0, Ln).

get_doctype([$"|T], L, Level, Ln) ->
    get_q_doctype($", T, [$"|L], Level, Ln);
get_doctype([$'|T], L, Level, Ln) ->
    get_q_doctype($', T, [$'|L], Level, Ln);
get_doctype([$>|T], L, 0, Ln) ->
    {done, {doctype, Ln, lists:reverse(L)}, T, Ln};
get_doctype([$>|T], L, Level, Ln) ->
    get_doctype(T, [$>|L], Level-1, Ln);
get_doctype([$<|T], L, Level, Ln) ->
    get_doctype(T, [$<|L], Level+1, Ln);
get_doctype([$\n|T], L, Level, Ln) ->
    get_doctype(T, [$\n|L], Level, Ln+1);
get_doctype([H|T], L, Level, Ln) ->
    get_doctype(T, [H|L], Level, Ln);
get_doctype([], L, Level, Ln) ->
    {more, fun(Str) -> get_doctype(Str, L, Level, Ln) end}.

get_q_doctype(S, [S|T], L, Level, Ln) ->
    get_doctype(T, [S|L], Level, Ln);
get_q_doctype(S, [$\n|T], L, Level, Ln) ->
    get_q_doctype(S, T, [$\n|L], Level, Ln+1);
get_q_doctype(S, [H|T], L, Level, Ln) ->
    get_q_doctype(S, T, [H|L], Level, Ln);
get_q_doctype(S, [], L, Level, Ln) ->
    {more, fun(Str) -> get_q_doctype(S, Str, L, Level, Ln) end}. 

%%----------------------------------------------------------------------
%% tag

get_tag(S, Ln) ->
    %% check we have enought data to parse the tag
    case is_tag_complete(S) of
	true  -> parse_tag(S, Ln);
	false -> {more, fun(Str) -> get_tag(S ++ Str, Ln) end}
    end.

%%----------------------------------------------------------------------
%% is_tag_complete (skips quotes) returns on first naked >
%%   checks to see if the entire tag is available

is_tag_complete([$"|T]) -> is_tag_complete($", T);
is_tag_complete([$'|T]) -> is_tag_complete($', T);
is_tag_complete([$>|_]) -> true;
is_tag_complete([_|T])  -> is_tag_complete(T);
is_tag_complete([])     -> false.
    
is_tag_complete(Stop, [Stop|T]) -> is_tag_complete(T);
is_tag_complete(Stop, [_|T])    -> is_tag_complete(Stop, T);
is_tag_complete(Stop, [])       -> false.

%%----------------------------------------------------------------------

%% Cut from:  REC-xml-19980210

%% [39] element      ::= EmptyElemTag | STag content ETag
%% [40] STag         ::= '<' Name (S Attribute)* S? '>'
%  [42] ETag         ::= '</' Name S? '>'
%% [4]  NameChar     ::= Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar
%%                     | Extender
%% [5]  Name         ::= (Letter | '_' | ':') (NameChar)*
%% [41] Attribute    ::= Name Eq AttValue
%% [25] Eq           ::= S? '=' S?
%% [10] AttValue     ::= '"' ([^<&"] | Reference)* '"'
%%                     |  "'" ([^<&'] | Reference)* "'"
%% [44] EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'

%% reent_test breaks the input string at all possible places
%% and check that the parser correctly re-enters.


%%----------------------------------------------------------------------
%% in parse tag we know that we have enought characters for a complete
%% parse so the code does not have to be re-entrant

parse_tag("/" ++ S, Ln) ->
    {Name, S1} = get_Name(S, []),
    {S2, Ln2}  = skip_white(S1, Ln),
    case S2 of
	">" ++ S3->
	    {done, {eTag, Ln, Name}, S3, Ln2};
	_ ->
	    error(Ln, "expecting >")
    end;
parse_tag(S, Ln) ->
    {Name, S1}      = get_Name(S, Ln),
    {Args, S2, Ln1} = get_args(S1, [], Ln),
    case S2 of
	"/>" ++ S3 ->
	    {done, {empty, Ln, Name, Args}, S3, Ln1};
	">" ++ S3 ->
	    {done, {sTag, Ln, Name, Args}, S3, Ln1};
	_ ->
	    error(Ln, "expecting /> or >")
    end.

%% start of name
get_Name([H|T], Ln) ->
    case is_Name_start(H) of
	true  -> get_Name(T, [H], Ln);
	false -> error(Ln, "Tag character:" ++ [H])
    end.

%% We are inside the Name
get_Name(S=[H|T], L, Ln) ->
    case is_NameChar(H) of
	true  -> get_Name(T, [H|L], Ln);
	false -> {lists:reverse(L), S}
    end.

%% [40] STag         ::= '<' Name (S Attribute)* S? '>'
%% [41] Attribute    ::= Name Eq AttValue
%% [25] Eq           ::= S? '=' S?


get_args(S, L, Ln) ->
    {S1, Ln1} = skip_white(S, Ln),
    case S1 of
	[$/|_] ->
	    {lists:reverse(L), S1, Ln1};
	[$>|_] ->
	    {lists:reverse(L), S1, Ln1};
	_ ->
	    {Name, S2} = get_Name(S1, Ln1),
	    {S3, Ln3}  = skip_white(S2, Ln1),
	    case S3 of
		"=" ++ S4 ->
		    {S5, Ln5} = skip_white(S4, Ln3),
		    {Val, S6, Ln6} = get_AttVal(S5, Ln5),
		    get_args(S6, [{Name,Val}|L], Ln6);
		_ ->
		    error(Ln3, "expecting =")
	    end
    end.

get_AttVal([$"|T], Ln) -> collect_AttVal($", T, [], Ln);
get_AttVal([$'|T], Ln) -> collect_AttVal($', T, [], Ln);
get_AttVal([H|T], Ln) ->  error(Ln, "expecting ' or \"").

collect_AttVal(S, [S|T], L, Ln)   -> {lists:reverse(L), T, Ln};
collect_AttVal(S, [$\n|T], L, Ln) -> collect_AttVal(S, T, [$\n|L], Ln+1);
collect_AttVal(S, [$r,$\n|T], L, Ln) -> collect_AttVal(S, T, [$\n|L], Ln+1);
collect_AttVal(S, [$\r|T], L, Ln)  -> collect_AttVal(S, T, [$\n|L], Ln+1);
collect_AttVal(S, [H|T], L, Ln)   -> collect_AttVal(S, T, [H|L], Ln);
collect_AttVal(S, [], L, Ln)      -> error(Ln, "eof in attribute value").

skip_white([$\s|T], Ln)  -> skip_white(T, Ln);
skip_white([$\t|T], Ln)  -> skip_white(T, Ln);
skip_white([$\n|T], Ln)  -> skip_white(T, Ln+1);
skip_white([$\r|T], Ln)  -> skip_white(T, Ln+1);
skip_white(Str, Ln)      -> {Str, Ln}.


is_Name_start($_) -> true;
is_Name_start($:) -> true;
is_Name_start(X) -> is_Letter(X).
    
is_Letter(X) when ?in(X, $a, $z) -> true;
is_Letter(X) when ?in(X, $A, $Z) -> true;
is_Letter(_)                     -> false.

is_Digit(X)  when ?in(X, $0, $9) -> true;
is_Digit(_)                      -> false.
 
is_MiscName($.) -> true;
is_MiscName($-) -> true;
is_MiscName($_) -> true;
is_MiscName($:) -> true;
is_MiscName(_)  -> false.
 
is_NameChar(X) ->
    case is_Letter(X) of
        true  -> true;
        false -> case is_Digit(X) of
                     true  -> true;
                     false -> is_MiscName(X)
                 end
    end.                            

get_raw(Str, Ln) -> get_raw(Str, [], Ln).

get_raw([$<|T], L, Ln)  -> 
    {done, {raw,Ln-count_nls(L,0),lists:reverse(L)}, [$<|T], Ln};
get_raw([$\n|T], L, Ln) -> get_raw(T, [$\n|L], Ln+1);
get_raw([$\r,$\n|T], L, Ln) -> get_raw(T, [$\n|L], Ln+1);
get_raw([$\r|T], L, Ln)  -> get_raw(T, [$\n|L], Ln+1);
get_raw([$&|T], L, Ln)  -> get_amp(T, [], L, Ln);
get_raw([H|T], L, Ln)   -> get_raw(T, [H|L], Ln);
get_raw([], L, Ln)      -> {more, fun(Str) -> get_raw(Str, L, Ln) end}.

count_nls([$\n|T], N) -> count_nls(T, N+1);
count_nls([_|T], N)   -> count_nls(T, N);
count_nls([], N)      -> N.

get_amp([$;|T], Amp, L, Ln) -> 
    Amp1 = lists:reverse(Amp),
    case translate_amp(Amp1) of
	error ->
	    Ent = "&" ++ Amp1 ++ ";",
	    get_raw(T, lists:reverse(Ent, L), Ln);
	Code ->
	    get_raw(T, [Code|L], Ln)
    end;
get_amp([H|T], Amp, L, Ln1) when ?in(H, $a, $z) -> 
    get_amp(T, [H|Amp], L, Ln1);
get_amp([H|T], Amp, L, Ln1) when ?in(H, $A, $Z) -> 
    get_amp(T, [H|Amp], L, Ln1);
get_amp([H|T], Amp, L, Ln) ->
    error(Ln, "Error in entity:");
get_amp([], Amp, L, Ln) ->
    {more, fun(S) -> get_amp(S, Amp, L, Ln) end}.

%%-type translate_amp(string()) -> int() | error.

translate_amp([$# | Ds]) ->
    amp_digits(Ds, 0);
translate_amp(Name) ->
    case Name of
	"lt" -> $<;
	"gt" -> $>;
	"amp" -> $&;
	"quot" -> $";
	"nbsp" -> 160;
	"iexcl" -> 161;
	"cent" -> 162;
	"pound" -> 163;
	"curren" -> 164;
	"yen" -> 165;
	"brvbar" -> 166;
	"sect" -> 167;
	"uml" -> 168;
	"copy" -> 169;
	"ordf" -> 170;
	"laquo" -> 171;
	"not" -> 172;
	"shy" -> 173;
	"reg" -> 174;
	"macr" -> 175;
	"deg" -> 176;
	"plusmn" -> 177;
	"sup2" -> 178;
	"sup3" -> 179;
	"acute" -> 180;
	"micro" -> 181;
	"para" -> 182;
	"middot" -> 183;
	"cedil" -> 184;
	"sup1" -> 185;
	"ordm" -> 186;
	"raquo" -> 187;
	"frac14" -> 188;
	"frac12" -> 189;
	"frac34" -> 190;
	"iquest" -> 191;
	"Agrave" -> 192;
	"Aacute" -> 193;
	"Acirc" -> 194;
	"Atilde" -> 195;
	"Auml" -> 196;
	"Aring" -> 197;
	"AElig" -> 198;
	"Ccedil" -> 199;
	"Egrave" -> 200;
	"Eacute" -> 201;
	"Ecirc" -> 202;
	"Euml" -> 203;
	"Igrave" -> 204;
	"Iacute" -> 205;
	"Icirc" -> 206;
	"Iuml" -> 207;
	"ETH" -> 208;
	"Ntilde" -> 209;
	"Ograve" -> 210;
	"Oacute" -> 211;
	"Ocirc"-> 212;
	"Otilde" -> 213;
	"Ouml" -> 214;
	"times" -> 215;
	"Oslash" -> 216;
	"Ugrave" -> 217;
	"Uacute" -> 218;
	"Ucirc" -> 219;
	"Uuml" -> 220;
	"Yacute" -> 221;
	"THORN" -> 222;
	"szlig" -> 223;
	"agrave" -> 224;
	"aacute" -> 225;
	"acirc" -> 226;
	"atilde" -> 227;
	"auml" -> 228;
	"aring" -> 229;
	"aelig" -> 230;
	"ccedil" -> 231;
	"egrave" -> 232;
	"eacute" -> 233;
	"ecirc" -> 234;
	"euml" -> 235;
	"igrave" -> 236;
	"iacute" -> 237;
	"icirc" -> 238;
	"iuml" -> 239;
	"eth" -> 240;
	"ntilde" -> 241;
	"ograve" -> 242;
	"oacute" -> 243;
	"ocirc" -> 244;
	"otilde" -> 245;
	"ouml" -> 246;
	"divide" -> 247;
	"oslash" -> 248;
	"ugrave" -> 249;
	"uacute" -> 250;
	"ucirc" -> 251;
	"uuml" -> 252;
	"yacute" -> 253;
	"thorn" -> 254;
	"yuml" -> 255;
	_ -> error
    end.

%%-type amp_digits(string(), int()) -> int() | error.

amp_digits([X | Xs], N) when X >= $0, X =< $9 ->
    amp_digits(Xs, N*10 + (X-$0));
amp_digits([], N) ->
    if
	N >= 0, N =< 8 -> error;
	N >= 127, N =< 159 -> error;
	N > 255 -> error;
	true -> N
    end.

dump_token(O, {raw, R}) -> 
    io:format(O, "~s", [R]);
dump_token(O, {tagStart, Tag, Args}) ->
    io:format(O, "<~s", [Tag]),
    lists:foreach(fun({Key,Val}) ->
			  io:format(O, ' ~s="~s"', [Key,Val])
		  end, Args),
    io:format(O, ">", []);
dump_token(O, {tagStart, Tag}) ->
    io:format(O, "<~s>", [Tag]);
dump_token(O, {tagEnd, Tag}) ->
    io:format(O, "</~s>", [Tag]);
dump_token(O, Other) ->
    io:format("dump_token ????~p~n", [Other]).

error(Ln, Term) ->
    throw({error, Ln, Term}).

%% reent_test breaks the input string at all possible places
%% and check that the parser correctly re-enters.

reent_test(Str=[H|T]) ->
    io:format("Parse:~s~n",[Str]),
    P = get_next_token(Str, 1),
    io:format("Result=~p~n",[P]),
    reent_test([H], T, P).

reent_test(_, [], _) ->
    ok;
reent_test(L1, L2=[H|T], P) ->
    io:format("Testing: ~p ++ ~p ",[L1,L2]),
    case get_next_token(L1, 1) of
	{more, C} -> 
	    P1 = continue(C, L2),
	    case P1 of
		P ->
		    io:format(" good~n"),
		    reent_test(L1 ++ [H], T, P);
		_ ->
		    io:format("~nParse failure:  Orig=~p Now=~p~n",
			      [P, P1]),
		    exit(oops)
	    end;
	_ ->
	    io:format(" good~n")
    end. 





