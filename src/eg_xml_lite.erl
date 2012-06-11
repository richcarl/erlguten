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
%% Authors: Joe Armstrong <joe@sics.se>
%% Purpose: XML parser
%%==========================================================================

-module(eg_xml_lite).

-export([parse_all_forms/1, 
	 parse_all_forms/2, 
	 parse_single_form/2,
	 parse_file/1,
	 continue/2, 
	 pp/1, 
	 xml2bin/2, 
	 bin2xml/2, 
	 test/1
	]).


%% ============================================================================

%% Test cases

test(1) ->
    parse_all_forms("<a>aa<b>aaa</b></a>");
test(2) ->
    {more, C} = parse_single_form("<", 0),
    continue(C, "abc>");
test(3) ->
    reent_test("<a def=\"ads\"   ghi  = 'abc'  >aa<b>aaa</b>"
               "gg<ab/>aa<abc a='bb' /></a>");
test(4) ->
    parse_all_forms("<a>bc</i>");
test(5) ->
    parse_all_forms("<?xml version=\"1.0\"?>
<!DOCTYPE report SYSTEM \"report-xml.dtd\">
<report>
  <header>a</header></report>");
test(6) ->
    parse_all_forms("<p>aaa<br/>aaa</p>");
test(7) ->
    parse_all_forms("<p>aaaa<![CDATA[
zip a doodly]]> aa </p>");
test(8) ->
    parse_all_forms("<?xml version=\"1.0\"?>
<!DOCTYPE fooy doody>
<!-- this is just a ball of fun -->
<p>aaa</p>").

%% This is a reentrant parser for XML streams

%% +deftype parse_result() =  {ok, xmlParseTree(), RestString : string()}
%%                         |  {more, cont()}
%%                         |  {error, parse_error()}
%% +type start(Str)                   -> parse_result().
%% +type more(Str, Cont)              -> parse_result().
%% +type format_error(parser_error()) -> string().
%% +type start_cont()                 -> cont().

%% parse_file(File) -> {error, What} | [Forms]

parse_file(F) ->
    case file:read_file(F) of
	{ok, Bin} ->
	    Result = parse_all_forms(binary_to_list(Bin), 1),
	    %% case Result of
	    %%   {error, E} -> true;
	    %% Tree -> pp(Tree)
	    %% end,
	    Result;
	Error ->
	    Error
    end.

xml2bin(In, Out) ->
    case file:read_file(In) of
	{ok, Bin} ->
	    case parse_all_forms(binary_to_list(Bin), 0) of
		{ok, Tree, _} ->
		    file:write_file(Out, term_to_binary(Tree));
		E = {error, _X} ->
		    E;
		{more, _} ->
		    {error, incomplete}
	    end;
	Error ->
	    Error
    end.

bin2xml(In, Out) ->
    case file:read_file(In) of
	{ok, Bin} ->
	    Tree = binary_to_term(Bin),
	    file:write_file(Out, pp(Tree));
	Error ->
	    Error
    end.

atomize(A={Atom,_}) when is_atom(Atom) ->
    A;
atomize({Str,Args,List}) -> 
    {list_to_atom(Str), Args, lists:map(fun atomize/1, List)}.
    
%%----------------------------------------------------------------------

%% Top level  ...

parse_all_forms(Str) -> parse_all_forms(Str, 0).

parse_all_forms(Str, Line) -> top_parse_loop(Str, Line, []).

top_parse_loop(Str, Line, L) ->
    case parse_single_form(Str, Line) of
	{ok, Form, Str1, Line1} -> 
	    case all_blanks(Str1) of
		true ->
		    lists:reverse([Form|L]);
		false ->
		    top_parse_loop(Str1, Line1, [Form|L])
	    end;
	E={error, _Why} ->
	    E;
	{more, _Cont} ->
	    {error, more_data_expected}
    end.

parse_single_form(Str, Line) ->
    parse([], Str, Line).

continue(Cont, Str) ->
    Cont(Str).

parse(State, Str, Line) ->
    tokenise_result(eg_xml_tokenise:get_next_token(Str, Line), State).

parse_cont(State, Cont, Str) ->
    tokenise_result(eg_xml_tokenise:continue(Cont, Str), State).

tokenise_result({error, Line, What}, _State) ->
    {error,{errorInLine,Line,What}};
tokenise_result({done, Token, Str1, Line1}, State) ->
    %% io:format("Token= ~p Str1=~p Line1=~p~n",[Token, Str1, Line1]),
    case step_parser(State, Token) of
	{more, State1} ->
	    parse(State1, Str1, Line1);
	{done, Parse} ->
	    {ok, Parse, Str1, Line1};
	{error, What} ->
	    {error, {errorInLine, Line1,What}}
    end;
tokenise_result({more, Cont}, State) ->
    {more, fun(Str) -> parse_cont(State, Cont, Str) end}. 
			   
%% The Stack is just [{STag,Args,Collected}]
%% pcdata and completed frames are just pushed onto the stack
%% When an end tag is found it is compared with the start tag
%% if it matches the stack frame is popped and it is
%% merged into the previous stack frame

%% step_parser(State, Event) -> {more, State1} | {done, Parse} | {error, What}

step_parser(Stack, {sTag, _, Tag, Args}) ->
    %% Push new frame onto the stack
    {more, [{Tag, lists:sort(Args), []}|Stack]};
step_parser([{Tag,Args,C}|L], _P={Flat, _, D}) when Flat == pi;
						   Flat == raw;
						   Flat == cdata;
						   Flat == comment;
						   Flat == doctype ->
    {more, [{Tag,lists:sort(Args),[{Flat,D}|C]}|L]};
step_parser([{Tag,Args,C}|L], {empty, _, TagE, ArgsE}) ->
    {more, [{Tag,Args,[{TagE,lists:sort(ArgsE),[]}|C]}|L]};
step_parser([{Tag, Args, C}|L], {eTag, _, Tag}) ->
    %% This is a matching endtag
    %% Now we normalise the arguments that were found
    C1 = deblank(lists:reverse(C)),
    pfinish([{Tag,Args,C1}|L]);
step_parser([{STag, _Args, _C}|_L], {eTag, _, Tag}) ->
    {error,{badendtagfound,Tag,starttagis,STag}};
step_parser([], {raw, _, S}) ->
    case all_blanks(S) of
	true ->
	    {more, []};
	false ->
	    {error, {nonblank_data_found_before_first_tag, S}}
    end;
step_parser([], {Tag,_,D}) when Tag==comment; Tag==doctype; Tag==pi ->
    {done, {Tag, D}};
step_parser(S, I) ->
    io:format("UUgh:Stack=~p Item=~p~n",[S, I]).


pfinish([X])                 -> {done, {xml, atomize(X)}};
pfinish([H1,{Tag,Args,L}|T]) -> {more, [{Tag,Args,[H1|L]}|T]}.

deblank(S=[{raw, _C}]) -> S;
deblank(X) -> deblank1(X).

deblank1([H={raw,X}|T]) ->
    case all_blanks(X) of
	true  -> deblank1(T);
	false -> [H|deblank1(T)]
    end;
deblank1([H|T]) ->
    [H|deblank1(T)];
deblank1([]) ->
    [].

all_blanks(L) -> lists:all(fun is_Blank/1, L).

is_Blank($ )  -> true;
is_Blank($\n) -> true;
is_Blank($\t) -> true;
is_Blank($\r) -> true;
is_Blank(_)   -> false.


%% Pretty printer

pp(Tree) ->
    pp(Tree, 0).

pp({Node,Args,[{raw,Str}]}, Level) ->
    S = name(Node),
    [indent(Level),"<",S,pp_args(Args),">",Str,"</",S,">\n"];
pp({Node,Args,[]}, Level) ->
    S = name(Node),
    [indent(Level),"<",S,pp_args(Args),"></",S,">\n"];
pp({Node,Args,L}, Level) ->
    S = name(Node),
    [indent(Level),"<",S,pp_args(Args),">\n",
     lists:map(fun(I) -> pp(I, Level+2) end, L),
     indent(Level),"</",S,">\n"];
pp({raw,Str}, Level) ->
    [indent(Level),Str,"/n"];
pp(X, _Level) ->
    io:format("How do I pp:~p~n",[X]),
    ["oops"].
    
pp_args([]) -> [];
pp_args([{Key,Val}|T]) ->
    Q=quote(Val),
    [" ",name(Key),"=",Q,Val,Q|pp_args(T)].

quote(Str) ->
    case lists:member($", Str) of
	true  -> $';
	false -> $"
    end.

name(X) ->
    atom_to_list(X).

indent(0) -> [];
indent(N) -> [$ |indent(N-1)].

reent_test(_O)->a.

