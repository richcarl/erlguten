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
%%==========================================================================

-module(eg_xml2richText).

-export([normalise_xml/2, 
	 normalise_xml/3, 
	 default_tagMap/1]).

-include("eg.hrl").

%% -define(DEBUG, true).

-ifdef(DEBUG).
dbg_io(Str) -> dbg_io(Str,[]).
dbg_io(Str,Args) ->
    io:format("eg_xml2richText: ~p " ++ Str, [self()] ++ Args),
    ok.
-else.
%dbg_io(_) -> ok.
dbg_io(_,_) -> ok.
-endif.

%%----------------------------------------------------------------------
%% normalise_xml(XML, RichTextTags, FontMap) ->
%%   XML = XML parse tree
%%   The tree is walked - if any Tag is in RichTextTags
%%   Then the subtree of this tag is assumend to be rich text
%%   RichTextTags = [Tag]
%%   FontMap = [#face{}]

%% Invarients no consequative spaces 
%% or spaces next to NLs

default_tagMap(Pts) -> 
    {[p],
     [{default,eg_richText:mk_face("Times-Roman", Pts, true, default, 0)},
      {em,     eg_richText:mk_face("Times-Italic", Pts, true, default, 0)},

      %% XXX !!! the font ZapfChancery-MediumItalic is not availible
      {red,    eg_richText:mk_face("ZapfChancery-MediumItalic", Pts, true, 
				   {1,0,0},0)},
      {blue,   eg_richText:mk_face("ZapfChancery-MediumItalic", Pts, true, 
				   {0,0,1},0)},

      {code,   eg_richText:mk_face("Courier", Pts, false, default, 0)},
      {b,      eg_richText:mk_face("Times-Bold", Pts, true, default, 0)}
     ]}.

normalise_xml(XML, {StandardTags, TagMap}) ->
    normalise_xml(XML, StandardTags, TagMap).

normalise_xml({Tag, Args, L}, RichTextTags, TagMap) ->
    case lists:member(Tag, RichTextTags) of
	true ->
	    L1 = normalise_richText(L, TagMap),
	    {Tag, Args, L1};
	false ->
	    L1 = lists:map(fun(I) ->
				   normalise_xml(I, RichTextTags, TagMap)
			   end, L),
	    {Tag, Args, L1}
    end;
normalise_xml(Z, _, _) ->
    dbg_io("I cannot normalise:~p~n",[Z]).

normalise_richText(Items, FontMap) ->
    L0 = lists:foldl(fun(I, L0) -> normalise_inline(I, FontMap, L0) end, 
		     [], Items),
    L1 = lists:reverse(L0),
    test_inline_invarient(L1),
    {richText, L1}.

test_inline_invarient([H1,H2|T]) ->    
    case {eg_richText:classify_inline(H1), eg_richText:classify_inline(H2)} of
	{space, space} ->
	    dbg_io("Warning spaces:~p ~p~n",[H1,H2]),
	    test_inline_invarient([H2|T]);
	{nl, space} ->
	    dbg_io("Warning NL + NL:~p ~p~n",[H1,H2]),
	    test_inline_invarient([H1|T]);
	{space,nl} ->
	    dbg_io("Warning spaces + NL:~p ~p~n",[H1,H2]),
	    test_inline_invarient([H2|T]);
	_ ->
	    test_inline_invarient([H2|T])
    end;
test_inline_invarient(_) ->
    true.

normalise_inline({raw,Str}, FontMap, L) ->
    normalise_tag(default, Str, FontMap, L);
normalise_inline({Tag, _, [{raw,Str}]}, FontMap, L) ->
    normalise_tag(Tag, Str, FontMap, L);
normalise_inline({Tag, _, []}, FontMap, L) ->
    L.

normalise_tag(Tag, Str, FontMap, L) ->
    Face = get_face(Tag, FontMap),
    case eg_richText:is_face_breakable(Face) of
	true ->
	    normalise_str(Str, Face, L, skip_ws);
	false ->
            normalise_str(Str, Face, L, keep_ws)
% 	    Wd = eg_richText:mk_fixedStr(Face, Str),
% 	    [Wd|L]
    end.

get_face(Tag, [{Tag,Face}|_]) -> Face;
get_face(Tag, [_|T]) -> get_face(Tag, T);
get_face(Tag, []) ->
    dbg_io("There is no face associated with Tag=~p~n",[Tag]),
    eg_pdf:default_face().

%% Collect spaces nls etc.
%% in a breakable face
normalise_str([$\r,$\n|T], Face, L, WS) ->
    normalise_str(T, Face, [eg_richText:mk_nl(Face)|L], WS);
normalise_str([$\n|T], Face, L, WS) ->
    normalise_str(T, Face, [eg_richText:mk_nl(Face)|L], WS);
normalise_str([H|T], Face, L, WS) ->
    case {is_white(H), WS} of 
	{true, skip_ws} ->
	    %% Hop over the white space
	    %% If we get to \n put in a NL otherwise
	    %% put in a space
	    T1 = skip_white(T),
	    case T1 of
		[] ->
		    Space = eg_richText:mk_space(Face),
		    normalise_str(T1, Face, [Space|L], WS);
		[H2|_] ->
		    case is_nl(H2) of
			true ->
			    normalise_str(T1, Face, L, WS);
			false ->
			    Space = eg_richText:mk_space(Face),
			    normalise_str(T1, Face, [Space|L], WS)
		    end
	    end;
        {true, keep_ws} ->
            Space = eg_richText:mk_space(Face),
            normalise_str(T, Face, [Space|L], WS);
	{false, _} ->
	    {Str, T1} = collect_word(T, [H]),
	    Word = eg_richText:mk_word(Face, Str),
	    normalise_str(T1, Face, [Word|L], WS)
    end;
normalise_str([], _, L, WS) ->
    L.

%% End Normalise XML
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% misc

is_white($\s) -> true;
is_white($\t) -> true;
is_white(_)   -> false.

is_white_or_nl($\n) -> true;
is_white_or_nl($\r) -> true;
is_white_or_nl(X)   -> is_white(X).

is_nl($\n) -> true;
is_nl($\r) -> true;
is_nl(_)   -> false.

skip_white(X=[H|T]) ->
    case is_white(H) of
	true  -> skip_white(T);
	false -> X
    end;
skip_white([]) ->
    [].

collect_word(X=[H|T], L) ->
    case is_white_or_nl(H) of
	true  -> {lists:reverse(L), X};
	false -> collect_word(T, [H|L])
    end;
collect_word([], L) ->
    {lists:reverse(L), []}.

