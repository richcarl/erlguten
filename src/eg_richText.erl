%%==========================================================================
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
%%==========================================================================

-module(eg_richText).

%% OLD AND BROKEN doc ?! (see modern version further down)

%% encapsulates operations on text() and inline() objects

%% ADT 
%%   +deftype richText() = {richText, [inLine()]
%%   +deftype inLine()   = Word | FixedString | Opaque | Space | NL
%%   +deftype Word       = {word, Width, Face, Str},
%%   +deftype FixedStr   = {fixedStr, Width, Face, Str},
%%   +deftype Opaque     = {opaque, Width, X}
%%   +deftype Space      = {space, Width, Face}
%%   +deftype NL         = {nl, Face}
%%   +deftype Face       = {Font, PointSize, Voffset, Color, Breakable}
%%   +deftype Color      = default | {R,G,B}
 
%% Interface 
%%   str2text(Foint, PointSize, Str) -> text()
%%   text2str(text()) -> str().

%% ----------------------------------------------------------------------------
%% This is what str2richText/6 returns, above ADT and Interface appears to be
%% out of date.
%%
%% @type richText() = {richText, [inLine()]}
%% @type inLine()   = word() | space() | nl()
%% @type word()     = {word, Width::thousendth_pt(), Face::record(face), 
%%                     Str::string()}
%% @type space()    = {space, Width::thousendth_pt(), Face::record(face)}
%% @type nl()       = {nl, Face::record(face)}
%% @type thousendth_pt() = integer()
%% @end
%% ----------------------------------------------------------------------------

-export([test/1, 
	 cloan_space/1,
	 cloan_word/2,
	 font/1,
	 fontFromFace/1,
	 classify_inline/1,
	 widthExcludingSpaces/1,
	 pointSize/1,
	 width/1,
	 color/1,
	 is_face_breakable/1,
	 is_breakable/1,
	 is_space/1, 
	 is_nl/1,
	 is_word/1,
	 lineWidth/1,
	 numberOfSpaces/1,
	 mk_test_word/1,
	 mk_face/5,
	 mk_fixedStr/2,
	 mk_word/2,
	 mk_nl/1,
	 mk_space/1,
	 string/1,
	 str2richText/1,
	 str2richText/2,
	 str2richText/6, 
	 richText2str/1,
	 width_of/3
	]).


-include("eg.hrl").


%% -define(DEBUG, true).

-ifdef(DEBUG).
dbg_io(Str) -> dbg_io(Str,[]).
dbg_io(Str,Args) ->
    io:format("eg_richText: ~p " ++ Str, [self()] ++ Args),
    ok.
-else.
dbg_io(_) -> ok.
dbg_io(_,_) -> ok.
-endif.


test(1) ->
    str2richText("TimesDutch", 12, 0, default, true,
"Hello joe how are you today?
May I take this opportunity
of saying
that my favorite color is blue.
Have a nice day,
from Mr. C. Computer.").

richText2str({richText, L}) ->
    eg_pdf_op:flatten(lists:map(fun inline2str/1, L)).

inline2str({word,_,_Face,Str}) -> Str;
inline2str({space,_, _}) -> " ";
inline2str({nl,_}) -> "\n";
inline2str({fixedStr,_,_Face,Str}) -> Str;
inline2str(_) -> "".

%% ----------------------------------------------------------------------------
str2richText(Str) ->
    str2richText("Times-Roman", 12, 0, default, true, Str).

str2richText(Str, Pts) ->
    str2richText("Times-Roman", Pts, 0, default, true, Str).

%% ----------------------------------------------------------------------------
%% @spec  str2richText(Font, Point, Voff, Color, Break, Str) -> richText()
%% @doc   convert string to "word" sequence containing word font/size info 
%% @end------------------------------------------------------------------------
str2richText(Font, Point, Voff, Color, Break, Str) ->
    valid_bool(Break),
    F  = fontHandler(Font),
    Face = #face{font=F, pointSize=Point, vOffset=Voff, 
		 color=Color, breakable=Break},
    L1 = normalise_str(Str, []),
    L2 = lists:map(fun({wd1,S}) ->
			   Width = width_of(F, Point, S),
			   {word, Width, Face, S};
		      (spaces) ->
			   Width = width_of(F, Point, [$\s]),
			   {space, Width, Face};
		      (lineFeed) ->
			   {nl, Face}
		   end, L1),
    {richText, L2}.

valid_bool(true) -> true;
valid_bool(false) -> true;
valid_bool(X) -> exit({badarg, str2richText, breakNotBool, was, X}).

%% ----------------------------------------------------------------------------
%% @spec normalise_str(Str, Acc) -> [spaces | lineFeed | {wd1, Word::string()}]
%% @doc  split Str into word and space segments, single and duplicate 
%%       whitespace chars get replaced by a single 'spaces', NL -> lineFeed 
%% @end------------------------------------------------------------------------
normalise_str([], L) ->
    lists:reverse(L);
normalise_str([$\n|T], L) ->
    normalise_str(T, [lineFeed|L]);
normalise_str([H|T],  L) ->
    case is_white(H) of
        true ->
            T1 = skip_white(T),
            normalise_str(T1, [spaces|L]);
        false ->
            {Word, T1} = collect_word(T, [H]),
            normalise_str(T1, [{wd1,Word}|L])
    end.

is_white($\s) -> true;
is_white(160) -> true; %% non-break-space
is_white($\t) -> true;
is_white($\r) -> true;
is_white(_)   -> false.

skip_white(X=[H|T]) ->
    case is_white(H) of
        true  -> skip_white(T);
        false -> X
    end;
skip_white([]) ->
    [].

collect_word(X=[$\n|_T], L) ->
    {lists:reverse(L), X};
collect_word(X=[H|T], L) ->
    case is_white(H) of
        true  -> {lists:reverse(L), X};
        false -> collect_word(T, [H|L])
    end;
collect_word([], L) ->
    {lists:reverse(L), []}.

width_of(Font, PointSize, Str) ->
    PointSize * sizeof(Font, Str).


%%----------------------------------------------------------------------
%% sizeof(FontIndex, Str)
%%   Computes width of Str which is of type FontIndex
%%   Size is correctly adjusted for kerning information

sizeof(Font, Str) ->
    Widths = lists:map(fun(I) -> char_width(Font, I) end, Str),
    %% dbg_io("Str=|~s| Widths=~p~nFont=~p~n",[Str, Widths, Font]),
    W1 = lists:sum(Widths),
    %% and add the correct kerning info
    kern_adj(Str, W1, Font).

char_width(Font, I) ->
    case Font:width(I) of
	Width when is_integer(Width) ->
	    Width;
	_ ->
	    dbg_io("Character ~w in font ~p has no width~n", [I, Font]),
	    Font:width($\s)
    end.

kern_adj([H1,H2|T], W, Font) ->
    Extra = Font:kern(H1, H2),
    kern_adj([H2|T], W+Extra, Font);
kern_adj(_, W, _) ->
    W.

%%----------------------------------------------------------------------
%% access funtions

is_space(X) -> element(1,X) == space.

is_word(X) ->    element(1, X) == word.
    
is_nl(X) -> element(1, X) == nl.

is_breakable({word,_,Face,_Str}) -> Face#face.breakable;
is_breakable(_) -> false.

%% Make a new word based on the face of an old word

cloan_word({word,_,Face,_}, Str) ->
    Font = Face#face.font,
    PointSize = Face#face.pointSize,
    W = width_of(Font, PointSize, Str),
    {word, W, Face, Str}.

cloan_space({word,_,Face,_}) ->
    cloan_space_from_face(Face);
cloan_space({nl, Face}) ->
    cloan_space_from_face(Face).

cloan_space_from_face(Face) ->
    Font = Face#face.font,
    PointSize = Face#face.pointSize,
    W = width_of(Font, PointSize, [$\s]),
    {space, W, Face}.

width({word,W,_,_})     -> W;
width({opaque,W,_})     -> W;
width({space,W,_})      -> W;
width({nl,_})           -> 0;
width({fixedStr,W,_,_}) -> W.

font({word,_,F,_})     -> F#face.font;
font({opaque,_,_F})     -> unknown;
font({space,_,F})      -> F#face.font;
font({nl,F})           -> F#face.font;
font({fixedStr,_,F,_}) -> F#face.font.

fontFromFace(F) -> F#face.font.

color({word,_,F,_})     -> F#face.color;
color({opaque,_,_F})     -> unknown;
color({space,_,F})      -> F#face.color;
color({nl,F})           -> F#face.color;
color({fixedStr,_,F,_}) -> F#face.color.

pointSize({word,_,F,_})     -> F#face.pointSize;
pointSize({opaque,_,_F})     -> unknown;
pointSize({space,_,F})      -> F#face.pointSize;
pointSize({nl,F})           -> F#face.pointSize;
pointSize({fixedStr,_,F,_}) -> F#face.pointSize.

classify_inline({word,_W,_,_})     -> word;
classify_inline({opaque,_W,_})     -> opaque;
classify_inline({space,_W,_})      -> space;
classify_inline({nl,_})           -> nl;
classify_inline({fixedStr,_W,_,_}) -> fixedStr.

string({word,_,_,S})     -> S;
string({fixedStr,_,_,S}) -> S;
string({space,_,_})      -> " ".
     
%%   +deftype FixedStr   = {fixedStr, Width, Face, Str},
%%   +deftype Opaque     = {opaque, Width, X}
%%   +deftype Space      = {space, Width, Face}
%%   +deftype NL         = {nl, Face}
%%   +deftype Face       = {Font, PointSize, Voffset, Color, Breakable}
%%   +deftype Color      = default | {R,G,B}

mk_word(Face, Str) ->
    F = Face#face.font,
    P = Face#face.pointSize,
    Width = width_of(F, P, Str),
    {word, Width, Face, Str}.

mk_fixedStr(Face, Str) ->
    F = Face#face.font,
    P = Face#face.pointSize,
    Width = width_of(F, P, Str),
    {fixedStr, Width, Face, Str}.

mk_nl(Face) ->
    {nl, Face}.

mk_space(Face) ->
    F = Face#face.font,
    P = Face#face.pointSize,
    Width = P * char_width(F, 32),
    {space, Width, Face}.

%% make a word that we can play with

mk_test_word(Str) ->
    F = fontHandler("Times-Roman"),
    Face = #face{font=F, pointSize=16, vOffset=0,
		 color=default, breakable=true},
    Width = width_of(F, 16, Str),
    {word, Width, Face, Str}.

mk_face(Font, PointSize, Breakable, Color, VoffSet) ->
    F = fontHandler(Font),
    #face{font=F, pointSize=PointSize, vOffset=VoffSet,
	  color=Color, breakable=Breakable}.

fontHandler(Font) ->
    case eg_font_map:handler(Font) of
	undefined ->
	    dbg_io("There is no font called:~s~n",[Font]),
	    dbg_io("Using Times-Roman~n"),
	    eg_font_map:handler("Times-Roman");
	Mod ->
	    Mod
    end.

lineWidth(Toks) ->
    lists:foldl(fun(I, S) -> width(I) + S end, 0, Toks).
    
numberOfSpaces(Toks) ->
    Toks1 = lists:filter(fun(I) -> is_space(I) end, Toks),
    length(Toks1).

widthExcludingSpaces(Toks) ->
    lists:foldl(fun(I, S) ->
			case is_space(I) of
			    true  -> S;
			    false -> S + width(I)
			end
		end, 0, Toks).

is_face_breakable(F) ->
    F#face.breakable.

