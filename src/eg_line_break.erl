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
%% Purpose: Line breaking algorithm
%%==========================================================================

-module(eg_line_break).

%% TODO: Make sure we exit the split para recursion if the paragraph is too
%% short

%% para_break ONLY computes the break points

-export([break_richText/2,
	 break_richText/3
	]).


-export([make_partitions/2]). % for testing only
                              % this export can be removed later

-include("eg.hrl").

%% -define(DEBUG, true).

-ifdef(DEBUG).
dbg_io(Str) -> dbg_io(Str,[]).
dbg_io(Str,Args) ->
    io:format("eg_line_break: ~p " ++ Str, [self()] ++ Args),
    ok.
display_lines(Lines) ->
    lists:foreach(fun(I) ->
		    dbg_io("~s~n",[r2s(I)])
	    end, Lines).
r2s(Toks) -> eg_richText:richText2str({richText, Toks}).
-else.
%dbg_io(_) -> ok.
dbg_io(_,_) -> ok.
-endif.

-define(en_GB, eg_hyphen_rules_en_GB).

%% ----------------------------------------------------------------------------
%% @doc Default to english (Great Britain) style linebreaks
%% @end------------------------------------------------------------------------
break_richText(RT, TW) ->
    break_richText(RT, TW, ?en_GB).

%% ----------------------------------------------------------------------------
%% @spec break_richText(RT::richText(),{Type, Widths}, Rules) ->  
%%                      {Lines, Widths, Spill}
%% @doc  Type    = justified | ragged | preformatted | centered | 
%%                 ragged_force_split | simple_hyphenate
%%       Rules   = is a the module name of the eg_hyphen_rules_*.erl file to use
%%                 to determine how to hyphenate, this argument is only used
%%                 when Type = justified 
%%       Widths  = [integer()] = lengths of the lines
%%       Lines   = [richText()], RT split into lines 
%%       Widths' = remaining Widths values
%%       Spill   =
%%
%%       justified    = Optimally break each line depending upon the widths
%%                      doing hyphenation where possible and required
%%       ragged       = break on first fit without hyphenation
%%       preformatted = break on NL
%%       centered     = break on NL
%%       ragged_force_split = works as 'ragged' (break at word boundry) but
%%                      this version will split words, that don't fit on the
%%                      current line, without adding hyphenation.
%%       simple_hyphenate = workd as 'ragged_force_split' but adds hyphen at
%%                      end of split words.
%%
%% Note: ragged_force_split can still overflow the bounds of Widths if a single
%%       letter is wider than the row.
%% Note: ragged_force_split can in theory perform as bad as O(WordLen^2) 
%%       if the initial split position guess is badly of e.g. a variable width
%%       font used with the string "1111111111111...XXX", when the string and
%%       __line width__ is very long.
%% @end -----------------------------------------------------------------------
break_richText({richText, T}, {justified, W}, Rules) -> 
    text2para_widths(T, justified, W, Rules);

break_richText({richText, T}, {ragged, W}, Rules) -> 
    text2para_widths(T, ragged, W, Rules);

break_richText({richText, T}, {ragged_force_split, W}, Rules) -> 
    text2para_widths(T, ragged_force_split, W, Rules);

break_richText({richText, T}, {simple_hyphenate, W}, Rules) -> 
    text2para_widths(T, simple_hyphenate, W, Rules);

break_richText({richText, T}, {preformatted, W}, _) -> 
    break_on_nl(T, W, []);

break_richText({richText, T}, {centered, W}, _) -> 
    break_on_nl(T, W, []).


%% ParaShape = ragged | ragged_force_split | justified | simple_hyphenate
text2para_widths(Txt, ParaShape, Widths, Rules) ->
    Txt1 = replace_nls_with_spaces(Txt),
    %% debug("text2", Txt1),
    case ParaShape of
	ragged             -> text2para_ragged(Txt1, Widths, [], ParaShape);
	ragged_force_split -> text2para_ragged(Txt1, Widths, [], ParaShape);
	simple_hyphenate   -> text2para_ragged(Txt1, Widths, [], ParaShape);
	justified          -> justify(Txt1, Widths, Rules)
    end.

replace_nls_with_spaces(L) ->
    lists:map(fun(I) ->
		      case eg_richText:is_nl(I) of
			  true  -> eg_richText:cloan_space(I);
			  false -> I
		      end
	      end, L).


%% Toks = inLine()
%%XXX only occures with initial Widths = [] ???
text2para_ragged(Toks, [], L, _SplitType) -> 
    {finalise_ragged(L), [], Toks};
text2para_ragged([], Widths, L, _SplitType) ->
    {finalise_ragged(L), Widths, []};

%% reuse last Widths value if there are fewer than the number of lines created
text2para_ragged(Lines, [W], L, SplitType) ->
    {Line, Rest} = first_break_line(Lines, W, SplitType),
    text2para_ragged(Rest, [W], [Line|L], SplitType);
text2para_ragged(Lines, [H|T], L, SplitType) ->
    {Line, Rest} = first_break_line(Lines, H, SplitType),
    text2para_ragged(Rest, T, [Line|L], SplitType).

%% build return lines of richText()
finalise_ragged(Final) ->
    lists:map(fun({_Cost,_Len,Line}) -> {richText, Line} end, 
	      lists:reverse(Final)).


%% get the first line of length Len from Toks
%% return: {Line::inLine(), RestToks}
first_break_line(Toks, Len, SplitType) ->
    Toks1 = remove_leading_spaces(Toks),
    %% eg_pdf:get_string_width(...) calcs use trunc(Val / 1000) to get length in
    %% pt, so a line in 1000th pt is shorter than Len if < (Len+1)*1000
    first_break_line(Toks1, 0, (Len+1)*1000, [], SplitType).

%% Len = 1000th pt
first_break_line(All=[H|T], Sum, Len, L, SplitType) ->
    Sum1 = Sum + eg_richText:width(H),
    case Sum1 < Len of
	true -> 
	    first_break_line(T, Sum1, Len, [H|L], SplitType);
	%% a single word >= Len
	false when L == [] ->
	    case SplitType of
		%% word to big - allowed to leak outside aloted space
		ragged ->
		    %%XXX first rm is redundant ???
		    {{Sum1, Len, remove_leading_spaces([H])}, 
		     remove_leading_spaces(T)};
		%% force split word to confirm to Len
		ragged_force_split ->
		    {HeadTok, RTok} = force_split(H, Len, SplitType),
		    Sum2 = Sum + eg_richText:width(HeadTok),
		    Line = {Sum2 ,Len, [HeadTok]},
		    RestToks = RTok ++ T,
		    {Line, RestToks};
		%% force split word to confirm to Len, but add hyphen
		simple_hyphenate ->
		    {HeadTok, RTok} = force_split(H, Len, SplitType),
		    Sum2 = Sum + eg_richText:width(HeadTok),
		    Line = {Sum2 ,Len, [HeadTok]},
		    RestToks = RTok ++ T,
		    {Line, RestToks}
	    end;
	false ->
	    {{Sum, Len, lists:reverse(remove_leading_spaces(L))}, 
	     remove_leading_spaces(All)}
    end;
first_break_line([], Sum, Len, L, SplitType) ->
    {{Sum, Len, lists:reverse(remove_leading_spaces(L))}, []}.


%% ----------------------------------------------------------------------------
%% @spec  force_split(Tok::inline(), Len::integer(), 
%%                    SplitType::ragged_force_split | simple_hyphenate) -> 
%%        {HeadTok, RestTok::[inline()] | []}
%% @doc   Tok = word() as space() and nl() have already been removed
%%        Len = 1000th pt
%% @end------------------------------------------------------------------------
force_split(Tok, Len, SplitType) ->
    {word, Width, Face, Str} = Tok,
    StrLen = length(Str),
    GLen = guess_head_length(StrLen, Len, Width),
    #face{font = Font, pointSize = PointSize} = Face, 
    {HLen,HyphenStr} = 
	case SplitType of
	    ragged_force_split -> 
		{force_split(Str, StrLen, Len, GLen, Font, PointSize), ""};
	    simple_hyphenate ->
		{hyphen_split(Str, StrLen, Len, GLen, Font, PointSize), "-"}
	end,

    {HStr0, RStr} = lists:split(HLen, Str),
    HStr = HStr0 ++ HyphenStr, % add hyphen if used
    HTok = {word, eg_richText:width_of(Font, PointSize, HStr), Face, HStr},
    RTok = case RStr of
	       [] -> []; % this occures e.g. when Str is a to big letter
	       _ ->
		   [{word, eg_richText:width_of(Font, PointSize, RStr), 
		     Face, RStr}]
	   end,
    {HTok, RTok}.

%% initial guess (length in no. of chars) - how many chars in the front
%% of Str can fit on the row of length Len, with the font and font size
%% as set in Face ?
guess_head_length(StrLen, Len, Width) ->
    Min = 0,
    Max = StrLen,
    Guess = trunc(StrLen * (Len / safe_width(Width))),
    
    %% don't start with the Min or Max values as xxx_split(...) will then 
    %% imidiatly stop in a base case
    if Guess >= Max -> 
	    Max - 1;
       Guess =< Min ->
	    Min + 1;
       true ->
	    Guess
    end.

%% 
safe_width(Val) ->
    if Val >= 1 ->
	    Val;
       %% text with point size close to 0 (-> Val = 0) or negative
       true ->
	    1 %XXX throw exception ???
    end.


%% return: integer() >= 1, number of initial Str letters to use 
%% split even if a single letter is > Len (doesn't fit in Len)
force_split(Str, StrLen, Len, GLen, Font, PointSize) when GLen =< 0 ->
    1;
force_split(Str, StrLen, Len, GLen, Font, PointSize) when GLen >= StrLen ->
    StrLen;
force_split(Str, StrLen, Len, GLen, Font, PointSize) ->
    case fits(Str, Len, GLen, Font, PointSize) of
	false -> % to long
	    force_split(Str, StrLen, Len, GLen-1, Font, PointSize);
	
	true -> % fits
	    case fits(Str, Len, GLen+1, Font, PointSize) of
		true ->
		    force_split(Str, StrLen, Len, GLen+1, Font, PointSize);
		false ->
		    GLen
	    end
    end.

%% return true if the GLen first chars in Str fit in Len 
%% Len = 1000th pt
fits(Str, Len, GLen, Font, PointSize) ->
    HStr = lists:sublist(Str, GLen),
    HWLen = ceiling(eg_richText:width_of(Font, PointSize, HStr)),
    HWLen =< Len.

%% round a number to next higher integer
ceiling(N) when is_number(N) ->
    case trunc(N) of
        I when I < N -> I + 1;
        I -> I
    end.

%% return: integer() >= 1, number of initial Str letters to use 
%% split even if a single letter (+ hyphen) is > Len (doesn't fit in Len)
hyphen_split(Str, StrLen, Len, GLen, Font, PointSize) when GLen =< 0 ->
    1;
hyphen_split(Str, StrLen, Len, GLen, Font, PointSize) when GLen >= StrLen ->
    StrLen;
hyphen_split(Str, StrLen, Len, GLen, Font, PointSize) ->
    case fits(Str ++ "-", Len, GLen, Font, PointSize) of
	false -> % to long
	    hyphen_split(Str, StrLen, Len, GLen-1, Font, PointSize);
	
	true -> % fits
	    case fits(Str ++ "-", Len, GLen+1, Font, PointSize) of
		true ->
		    hyphen_split(Str, StrLen, Len, GLen+1, Font, PointSize);
		false ->
		    GLen
	    end
    end.

%% ----------------------------------------------------------------------------

remove_leading_spaces(X=[H|T]) ->
    case eg_richText:is_space(H) of
	true  -> remove_leading_spaces(T);
	false -> X
    end;
remove_leading_spaces([]) -> [].


%% break_on_nl
break_on_nl(Toks, [], L)     -> {lists:reverse(L), [], {richText, Toks}};
break_on_nl([], T, L)        -> {lists:reverse(L), T, []};
break_on_nl(Toks, [H|T], L)  ->
    T1 = if T == [] -> [H];
            true    -> T
         end,
    {Line, Toks1} = collect_line(Toks, []),
    break_on_nl(Toks1, T1, [{richText, Line}|L]).

collect_line([H|T], L) ->
    case eg_richText:is_nl(H) of
	true  -> {lists:reverse(L), T};
	false -> collect_line(T, [H|L])
    end;
collect_line([], L) ->
    {lists:reverse(L), []}.


%%------------------------------------------------------------------------

%% OLD AND BROKEN doc ?! (see modern version further down)

%% Units = Picas (1 Pica = 12 points, 72 Ppints = 1")

%% justify([Tok], [Widths], PointSize)
%%   Tok       = {wd2,FontNumber,Width,Str} | {sp2,FontIndex,Width}
%%   [Widths]  = lines widths per line in picas
%%   PointSize = int() = point size of font

%% How does this work
%%   We have a list which starts [{Cost, Para, LineWidths, Result}]
%%      For each iteration
%%      1) We split Para into {FirstLine, Rest} pairs in all reasonable ways
%%         where FirstLine fits into hd(lineWidths) with Cost Cost'
%%         From this we form {Cost'', Rest, tl(LineWidth), FirstLine ++ Result}
%%      2) We sort by Cost'' and retain the first N pairs
%%      3) When Para = [] we remove the list from the iterator and
%%         check the final value of Cost
%%         If this is better than the best value we keep it

%% Iternate [{Cost,Text,Widths,Broken}]

%% ----------------------------------------------------------------------------
%% @spec  justify(Text::[word()|space()], Widths::[pt()], Rules) -> 
%%        impossible | {Lines, Widths, {richText, Spill}}
%% @doc   Lines  =
%%        Widths =
%%        Spill  = 
%%        Rules  = is a the module name of the eg_hyphen_rules_*.erl file to use
%%                 to determine how to hyphenate 
%% 
%% XXX same return val as break_richText(...)    
%% @end------------------------------------------------------------------------
justify(Text, Widths, Rules) ->
    %% dbg_io("justify Text=~p widths=~p~n",[Text,Widths]),
    case iterate([{0,Text,Widths,[],Rules}], none, 20) of
	{Cost, Lines, Widths1, Spill, Rules} ->
	    Lines1 = lists:map(fun(I) -> {richText, I} end, Lines),
	    {Lines1, Widths1, {richText, Spill}};
	none ->
	    impossible
    end.

iterate([], Best, Max) ->
    Best;
iterate(L, Best, Max) ->
    L1 = next_generation(L),
    L2 = lists:sort(fun({I,_,_,_,_}, {J,_,_,_,_}) -> I < J end, L1),
    L3 = trim(Max, L2),
    {Best1, L4} = finalise(L3, Best, []),
    iterate(L4, Best1, Max).

next_generation([])    -> [];
next_generation([H|T]) -> next(H) ++ next_generation(T).

%% next({Cost, RichText, Widths*, Before, Rules})

next({Cost, Text, [H|T], Before, Rules}) ->    
    L = break_line(Text, H, Rules),
    T1 = if T == [] ->
                 [H];
            true ->
                 T
         end,
    lists:map(fun({Cost1, {Line, Text1}}) ->
		      {Cost+Cost1*Cost1, Text1, T1, [Line|Before], Rules}
	      end, L).

%% finalising an item means we move it out of the iterator
%% into the final value

%% the first two cases are when we terminate normally
finalise([{Cost,More,[],Partition,Rules}|T], none, L) ->
    finalise(T, {Cost, lists:reverse(Partition), [], More, Rules}, L);
finalise([{Cost,More,[],Partition,Rules}|T], Best={C,_,_,_,_}, L) ->
    if 
	Cost < C ->
	    finalise(T, {Cost, lists:reverse(Partition),[], More, Rules}, L);
	true ->
	    finalise(T, Best, L)
    end;
%% these two cases are when we manage to fit the paragraph
finalise([{Cost,[],Ws,Partition,Rules}|T], none, L) ->
    finalise(T, {Cost, lists:reverse(Partition), Ws, [], Rules}, L);
finalise([{Cost,[],Ws,Partition,Rules}|T], Best={C,_,_,_,_}, L) ->
    if 
	Cost < C ->
	    finalise(T, {Cost, lists:reverse(Partition),Ws,[],Rules}, L);
	true ->
	    finalise(T, Best, L)
    end;
finalise([H|T], Best, L) ->
    finalise(T, Best, [H|L]);
finalise([], Best, L) ->
    {Best, L}.

trim(0, L)     -> [];
trim(N, [H|T]) -> [H|trim(N-1, T)];
trim(N, [])    -> [].

%% ----------------------------------------------------------------------------

-record(q,{minSpaceStretch,maxSpaceStretch}).

%% break_line(Toks, Len, Rules) -> [{Cost,Before,After}] sorted by Cost
%%  Toks  = Before = After = [inline()]
%%  Len   = width of paragraph in points
%%  Rules = is a the module name of the eg_hyphen_rules_*.erl file to use
%%          to determine how to hyphenate
%%  NOTES: 
%%      1) NLs have been replace by spaces PRIOR to calling this routine.
%%      2) The paragraph will only be split at a space 
%%         or in the middle of a hyphenatable word
%%      3) If the line fits then there is only one alternative
%%  Method:
%%    1) change all NL's into spaces
%%    2) compute the first before such that length(Before--Spaces) > Len
%%    3) Iterate 

break_line(Toks, Len, Rules) ->
    %% just make a quick check to see if the entire line fits
    L = case eg_richText:lineWidth(Toks) of
	    K when K < Len*1000 ->
		[{0,{Toks,[]}}];
	    _ ->
		Q = #q{maxSpaceStretch=150, minSpaceStretch=70},
		break_line(Toks, Len, Q, Rules)
	end,
    %% dbg_io("break_line|~s|~n=~p~n",
    %% [r2s(Toks),lists:map(fun({Cost, {L1,L2}}) -> 
    %% {Cost, r2s(L1),r2s(L2)} end, L)]),
    L.

break_line(Toks, Len, Q, Rules) ->
    %% dbg_io("break_line Len=~p measure=~p PointSize=~p~n",
    %% [Len,Measure,PointSize]),
    {Before, After} = worse_break_line(Toks, Len),
    %% dbg_io("Worse break line: Before=|~s|~nAfter=|~s|~n",[r2s(Before),
    %% r2s(After)]),
    L = break_before(lists:reverse(Before), After, Len, Q, [], Rules),
    filter_candidates(L, Len, Q).

%% worse_break_line(Toks, Len) -> {Before, After}
%%   Find the worse possible break of a line when the
%%   line has so much data that the size of all the characters excluding the
%%   spaces is greater than the required length.

%%   
%% Split Toks into {Before, After} when the length of
%% all the data in Before (excluding spaces) is greater than Len.
%% split *only* at a space. After splitting the After will begin with
%% a space (or be [])

worse_break_line(Toks, Len) -> worse_break_line(Toks, Len*1000, []).

worse_break_line(All=[H|T], Len, L) ->
    %% len is the length of the line in milli points
    case eg_richText:is_space(H) of
	true ->
	    case eg_richText:widthExcludingSpaces(L) of
		Len1 when Len1 > Len ->
		    {lists:reverse(L), All};
		_ ->
		    worse_break_line(T, Len, [H|L])
	    end;
	false ->
	    worse_break_line(T, Len, [H|L])
    end;
worse_break_line([], Len, L) ->
    {lists:reverse(L), []}.

filter_candidates(Partitions, Len, Q) ->
    P1 = lists:map(fun({Before, []}) ->
			   %% If After is zero and the line fits then it is a 
			   %% perfect fit
			   case eg_richText:lineWidth(Before) of
			       K when K < Len*1000 ->
				   {0, {Before,[]}};
			       _ ->
				   {badness(Before, Len, Q), {Before, []}}
			   end;
		      ({Before, After}) ->
			   {badness(Before, Len, Q), {Before, After}};
		      (X) ->
			   dbg_io("uugh=~p~n",[X]),
			   aaaa
		   end, Partitions),
    %% dbg_io("P1=~p~n",[lists:map(fun({C,{I,J}}) -> 
    %% {C,r2s(I),r2s(J)} end, P1)]),
    P2 = lists:filter(fun({-1000000, _}) -> false;
			 ({_,{[],_}}) -> false;
			 (_) -> true end, P1),
    %% dbg_io("P2=~p~n",[P2]),
    lists:sort(fun({I,_},{J,_}) -> I*I < J*J end, P2).

%% break_before(Before, After, Len, Q, .. , Rules)
%%   This breaks up the before sequence by moving objects onto the
%%   after sequence. This makes the line gapper and gappier.
%%   The before segment has the words in reverse order
%%   So normally before looks like
%%   [{wd2,..},{sp2, ...} - if it begins with a space we have
%%   a little problem

%% break_before(Before, After, Len, Q, L, Rules)

%% This bit is tricky the after segment starts with a space OR []
%% The starting point is
%%    
%%    Bn ...  B3 B2 B1 B0 | S A0 A1 ...
%%    
%%    Split this thus
%%  
%%    Bn ...  Bk S | M | S A0 A1
%%   
%%    M is called a segement. A segment is a sequence of items that
%%  must be moved as a whole. This is to maintain the invarient that
%%  we can only split on a space.

%%  1) If B0 = S => impossible
%%
%%    2)  If B0 is a word then hyphenate in all possible
%%        ways add into B0[1] and B0[2] to either side of the
%%        partition forming:
%%
%%        Bn ...  B3 B2 B1 B0[1] | [B0[2] S A0 A1 ...
%%
%%    3)  Now run shift_until_space

%%    2)  Can we move all of B0 onto After
%%        Yes if B1 = space
%%        No otherwise

%% There are two invarients in this loop
%% 1) Before does not begin with a space
%% 2) After  does begin with a space

break_before([], After, Len, Q, L, _) ->
    check_after_invarient(After),
    L;
break_before(Before, After, Len, Q, L0, Rules) ->
    check_before_invarient(Before),
    check_after_invarient(After),
    %% Before is in reverse order
    %% dbg_io("Break_before:~s~n",[r2s(Before)]),
    B = badness(Before, Len, Q),
    %% dbg_io("Badness=~p~n", [B]),
    case B of
	%% stop when the line is very loose
	Finite when Finite > 40, length(L0) > 10 ->
	    L0;
	_ ->
	    %% Given Bn.....B0
	    %% Split into two segments
	    %% {Sement, S2} such that Segment contains no blanks
	    %% then {Bn.....S, ... B0}
	    {Segment, Before1} = extract_segment(Before, []),
	    %% Segment is the list of things that can be
	    %% shifted into the After space
	    %% Segment is in normal order
	    %% Before1 *begins* with a space or is []
	    %% dbg_io("Segment=~p~n",[Segment]),
	    Ps = make_partitions(Segment, Rules),
	    %% dbg_io("partitions=~p~n",[Ps]),
	    %% Ps = [{Bs,As}]
	    %% Now the final {B,A} pairs
	    %% are formed from
	    %% {Bs++Before1, As++After}
	    %% 
	    L1 = lists:map(fun({Bs,As}) ->
				   {lists:reverse(lists:reverse(Bs)++Before1), 
				    As++After}
			   end, Ps),
	    %% After begun with a space which we remove in
	    %% the final list
	    L2 = [{lists:reverse(Before), 
		   remove_leading_spaces(After)}|L1] ++ L0,
	    After1 = lists:reverse(Segment, After),
	    case Before1 of
		[] -> L2;
		[Space|Before2] ->
		    break_before(Before2, [Space|After1], Len, Q, L2, Rules)
	    end
    end.

check_before_invarient(Before=[H|_]) ->
    case eg_richText:is_space(H) of
	true ->
	    dbg_io("Invarient broken before begines with a space:"
		      "~p~n",[Before]),
            ok;
	false ->
	    true
    end.

check_after_invarient([]) -> true;
check_after_invarient(After=[H|_]) ->
    case eg_richText:is_space(H) of
	true -> true;
	false -> dbg_io("invarient broken after begins:~p~n",[After])
    end.	    
    
%% ----------------------------------------------------------------------------
%% make_partitions(M, Rules) -> [{Before, After}]
%%    M is a list of inlines that does not contain a space or a newline
%%    it must be split by hyphenating all words in M in all possible
%%    ways. M is in normal order.
%%    Rules is a the module name of the eg_hyphen_rules_*.erl file to use
%%    to determine how to hyphenate

make_partitions(M, Rules) ->  make_partitions(M, Rules, [], []).

make_partitions([H|T], Rules, Before, Final) ->
    case eg_richText:is_word(H) of
	true ->
	    Ps = all_hyphenations(H, Rules),
	    L1 = lists:map(fun({X1,X2}) ->
				   {lists:reverse([X1|Before]), [X2|T]}
			   end, Ps),
	    make_partitions(T, Rules, [H|Before], L1 ++ Final);
	false ->
	    make_partitions(T, Rules, [H|Before], Final)
    end;
make_partitions([], Rules, _, Final) ->
    Final.

%% extract segment x y S a b c -> {[x,y], [S,a,b,c]}
 
extract_segment(A=[H|T], L) ->
    case eg_richText:is_space(H) of
	true ->
	    {lists:reverse(L), A};
	false ->
	    extract_segment(T, [H|L])
    end;
extract_segment([], L) ->
    {lists:reverse(L), []}.

%% all_hyphenations(Word, Rules) -> [{Word1, Word2}].

all_hyphenations(Word, Rules) ->
    %% extract the string from the word
    Str = eg_richText:string(Word),
    {Str1, Tail} = remote_trailing_stuff(Str, []),
    Ps = eg_hyphenate:partitions(Str1, Rules),
    lists:map(fun({A,B}) ->
		      StrA = A ++ "-",
		      StrB = B ++ Tail,
		      Word1 = eg_richText:cloan_word(Word, StrA),
		      Word2 = eg_richText:cloan_word(Word, StrB),
		      {Word1, Word2}
	      end, Ps).

remote_trailing_stuff([H|T], L) when H >= $a, H =< $z ->
    remote_trailing_stuff(T, [H|L]);
remote_trailing_stuff([H|T], L) when H >= $A, H =< $Z ->
    remote_trailing_stuff(T, [H|L]);
remote_trailing_stuff(X, L) ->
    {lists:reverse(L), X}.

%% The badness of the line
%% ActualWidth = size of line with zero spaces
%% Len         = Required length of the line
%% NBlanks     = #places to distribute the blanks 
%%
%% To compute the badness of a line
%%   we compute the actual length (including blanks)
%%   and the size of the blanks themselves


badness(Line, Len0, Q) ->
    %% dbg_io("Badness of:~s~n",[r2s(Line)]),
    %% DataWidth = width of line (not including blanks)
    DataWidth= eg_richText:widthExcludingSpaces(Line),
    Len = Len0 * 1000,
    %% dbg_io("Data Width=~p Len=~p Str=~s~n",[DataWidth, Len,r2s(Line)]),
    B = if 
	    Len - DataWidth < 1 ->
		%% It won't fit at all this is a catastrophy
		%% give a large negative badness
		-1000000;
	    true ->
		%% It will fit
		%% We compute how much to stretch or shrink
		%% the blanks in the line
		%% Len          = required width
		%% DataWidth    = width of data alone, i.e. no blanks
		Width = spaces_width(Line),
		%% Width   = The "natural width" of all the spaces
		if Width < 1 ->
			-1000000;
		   true ->
			R = (Len - DataWidth)/Width,
			%% dbg_io("R=~p~n",[R]),
			badness(R, Q)
		end
	end,
    %% dbg_io("Badness=~p~n",[B]),
    B.
	    
%% R = strechyness of blank
badness(R, Q) ->
    Max = Q#q.maxSpaceStretch/100,
    Min = Q#q.minSpaceStretch/100,
    B = if 
	    R < 0.1 ->
		-100000;
	    R < Min ->
		Alpha = -100000/(Min*Min*Min*Min),
		Beta = -1,
		T = (Min-R),
		Alpha*T*T*T*T + Beta;
	   true ->
		%% R > 1
		T = (R - 1)/(Max - 1),
		T*T
	end,
    %% dbg_io("Nominal size of blank=~p actual=~p Min=~p Max=~p B=~p~n",
    %% [Nominal,Need, Min, Max, B]),
    B.

%% Toks = [tok()]
%%   tok() = {sp2,Index,W} | {wd2,Index,W,Str}

%% spaces_info(inline()) ->
%%   {NBlanks, Width}

spaces_width(Toks) ->
    Toks1 = lists:filter(fun(I) -> eg_richText:is_space(I) end, Toks),
    eg_richText:lineWidth(Toks1).

