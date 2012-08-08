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

-module(eg_afm).

-record(afm2, {baseFont,    % FontName
	       firstChar,   % first char in widths table
	       lastChar,    % last char in Widths table
	       widths,      % the widths table (0=undefined)
	       kernPairs,   % [{{C1,C2},K}]
	       index,       % int
	       fixedPitch,  % bool
	       encoding,    % string
	       type,        % internal, external
	       ascender,    % num
	       capHeight,   % num
	       descender,   % num
	       flags,       % num
	       fontBBox,    % {int,int,int,int}
	       italicAngle, % num 
	       stemV,       % num
	       xHeight}).   % num

%% Version 3 ...
%% Parse an afm file and record the following information

%% We need the following parameters. Most (but not all) 
%% of these can be found by
%% parsing the .fm
%% 
%% BaseFont    = The name of the Font (This is the exact name)
%% FirstChar   = First Char in the widths table (derived from the widths)
%% LastChar    = Last  Char in the widths table 
%% Ascender    = value of Ascender  in .afm file
%% CapHeight   = value of CapHeight in .afm file
%% Descender   = value of Descender in .afm fil},
%% Flags       = see notes
%% FontBBox    = value of FontBox in .afm (four integers)
%% ItalicAngle = value of ItalicAngle in .afm
%% StemV       = see notes 
%% XHeight     = value of XHeight in .afm
%% 
%% Special parameters.
%% 

%% StemV is NOT in the .afm file but *is* in the
%% encrypted part of the .pfb file where it is set with a command like
%% /StdVW[50]def - this is the dominant width of the vertical stems
%% (see page 42 of the Adobe "black book")
%% 
%% The  entry  StdVW  is an  array  with  only  one real  number  entry
%% expressing   the   dominant   width   of  vertical   stems   (measured
%% horizontally in  character space units). Typically, this  will be the
%% width of  straight stems  in lower case  letters. (For an  italic font
%% program,  give the width  of the  vertical stem  measured at  an angle
%% perpendicular to the stem direction.) For example: /StdVW [85] def

%% Page 42 of the Black book
%% Flags (sanserif/serif, fixed-pitch, symbolic, script, all-cap, 
%%       small-cap etc)
%% You can ask Acrobat Distiller in such cases by having it distill a 
%% simple PS file like below, with compression turned off, 
%% and then examining the PDF file
%% 
%% 	    %!PS-Adobe-2.0
%% 	    /Palatino-Roman findfont 16.0 scalefont setfont
%% 	    40 700 moveto (Palatino-Roman 16.0 point) show
%% 	    showpage


%% afm parser (quick and dirty)
%% Just look for the stuff I'm interested in and assume a fixed format for 
%% input


-export([make/0]).

-include_lib("kernel/include/file.hrl").    

%% ============================================================================

make() ->
    F = all_afms(),
    {F1,_} = lists:mapfoldl(fun(File,I) ->
			      {{File,I}, I+1}
		      end, 1, F),
    io:format("Found:~p~n",[F1]),
    F2 = lists:map(fun({File, Index}) -> 
		     {Mod,FontName,Type} = parse(File, Index),
		     {File,Mod,FontName,Index,Type}
	     end, F1),
    mk_Make(lists:map(fun(I) -> element(2,I) end, F2)),
    mk_eg_font_map(lists:map(fun({_,M,Func,I,_}) -> {M,Func,I} end, F2)),
    External = [{File,Mod} || {File,Mod,_,_,external} <- F2],
    io:format("External=~p~n",[External]),
    lists:foreach(fun({File,Mod}) -> copy_pfb(File,Mod) end, External).

%% Just run pdf_afm_qdh:all() to build the font tables
all_afms() ->
    {ok, F} = file:open(filename:join(eg_lib:priv_dir(), "font_locations"),
                        [read]),
    L= read_locations(F, []),
    file:close(F),
    find_atms(L, []).

find_atms([H|T], L) ->
    case is_dir(H) of
	true ->
	    A = eg_lib:find_files(H, "*.afm", false),
	    find_atms(T, A ++ L);
	false ->
	    case exists(H) of
		true ->
		    find_atms(T, [H|L]);
		false ->
		    io:format("~s is not a file~n",[H]),
		    find_atms(T, L)
	    end
    end;
find_atms([], L) ->
    L.

read_locations(F, L) ->
    case io:get_line(F, '>') of
	eof ->
	    L;
	Str ->
	    read_locations(F, [first(Str)|L])
    end.


file_type(File) ->
    case file:read_file_info(File) of
	{ok, Info} -> Info#file_info.type;
	_ -> error
    end.


is_dir(X) ->
    file_type(X) == directory.


exists(File) ->
    case file:read_file_info(File) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

first([X]) ->
    [];
first([H|T]) ->
    [H|first(T)].

mk_Make(FontMap) ->
    Str = ["MODULES = eg_font_map ",
	   lists:map(fun(J) ->[J," "] end, FontMap),"\n\n",
	   "OBJECTS=$(MODULES:%=../../ebin/%.beam)\n",
	   "all: $(OBJECTS)\n\n",
	   "../../ebin/%.beam: %.erl\n",
	   "\terlc +debug_info -o ../../ebin $<\n\n",
	   "clean:\n",
	   "\trm -f $(OBJECTS) Makefile *.erl *.pfb\n\n"
	  ],
    %% io:format("Makefile(~s)=~s~n",[Makefile,Str]),
    file:write_file(filename:join(eg_lib:priv_src_dir(), "Makefile"), [Str]).

%% [{Mod,Fname,Index}]
mk_eg_font_map(FontMap) ->
    All = lists:map(fun(I) -> element(2, I) end, FontMap),
    Str = ["-module(eg_font_map).\n",
	   "-export([handler/1,all_fonts/0]).\n",
	   lists:map(fun({Mod,Font,I}) ->
		       ["handler(\"",Font,"\")-> ", Mod,";\n"]
	       end, FontMap),
	   "handler(_) -> undefined.\n",
	   "all_fonts() -> ",io_lib:format("~p.~n", [All])],
    file:write_file(filename:join(eg_lib:priv_src_dir(), "eg_font_map.erl"),
                    [Str]).

parse(F,Index) ->
    %% io:format("Parsing:~p~n",[F]),
    Mod = "eg_font_" ++ eg_pdf_op:i2s(Index),
    Out = filename:join(eg_lib:priv_src_dir(), Mod ++ ".erl"),
    L = file2numbered_lines(F),
    Fn   = get_font_name(L),
    io:format("Found font:~s~n",[Fn]),
    Type = case lists:member(Fn, eg_pdf:inBuiltFonts()) of
	       true -> internal;
	       false ->  external
	   end,
    Cw = get_char_widths(L, get_encoding(L)),
    Kern = get_kerning_info(L, Cw),
    Cw1  = lists:map(fun({Index1,Width,_Name}) ->
		      {Index1, Width}
	      end, Cw),
    %% io:format("Cw1=~p~n",[Cw1]),
    {First,Last,Widths} = normalise_widths(Cw1),
    %% io:format("First=~p last=~p ~n",[First,Last]),
    Kern1 = lists:map(fun({XY,_,W}) -> {XY, W} end, Kern),
    Ascender = get_val(L, "Ascender"),
    T = #afm2{baseFont=Fn, widths=Widths, firstChar=First,
	      lastChar=Last, kernPairs=Kern1,
	      ascender=get_val(L, "Ascender"),
	      capHeight=get_val(L, "CapHeight"),
	      index=Index,
	      type=Type,
	      fixedPitch=get_fixed_pitch(L),
	      encoding=get_encoding(L), 
	      descender=get_val(L, "Descender"),
	      italicAngle=get_val(L, "ItalicAngle"),
	      xHeight=get_val(L,"XHeight"),
	      flags=flags(Fn),
	      stemV=stemV(Fn),
	      fontBBox=get_fontBBox(L)},
    file:write_file(Out, mk_program(Mod, T)),
    %% io:format("Font ~s ~w entries ~w entries in kerning table~n",
    %% [Fn, length(Cw1), length(Kern1)]),
    {Mod, Fn, Type}.


copy_pfb(File, Mod) ->
    Root = filename:rootname(File),
    From = Root ++ ".pfb",
    To = filename:join(eg_lib:priv_src_dir(), Mod ++ ".pfb"),
    io:format("Copy From:~s To:~s~n",[From,To]),
    case file:read_file(From) of
	{ok, Bin} ->
	    file:write_file(To, Bin);
	{error,_} ->
	    io:format("fatal error non existent file:~s~n",[From]),
	    exit(enofile)
    end.

mk_program(Mod, T) ->
    %% io:format("Mod=~p T=~p~n",[Mod,T]),
    ["-module(", Mod, ").\n",
     "-export([width/1, kern/2, fontName/0, firstChar/0,lastChar/0]).\n",
     "-export([index/0,ascender/0,capHeight/0,descender/0,italicAngle/0]).\n", 
     "-export([xHeight/0, flags/0, type/0, stemV/0,fontBBox/0,widths/0]).\n",
     "-export([encoding/0]).\n",
     "fontName() -> \"", T#afm2.baseFont,"\".\n",
     "index() -> ",      eg_pdf_op:n2s(T#afm2.index),".\n",
     "type() -> ",       eg_pdf_op:a2s(T#afm2.type),".\n",
     "encoding() -> \"", T#afm2.encoding,"\".\n",
     "firstChar() ->",   eg_pdf_op:n2s(T#afm2.firstChar),".\n",
     "lastChar() ->",    eg_pdf_op:n2s(T#afm2.lastChar),".\n",
     "ascender() ->",    eg_pdf_op:n2s(T#afm2.ascender),".\n",
     "capHeight() ->",   eg_pdf_op:n2s(T#afm2.capHeight),".\n",
     "descender() ->",   eg_pdf_op:n2s(T#afm2.descender),".\n",
     "italicAngle() ->", eg_pdf_op:n2s(T#afm2.italicAngle),".\n",
     "xHeight() ->",     eg_pdf_op:n2s(T#afm2.xHeight),".\n",
     "flags() ->",       eg_pdf_op:n2s(make_flags(T)),".\n",
     "stemV() ->",       eg_pdf_op:n2s(T#afm2.stemV),".\n",
     "fontBBox() ->",    io_lib:format("~p.\n", [T#afm2.fontBBox]),
     "widths() ->",      io_lib:format("~p.\n", [T#afm2.widths]),
     widths_2_erl(T#afm2.firstChar, T#afm2.widths),
     mk_kern(T#afm2.kernPairs)].

mk_kern([{{I,J},K}|T]) ->
    ["kern(",eg_pdf_op:n2s(I),",",eg_pdf_op:n2s(J),")->",
     eg_pdf_op:n2s(K),";\n"
     | mk_kern(T)];
mk_kern([]) ->
    ["kern(_,_) -> 0.\n"].

widths_2_erl(N, []) ->
    ["width(_)->unknown.\n"];
widths_2_erl(N, [0|T]) ->
    widths_2_erl(N+1, T);
widths_2_erl(N, [H|T]) ->
    ["width(",eg_pdf_op:n2s(N),")->",eg_pdf_op:n2s(H),";\n"
     | widths_2_erl(N+1, T)].


normalise_widths(Pairs) ->
    P1 = lists:sort(Pairs),
    {First,Width} = hd(P1),
    {Last, Ws} = gather(First, P1, []),
    {First,Last, Ws}.

gather(X, [{X,W}], L) ->
    {X, lists:reverse([W|L])};
gather(X, [{X,W}|T], L) ->
    gather(X+1, T, [W|L]);
gather(X, Z=[{Y,W}|T], L) when Y > X ->
    gather(X+1,Z,[0|L]).

get_fixed_pitch(L) ->
    T = get_keyword(L, "IsFixedPitch "),
    [F1|_] = string:tokens(T, "\s\r\n"),
    case F1 of
	"true" ->
	    true;
	"false" ->
	    false
    end.

get_encoding(L) ->
    T = get_keyword(L, "EncodingScheme "),
    [F1|_] = string:tokens(T, "\s\r\n"),
    F1.

get_fontBBox(L) ->
    T = get_keyword(L, "FontBBox "),
    [F1,F2,F3,F4|_] = string:tokens(T, "\s\r\n"),
    {list_to_integer(F1),list_to_integer(F2),list_to_integer(F3),
     list_to_integer(F4)}.

get_font_name(L) ->
    T = get_keyword(L, "FontName "),
    [F|_] = string:tokens(T, "\s\r\n"),
    F.

get_val(L, Tag) ->
    T = get_keyword(L, Tag ++" "),
    [F|_] = string:tokens(T, "\s\r\n"),
    %% io:format("Tag=~s T=~s F=~s~n",[Tag, T, F]),
    case lists:member($., F) of
	true ->
	    list_to_float(F);
	false ->
	    list_to_integer(F)
    end.

get_keyword([{_,Str}|T], Prefix) ->
    case is_prefix(Prefix, Str) of
	{yes, T1} ->
	    T1;
	no ->
	    get_keyword(T, Prefix)
    end;
get_keyword([], Prefix) ->
    io:format("Cannot find:~s setting to 0~n",  [Prefix]),
    "0".

is_prefix([], L) ->
    {yes, L};
is_prefix([H|T], [H|T1]) ->
    is_prefix(T, T1);
is_prefix(_, _) ->
    no.
    
get_kerning_info(L, C) ->
    lists:foldl(fun(I, Acc) ->
		  case parse_kerning(I, C) of
		      {ok, Add} ->
			  [Add|Acc];
		      no ->
			  Acc
		  end
	  end, [], L).

parse_kerning({_,Str="KPX " ++ _}, Cw) ->
    case string:tokens(Str, "\s\r\n") of
	["KPX",C1,C2,W] ->
	    case charno(C1, Cw) of
		{ok, N1} ->
		    case charno(C2, Cw) of
			{ok, N2} ->
			    {ok, {{N1,N2}, {C1, C2}, list_to_integer(W)}};
			_ ->
			    no
		    end;
		_ ->
		    no
	    end;
	Other ->
	    io:format("UUgh:~s:~p~n",[Str,Other]),
	    no
    end;
parse_kerning(_, Cw) ->
    no.

charno(C, Cw) ->
    case [I || {I,_,Name} <- Cw, Name == C] of
	[N|_] ->
	    {ok, N};
	_ ->
	    io:format("Cannot locate character:~p~n",[C]),
	    error
    end.

get_char_widths(L, Enc) ->
    F = fun(X,Acc) ->
		add_char(X, Acc, Enc)
	end,
    lists:foldl(F, [], L).

add_char({Line,Str= "C " ++ _}, Acc, Enc) ->
    case parse_char_data(Str, Enc) of
	{-1,_,_} ->
	    Acc;
	V ->
	    [V|Acc]
    end;
add_char(_, Acc, _Enc) ->
    Acc.

parse_char_data(S, "FontSpecific") ->
    case string:tokens(S, "\s\r\n") of
	["C",C0,";","WX",W,";","N",Name,";"|_] ->
	    C = list_to_integer(C0),
	    {C, list_to_integer(W), Name};
	Other ->
	    io:format("wot is:~s:~pn",[S, Other])
    end;
parse_char_data(S, Enc) ->
    case string:tokens(S, "\s\r\n") of
	["C",_,";","WX",W,";","N",Name,";"|_] ->
	    C = name_to_char(Name),
	    {C, list_to_integer(W),Name};
	Other ->
	    io:format("wot is:~s:~pn",[S, Other])
    end.

%% FontDescriptor Flags 
%% Bit       32-20        19       18
%% Meaning  reserved  force bold  small ..... not completed
%%                 when small   cap
%% (Note: MSB is bit-32, LSB is bit-1)

flags("Helvetica") -> 32;
flags("Helvetica-Bold") -> 0;
flags("Helvetica-Oblique") -> 96;
flags("Helvetica-BoldOblique") -> 0;
flags("Times-Roman") -> 0;
flags("Times-Bold") -> 0;
flags("Times-Italic") -> 0;
flags("Times-BoldItalic") -> 0;
flags("Courier") -> 0;
flags("Courier-Bold") -> 0;
flags("Courier-Oblique") -> 0;
flags("Courier-BoldOblique") -> 0;
flags("Symbol") -> 0;
flags("ZapfDingbats") -> 0;
flags("AvantGarde-Book") -> 32;
flags("AvantGarde-BookOblique") -> 96;
flags("AvantGarde-Demi") -> 262176;
flags("AvantGarde-DemiOblique") -> 262240;
flags("Bookman-Demi") -> 262178;
flags("Bookman-DemiItalic") -> 262242;
flags("Bookman-Light") -> 34;
flags("Bookman-LightItalic") -> 98;
flags("Helvetica-Narrow") -> 32;
flags("Helvetica-Narrow-Oblique") -> 96;
flags("Helvetica-Narrow-Bold") -> 262176;
flags("Helvetica-Narrow-BoldOblique") -> 262240;
flags("NewCenturySchlbk-Roman") -> 34;
flags("NewCenturySchlbk-Italic") -> 98;
flags("NewCenturySchlbk-Bold") -> 262178;
flags("NewCenturySchlbk-BoldItalic") -> 262242;
flags("Palatino-Roman") -> 34;
flags("Palatino-Italic") -> 98;
flags("Palatino-Bold") -> 262178;
flags("Palatino-BoldItalic") -> 262242;
flags("ZapfChancery-MediumItalic") -> 98;
flags("Helvetica-Condensed") -> 32;
flags("Helvetica-Condensed-Bold") -> 262176;
flags("Helvetica-Condensed-Oblique") -> 96;
flags("Helvetica-Condensed-BoldObl") -> 262240;
flags(_) -> 0.

%% stemV
stemV("Helvetica") -> 0;
stemV("Helvetica-Bold") -> 0;
stemV("Helvetica-Oblique") -> 0;
stemV("Helvetica-BoldOblique") -> 0;
stemV("Times-Roman") -> 0;
stemV("Times-Bold") -> 0;
stemV("Times-Italic") -> 0;
stemV("Times-BoldItalic") -> 0;
stemV("Courier") -> 0;
stemV("Courier-Bold") -> 0;
stemV("Courier-Oblique") -> 0;
stemV("Courier-BoldOblique") -> 0;
stemV("Symbol") -> 0;
stemV("ZapfDingbats") -> 0;
stemV("AvantGarde-Book") -> 70;
stemV("AvantGarde-BookOblique") -> 70;
stemV("AvantGarde-Demi") -> 133;
stemV("AvantGarde-DemiOblique") -> 133;
stemV("Bookman-Demi") -> 167;
stemV("Bookman-DemiItalic") -> 172;
stemV("Bookman-Light") -> 96;
stemV("Bookman-LightItalic") -> 96;
stemV("Helvetica-Narrow") -> 88;
stemV("Helvetica-Narrow-Oblique") -> 88;
stemV("Helvetica-Narrow-Bold") -> 140;
stemV("Helvetica-Narrow-BoldOblique") -> 140;
stemV("NewCenturySchlbk-Roman") -> 92;
stemV("NewCenturySchlbk-Italic") -> 80;
stemV("NewCenturySchlbk-Bold") -> 154;
stemV("NewCenturySchlbk-BoldItalic") -> 150;
stemV("Palatino-Roman") -> 84;
stemV("Palatino-Italic") -> 84;
stemV("Palatino-Bold") -> 122;
stemV("Palatino-BoldItalic") -> 122;
stemV("ZapfChancery-MediumItalic") -> 70;
stemV("Helvetica-Condensed") -> 79;
stemV("Helvetica-Condensed-Bold") -> 130;
stemV("Helvetica-Condensed-Oblique") -> 79;
stemV("Helvetica-Condensed-BoldObl") -> 130;
stemV(X) -> io:format("No Vstem for:~p~n", [X]),
    70.

%% mk_widths(M) ->
%%     First = M#afm2.firstChar,
%%     W     = M#afm2.widths,
%%     mk_widths(First, W).
%% 
%% mk_widths(N, [])    -> [];
%% mk_widths(N, [0|T]) -> mk_widths(N+1,T);
%% mk_widths(N, [W|T]) -> [{N,W}|mk_widths(N+1,T)].

-define(FIXEDPITCH, (1 bsl 0)).
-define(ADOBESTANDARD, (1 bsl 5)).
-define(SYMBOL, (1 bsl 2)).
-define(ITALIC, (1 bsl 6)).

make_flags(T) ->    
    io:format("*** Make flags:~n"),
    Font = T#afm2.baseFont,
    Fixed = T#afm2.fixedPitch,
    Enc = T#afm2.encoding,
    I = T#afm2.italicAngle, 
    io:format("Font=~s Fixed=~p Enc=~p Ital=~p~n",[Font,Fixed,Enc,I]),
    %% flags(Font).
    Flags0 = 0,
    Flags1 = case  Fixed of
		 true ->
		     Flags0 bor ?FIXEDPITCH;
		 false ->
		     Flags0
	     end,
    Flags2 = case Enc of
		 "AdobeStandardEncoding" -> 
		     (Flags1 bor ?ADOBESTANDARD); 
		 "AppleStandard" -> 
		     (Flags1 bor ?ADOBESTANDARD);
		 "FontSpecific" ->
		     (Flags1 bor ?SYMBOL);
		 _ ->
		     io:format("????????Encoding=~p~n",[Enc]),
		     (Flags1 bor ?ADOBESTANDARD)
	     end,
    Flags3 = if
		 I < 0 ->
		     (Flags2 bor ?ITALIC);
		 true ->
		     Flags2
	     end,
    io:format("Flags=~p~n",[Flags3]),
    Flags3.

file2numbered_lines(File) ->
    {ok, B} = file:read_file(File),
    str2lines(binary_to_list(B)).
    
str2lines(L) -> str2lines(L, 1, [], []).
 
str2lines([H|T], Line, C, L) ->
    case H of
        $\n -> str2lines(T, Line+1,[],[{Line,lists:reverse([$\n|C])}|L]);
        _   -> str2lines(T, Line,  [H|C], L)
    end;
str2lines([], Line, [], L) ->
    lists:reverse(L);
str2lines([], Line, C, L) ->
    lists:reverse([{Line,lists:reverse(C)}|L]).


%% AdobeStandardEncoding

name_to_char("space") ->  32 ;
name_to_char("exclam") -> 33 ;
name_to_char("quotedbl") -> 34 ;
name_to_char("numbersign") -> 35 ;
name_to_char("dollar") -> 36 ;
name_to_char("percent") -> 37 ;
name_to_char("ampersand") -> 38 ;
name_to_char("quotesingle") -> 39 ;
name_to_char("parenleft") -> 40 ;
name_to_char("parenright") -> 41 ;
name_to_char("asterisk") -> 42 ;
name_to_char("plus") -> 43 ;
name_to_char("comma") -> 44 ;
name_to_char("hyphen") -> 45 ;
name_to_char("period") -> 46 ;
name_to_char("slash") -> 47 ;
name_to_char("zero") -> 48 ;
name_to_char("one") -> 49 ;
name_to_char("two") -> 50 ;
name_to_char("three") -> 51 ;
name_to_char("four") -> 52 ;
name_to_char("five") -> 53 ;
name_to_char("six") -> 54 ;
name_to_char("seven") -> 55 ;
name_to_char("eight") -> 56 ;
name_to_char("nine") -> 57 ;
name_to_char("colon") -> 58 ;
name_to_char("semicolon") -> 59 ;
name_to_char("less") -> 60 ;
name_to_char("equal") -> 61 ;
name_to_char("greater") -> 62 ;
name_to_char("question") -> 63 ;
name_to_char("at") -> 64 ;
name_to_char("A") -> 65 ;
name_to_char("B") -> 66 ;
name_to_char("C") -> 67 ;
name_to_char("D") -> 68 ;
name_to_char("E") -> 69 ;
name_to_char("F") -> 70 ;
name_to_char("G") -> 71 ;
name_to_char("H") -> 72 ;
name_to_char("I") -> 73 ;
name_to_char("J") -> 74 ;
name_to_char("K") -> 75 ;
name_to_char("L") -> 76 ;
name_to_char("M") -> 77 ;
name_to_char("N") -> 78 ;
name_to_char("O") -> 79 ;
name_to_char("P") -> 80 ;
name_to_char("Q") -> 81 ;
name_to_char("R") -> 82 ;
name_to_char("S") -> 83 ;
name_to_char("T") -> 84 ;
name_to_char("U") -> 85 ;
name_to_char("V") -> 86 ;
name_to_char("W") -> 87 ;
name_to_char("X") -> 88 ;
name_to_char("Y") -> 89 ;
name_to_char("Z") -> 90 ;
name_to_char("bracketleft") -> 91 ;
name_to_char("backslash") -> 92 ;
name_to_char("bracketright") -> 93 ;
name_to_char("asciicircum") -> 94 ;
name_to_char("underscore") -> 95 ;
name_to_char("grave") -> 96 ;
name_to_char("a") -> 97 ;
name_to_char("b") -> 98 ;
name_to_char("c") -> 99 ;
name_to_char("d") -> 100 ;
name_to_char("e") -> 101 ;
name_to_char("f") -> 102 ;
name_to_char("g") -> 103 ;
name_to_char("h") -> 104 ;
name_to_char("i") -> 105 ;
name_to_char("j") -> 106 ;
name_to_char("k") -> 107 ;
name_to_char("l") -> 108 ;
name_to_char("m") -> 109 ;
name_to_char("n") -> 110 ;
name_to_char("o") -> 111 ;
name_to_char("p") -> 112 ;
name_to_char("q") -> 113 ;
name_to_char("r") -> 114 ;
name_to_char("s") -> 115 ;
name_to_char("t") -> 116 ;
name_to_char("u") -> 117 ;
name_to_char("v") -> 118 ;
name_to_char("w") -> 119 ;
name_to_char("x") -> 120 ;
name_to_char("y") -> 121 ;
name_to_char("z") -> 122 ;
name_to_char("braceleft") -> 123 ;
name_to_char("bar") -> 124 ;
name_to_char("braceright") -> 125 ;
name_to_char("asciitilde") -> 126 ;
name_to_char("Euro") -> 128 ;
name_to_char("quotesinglbase") -> 130 ;
name_to_char("florin") -> 131 ;
name_to_char("quotedblbase") -> 132 ;
name_to_char("ellipsis") -> 133 ;
name_to_char("dagger") -> 134 ;
name_to_char("daggerdbl") -> 135 ;
name_to_char("circumflex") -> 136 ;
name_to_char("perthousand") -> 137 ;
name_to_char("Scaron") -> 138 ;
name_to_char("guilsinglleft") -> 139 ;
name_to_char("OE") -> 140 ;
name_to_char("Zcaron") -> 142 ;
name_to_char("quoteleft") -> 145 ;
name_to_char("quoteright") -> 146 ;
name_to_char("quotedblleft") -> 147 ;
name_to_char("quotedblright") -> 148 ;
name_to_char("bullet") -> 149 ;
name_to_char("endash") -> 150 ;
name_to_char("emdash") -> 151 ;
name_to_char("tilde") -> 152 ;
name_to_char("trademark") -> 153 ;
name_to_char("scaron") -> 154 ;
name_to_char("guilsinglright") -> 155 ;
name_to_char("oe") -> 156 ;
name_to_char("zcaron") -> 158 ;
name_to_char("Ydieresis") -> 159 ;
name_to_char("nbspace") -> 160 ;
name_to_char("exclamdown") -> 161 ;
name_to_char("cent") -> 162 ;
name_to_char("sterling") -> 163 ;
name_to_char("currency") -> 164 ;
name_to_char("yen") -> 165 ;
name_to_char("brokenbar") -> 166 ;
name_to_char("section") -> 167 ;
name_to_char("dieresis") -> 168 ;
name_to_char("copyright") -> 169 ;
name_to_char("ordfeminine") -> 170 ;
name_to_char("guillemotleft") -> 171 ;
name_to_char("logicalnot") -> 172 ;
name_to_char("minus") -> 173 ;
name_to_char("registered") -> 174 ;
name_to_char("macron") -> 175 ;
name_to_char("degree") -> 176 ;
name_to_char("plusminus") -> 177 ;
name_to_char("twosuperior") -> 178 ;
name_to_char("threesuperior") -> 179 ;
name_to_char("acute") -> 180 ;
name_to_char("mu") -> 181 ;
name_to_char("paragraph") -> 182 ;
name_to_char("periodcentered") -> 183 ;
name_to_char("cedilla") -> 184 ;
name_to_char("onesuperior") -> 185 ;
name_to_char("ordmasculine") -> 186 ;
name_to_char("guillemotright") -> 187 ;
name_to_char("onequarter") -> 188 ;
name_to_char("onehalf") -> 189 ;
name_to_char("threequarters") -> 190 ;
name_to_char("questiondown") -> 191 ;
name_to_char("Agrave") -> 192 ;
name_to_char("Aacute") -> 193 ;
name_to_char("Acircumflex") -> 194 ;
name_to_char("Atilde") -> 195 ;
name_to_char("Adieresis") -> 196 ;
name_to_char("Aring") -> 197 ;
name_to_char("AE") -> 198 ;
name_to_char("Ccedilla") -> 199 ;
name_to_char("Egrave") -> 200 ;
name_to_char("Eacute") -> 201 ;
name_to_char("Ecircumflex") -> 202 ;
name_to_char("Edieresis") -> 203 ;
name_to_char("Igrave") -> 204 ;
name_to_char("Iacute") -> 205 ;
name_to_char("Icircumflex") -> 206 ;
name_to_char("Idieresis") -> 207 ;
name_to_char("Eth") -> 208 ;
name_to_char("Ntilde") -> 209 ;
name_to_char("Ograve") -> 210 ;
name_to_char("Oacute") -> 211 ;
name_to_char("Ocircumflex") -> 212 ;
name_to_char("Otilde") -> 213 ;
name_to_char("Odieresis") -> 214 ;
name_to_char("multiply") -> 215 ;
name_to_char("Oslash") -> 216 ;
name_to_char("Ugrave") -> 217 ;
name_to_char("Uacute") -> 218 ;
name_to_char("Ucircumflex") -> 219 ;
name_to_char("Udieresis") -> 220 ;
name_to_char("Yacute") -> 221 ;
name_to_char("Thorn") -> 222 ;
name_to_char("germandbls") -> 223 ;
name_to_char("agrave") -> 224 ;
name_to_char("aacute") -> 225 ;
name_to_char("acircumflex") -> 226 ;
name_to_char("atilde") -> 227 ;
name_to_char("adieresis") -> 228 ;
name_to_char("aring") -> 229 ;
name_to_char("ae") -> 230 ;
name_to_char("ccedilla") -> 231 ;
name_to_char("egrave") -> 232 ;
name_to_char("eacute") -> 233 ;
name_to_char("ecircumflex") -> 234 ;
name_to_char("edieresis") -> 235 ;
name_to_char("igrave") -> 236 ;
name_to_char("iacute") -> 237 ;
name_to_char("icircumflex") -> 238 ;
name_to_char("idieresis") -> 239 ;
name_to_char("eth") -> 240 ;
name_to_char("ntilde") -> 241 ;
name_to_char("ograve") -> 242 ;
name_to_char("oacute") -> 243 ;
name_to_char("ocircumflex") -> 244 ;
name_to_char("otilde") -> 245 ;
name_to_char("odieresis") -> 246 ;
name_to_char("divide") -> 247 ;
name_to_char("oslash") -> 248 ;
name_to_char("ugrave") -> 249 ;
name_to_char("uacute") -> 250 ;
name_to_char("ucircumflex") -> 251 ;
name_to_char("udieresis") -> 252 ;
name_to_char("yacute") -> 253 ;
name_to_char("thorn") -> 254 ;
name_to_char("ydieresis") -> 255 ;
name_to_char(_) -> -1.
     
