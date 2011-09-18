%%==========================================================================
%% Copyright (C) 2003 Mikael Karlsson
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
%% Authors:   Mikael Karlsson <mikael.karlsson@creado.com>
%%            Joe Armstrong <joe@sics.se>
%% Purpose: PDF objects api
%%==========================================================================

%%
%% @doc PDF Objects.
%%
%% <p>Purpose: Create and manipulate PDF Objects </p>
%% <p> </p>
%% 
%% @end

-module(eg_pdf_obj).

-export([catalogue/2, info/1, destination/2, fonts/2]).

%% ============================================================================

%% @spec catalogue(PagesRef::integer(), Options) -> dict() 
%% Options = [Option]
%% Option = {'"Version"', name()} | {'"PageLabels"', number_tree()} | 
%% {'"Names"', dict()}
%% Ref = integer()
%% @doc Creates a Catalogue dictionary.
%% @see eg_pdf_lib
catalogue(PagesRef, Options) ->
    {dict,[{"Type",{name,"Catalog"}},
	   {"Pages",{ptr, PagesRef,0}}|
	   lists:map( fun catalogue_opt/1, Options)]}.
catalogue_opt({"Version" = A, {name, _}} = B) ->
    {A, B};
catalogue_opt({"PageLabels" = A, {number_tree, _ } = B}) ->
    {A, B};
catalogue_opt({"Names" = A, {dict, _ } = B}) ->
    {A, B}.


%% @spec info(Options) -> dict()
%% Options = [Option]
%% Option = {'"Creator"', string()} | {'"CreationDate"', Date} | 
%% {'"Producer"', string()} | {'"Author"', string()} | {'"Title"', string()} | 
%% {'"Subject"', string()} | {'"Keywords"', string()}
%% @doc Creates a PDF Info dictionary.
info(Options) ->
    {dict, lists:map( fun info_opt/1, Options)}.

info_opt({"Creator" = A, B}) ->
    { A, {string, B}};
info_opt({"CreationDate" = A, B }) ->
    { A, {date, B}};
info_opt({"Producer" = A, B}) ->
    { A, {string, B}};
info_opt({"Author"= A, B}) ->
    { A, {string, B}};
info_opt({"Title"= A, B}) ->
    { A, {string, B}};
info_opt({"Subject" = A, B}) ->
    { A , {string, B}};
info_opt({"Keywords" = A, B}) ->
    { A, {string, B}}.



%% @spec destination(PageRef::integer(), BoundBox) -> array()
%% BoundBox = {'"XYZ"', Left, Top, Zoom } | '"Fit"' | 
%% {'"FitH"', Top} | {'"FitV"', Left} | 
%% {'"FitR"', Left, Bottom, Right, Top} | '"FitB"' | 
%% {'"FitBH"', Top} | {'"FitBV"', Left}
%% Top = integer()
%% Bottom = integer()
%% Right = integer()
%% Bottom = integer()
%% Zoom = null | number() 
%% @doc Creates a destination array. 
%% PageRef is the indirect reference Ptr to the Page Object.
%% see also: PDF reference chapter 8.2.1 Destinations 
destination(PageRef, {"XYZ", Left, Top, Zoom }) ->
    {array,[{ptr, PageRef, 0}, {name,"XYZ"}, Left, Top, Zoom ]};
destination(PageRef, "Fit") ->
    {array,[{ptr, PageRef, 0}, {name,"Fit"}]};
destination(PageRef, {"FitH", Top}) ->    
    {array,[{ptr, PageRef, 0}, {name,"FitH"}, Top ]};
destination(PageRef, {"FitV", Left}) ->    
    {array,[{ptr, PageRef, 0}, {name,"FitV"}, Left ]};
destination(PageRef, {"FitR", Left, Bottom, Right, Top}) ->    
    {array,[{ptr, PageRef, 0}, {name,"FitR"}, Left, Bottom, Right, Top ]};
destination(PageRef, "FitB") ->    
    {array,[{ptr, PageRef, 0}, {name,"FitB"} ]};
destination(PageRef, {"FitBH", Top}) ->    
    {array,[{ptr, PageRef, 0}, {name,"FitBH"}, Top ]};
destination(PageRef, {"FitBV", Left}) ->    
    {array,[{ptr, PageRef, 0}, {name,"FitBH"}, Left ]}.

%% ==========================================================
%% Fonts
%% ==========================================================

%% @spec fonts(Fonts, Objects::pdftype()) -> {FontsPtr, Objects}

fonts(Fonts, Objects) ->
    Free0 = eg_pdf_lib:get_next_ref(Objects),
    Fonts1 = lists:map(fun(I) -> eg_font_map:handler(I) end, Fonts),
    {Free,FontsPtr,O1s}  = mk_fonts(Fonts1, Free0, [], []),
    {FontsPtr, eg_pdf_lib:store_object(O1s, Objects)}.

mk_fonts([], I, Fs, Os) -> 
    A = {{obj,I,0},{dict,lists:map(fun({Alias, FontObj}) ->
		      {Alias, {ptr,FontObj,0}}
	      end, lists:reverse(Fs))}},
    {I+1, {ptr,I,0}, lists:reverse([A|Os])};
mk_fonts([Handler|T], I, Fs, E) ->
    %% io:format("I need the font:~p~n",[Handler]),
    Index = Handler:index(),
    Alias = "F" ++ eg_pdf_op:i2s(Index),
    case Handler:type() of
	internal ->
	    O = {{obj,I,0},mkFont(Handler)},
	    mk_fonts(T, I+1, [{Alias,I}|Fs], [O|E]);
	{Index, pdf_builtin} ->
	    O1 = {{obj,I,0},   mkFont1(Handler, I+1, Index)},
	    O2 = {{obj,I+1,0}, mkFontDescriptor(Handler, false, 0)},
	    mk_fonts(T, I+2, [{Alias,I}|Fs], [O2,O1|E]);
	external ->
	    O1 = {{obj,I,0},   mkFont1(Handler, I+1, Index)},
	    O2 = {{obj,I+1,0}, mkFontDescriptor(Handler, true,I+2)},
	    O3 = {{obj,I+2,0}, mkFontFile(Handler)},
	    mk_fonts(T, I+3, [{Alias,I}|Fs], [O3,O2,O1|E])
    end.

%% mkFont is used for the 14  inbuilt fonts
mkFont(FontHandler) ->
    Index = FontHandler:index(),
    Alias = "F" ++ eg_pdf_op:i2s(Index),
    %% io:format("mkFont Alias=~s FontHandler=~p~n",[Alias, FontHandler]),
    {dict,[{"Type",{name,"Font"}},
	   {"Subtype",{name,"Type1"}},
	   {"Name",{name,Alias}},
	   {"BaseFont",{name,FontHandler:fontName()}},
	   {"Encoding",{name,encoding(FontHandler)}}]}.

encoding(M) ->
    %% Change the encoding to "MacRomanEncoding" except for
    %% "FontSpecific" encodings ...
    %% This seems to work for everything except those fonts
    %% which have a "FontSpecif" encoding.
    %% *usally the encoding in the AFM file is 
    %% "AdobeStandardEncoding" - but this gives an error
    %% for fonts with encoding "AppleStandard". Setting
    %% *everything* to MacRomanEncoding seems to work for all cases
    %% except Zapfdingblats which is "FontSpecific"
    %% - this might not work with files produced on an apple ?
    %% - I have not yet tested this on an apple
    case M:encoding() of
	S = "FontSpecific" ->
	    S;
	_ ->
	    "MacRomanEncoding"
    end.

mkFont1(M, FontDescriptorPrt, Index) ->
    FirstChar = M:firstChar(),
    LastChar = M:lastChar(),
    Widths = make_width(M:encoding(), M, FirstChar, LastChar),
    {dict,[{"Type",{name,"Font"}},
	   {"Subtype",{name,"Type1"}},
	   {"Name",{name,"F" ++ eg_pdf_op:i2s(Index)}},
	   {"BaseFont",{name,M:fontName()}},
	   {"Encoding",{name,encoding(M)}},
	   {"FirstChar",FirstChar},
	   {"LastChar",LastChar},
	   {"Widths", {array,Widths}},
	   {"FontDescriptor",{ptr,FontDescriptorPrt,0}}]}.

make_width("AdobeStandardEncoding", M, F, L) ->
    Seq = lists:seq(F,L),
    Fu = fun(unknown) -> 0;
	   (X) -> X
	end,
    Map = eg_convert:mac2pdf(Seq),
    [Fu(M:width(X)) || X <- Map];
make_width(_, M, _, _) ->
    M:widths().

mkFontDescriptor(M, Embedded, I) ->
    {X1,X2,X3,X4} = M:fontBBox(),
    %% io:format("Flags FIXED to 6 ...~n"),
    FontBBox = [X1,X2,X3,X3],
    D0 = [{"Type",{name,"FontDescriptor"}},
	  {"Ascent", M:ascender()},
	  {"CapHeight", M:capHeight()},
	  {"Descent", M:descender()},
	  {"Flags", M:flags()},
	  {"FontBBox",{array,FontBBox}},
	  {"FontName",{name,M:fontName()}},
	  {"ItalicAngle",M:italicAngle()},
	  {"StemV",M:stemV()},
	  {"XHeight",M:xHeight()}],
    D = case Embedded of
	    true ->
		[{"FontFile", {ptr,I,0}}|D0];
	    false ->
		D0
	end,
    {dict, D}.

%%          {{obj,8,0},
%%           {dict,[{"Type",{name,"FontDescriptor"}},
%%                  {"Ascent",890},
%%                  {"CapHeight",707},
%%                  {"Descent",65306},
%%                  {"Flags",6},
%%                  {"FontBBox",{array,[-100,-65311,1218,895]}},
%%                  {"FontName",{name,"UtopiaMedium"}},
%%                  {"ItalicAngle",0},
%%                  {"StemV",80},
%%                  {"XHeight",512},
%%                  {"FontFile",{ptr,9,0}}]}},
%%          {{obj,9,0},
%%           {stream,94215,
%%                   "stream_9_0_94215",
%%                   [{"Length",94215},
%%                    {"Length1",5750},
%%                    {"Length2",87922},
%%                    {"Length3",543}]}},

mkFontFile(Handler) ->
    {Len,Len1,Len2,Len3,Bin} = get_font_program(Handler),
    {stream,{dict,[{"Length",Len},
		   {"Length1",Len1},
		   {"Length2",Len2},
		   {"Length3",Len3}]},
     Bin}.


get_font_program(Handler) ->
    File = filename:join(eg_lib:priv_src_dir(),
                         atom_to_list(Handler) ++ ".pfb"),
    %% io:format("reading Font from:~s~n",[File]),
    P = eg_embed:parse_pfb(File),
    case P of
	[{_,L1,B1},{_,L2,B2},{_,L3,B3}|_] ->
	    {L1+L2+L3,L1,L2,L3,list_to_binary([B1,B2,B3])};
	_ ->
	    error
    end.
