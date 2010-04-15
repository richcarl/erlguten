%%==========================================================================
%% Copyright (C) 2003 Joe Armstrong, Mikael Karlsson 
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
%% Authors:   Joe Armstrong   <joe@sics.se>
%%            Mikael Karlsson <mikael.karlsson@creado.com>
%% Purpose: PDF assembler
%%==========================================================================

-module(eg_pdf_assemble).

-include("eg.hrl").

-export([make_pdf_file/5,
	 pdfloop/2
	]).

%% ============================================================================


%% This is for backwards compatibility with pre-image test files
make_pdf_file(OutFile, Info, Fonts, Pages, MediaBox) ->
    make_pdf_file(OutFile, Info, Fonts, [], Pages, MediaBox,{[],[]}).

make_pdf_file(OutFile, Info, Fonts, Images, Pages, MediaBox, ProcSet) ->
    {Root, Ninfo, Os} = build_pdf(Info, Fonts, Images,Pages,MediaBox,ProcSet),
    {ok, F} = file:open(OutFile, [binary,raw,write]),
    file:write(F, header()),
    {ok, Pos0} = file:position(F, cur),
    %% io:format("Os=~p~n", [Os]),
    {Posns, _} = lists:mapfoldl(fun(I, Pos) -> 
				  {{obj,Index,_},_} = I,
				  Data = eg_pdf_lib:serialise(I),
				  file:write(F, Data),
				  {ok, Pos1} = file:position(F, cur),
				  {{Index,Pos}, Pos1}
			  end, Pos0, Os),
   %% io:format("Posn=~p~n",[Posns]),
    XrefStartPos = add_xref(F, Posns),
    add_trailer(F, Posns, Root, Ninfo),
    add_start_xref(F, XrefStartPos),
    file:close(F).

build_pdf(Info, Fonts, Images, Pages, MediaBox, ProcSet) ->
    %% io:format("build pdf Fonts=~p~n",[Fonts]),
    {Free0,XObjects,O0s}  = eg_pdf_image:mk_images(Images, 1, [], []),
    {Free,Fonts1,O1s}  = mk_fonts(Fonts, Free0, [], []),
    PageTree = Free,
    {Free1,Ps,O3s} = mk_pages(Pages, PageTree, Free+1,[],[]),
    %% io:format("here2:~p~n",[O3s]),
    O2 = {{obj,PageTree,0},
          mkPageTree(Ps, Fonts1, XObjects, MediaBox, ProcSet)},
    Root = Free1,
    O4 = {{obj,Root,0}, mkCatalogue(PageTree)},
    %% io:format("Free1=~p~n",[Free1]),
    NInfo = Free1 + 1,
    O5 = {{obj,NInfo,0}, mkInfo(Info)},
    {Root, NInfo, O0s ++ O1s ++ [O2|O3s] ++ [O4,O5]}.
    
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

mk_pages([], _, N, P, O) -> {N, lists:reverse(P), lists:reverse(O)};
mk_pages([{page,Str}|T], Parent, I, L, E) ->
    O1 = {{obj,I,0},mkPageContents(Str)},
    O2 = {{obj,I+1,0},mkPage( Parent, I)},
    mk_pages(T, Parent, I+2, [I+1|L], [O2,O1|E]);
mk_pages([{page,Str,Script}|T], Parent, I, L, E) ->
    O1 = {{obj,I,0},mkPageContents(Str)},
    O2 = {{obj,I+1,0},mkScript(Script)},
    O3 = {{obj,I+2,0},mkPage( Parent, I, I+1)},
    mk_pages(T, Parent, I+3, [I+2|L], [O3,O2,O1|E]).

mkCatalogue(PageTree) ->
    {dict,[{"Type",{name,"Catalog"}},
	   {"Pages",{ptr,PageTree,0}}]}.

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
%   	S = "AppleStandard" ->
%   	    "MacRomanEncoding";
%   	S = "AdobeStandardEncoding" ->
%   	    S;
	_ ->
	    "MacRomanEncoding"
    end.

mkFont1(M, FontDescriptorPrt, Index) ->
    FirstChar = M:firstChar(),
    LastChar = M:lastChar(),
    Widths = make_width(M:encoding(),M,FirstChar,LastChar),
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

this_dir() ->
    filename:dirname(code:which(?MODULE)).

font_dir() ->
    case code:priv_dir(erlguten) of
	{error, bad_name} ->
	    filename:join(this_dir(), "../priv/src");
	N ->
	    filename:join(N, "priv/src")
    end.

get_font_program(Handler) ->
    File = filename:join(font_dir(), atom_to_list(Handler) ++ ".pfb"),
    %% io:format("reading Font from:~s~n",[File]),
    P = eg_embed:parse_pfb(File),
    case P of
	[{_,L1,B1},{_,L2,B2},{_,L3,B3}|_] ->
	    {L1+L2+L3,L1,L2,L3,concat_binary([B1,B2,B3])};
	_ ->
	    error
    end.

mkInfo(I) ->
    {dict,[{"Creator",{string,I#info.creator}},
	   {"CreationDate",{date, I#info.creationDate}},
	   {"Producer",{string,I#info.producer}},
	   {"Author",{string,I#info.author}},
	   {"Title",{string,I#info.title}},
	   {"Subject",{string,I#info.subject}},
	   {"Keywords",{string,I#info.keywords}}]}.

%% L = [int()] = list of objects representing pages


mkPageTree(L, Fonts, XObjects, MediaBox = {A,B,C,D}, ProcSet ) ->
    ImProcSet = case ProcSet of
		    {imageb,imagec} -> [{name, "ImageB"},{name, "ImageC"}];
		    {imageb,_} -> [{name, "ImageB"}];
		    {_,imagec} -> [{name, "ImageC"}];
		    _ -> []
		end,
    {dict,[{"Type",{name,"Pages"}},
	   {"Count",length(L)},
	   {"MediaBox", {array,[A,B,C,D]}},
	   {"Kids",{array,lists:map(fun(I) ->{ptr,I,0} end,L)}},
	   {"Resources",
	    {dict,[{"Font", Fonts },{"XObject", XObjects },
		   {"ProcSet",
                    {array,[{name,"PDF"},{name,"Text"}|ImProcSet]}}]}}]}.



%% Fonts = [{Name,PageNo}]
%%   example [{"F1",12},{"F7",15}]

mkScript(Script) ->
    {dict,[{"S",{name,"JavaScript"}},
	   {"JS",{string,Script}}]}.


mkPage(Parent, Contents) ->
    {dict, [{"Type", {name,"Page"}},
	    {"Parent", {ptr,Parent,0}},
	    {"Contents", {ptr, Contents, 0}}
	   ]}.

mkPage(Parent, Contents, Script) ->
    {dict, [{"Type", {name,"Page"}},
	    {"Parent", {ptr,Parent,0}},
	    {"Contents", {ptr, Contents, 0}},
	    {"AA", {dict, [{"O", {ptr, Script, 0}}]}}
	   ]}.

% mkPage(Parent, Contents, Script) ->
%     {dict, [{"Type", {name,"Page"}},
% 	    {"Parent", {ptr,Parent,0}},
% 	    {"Contents", {ptr, Contents, 0}},
% 	    {"AA", {dict, [{"O", {ptr, Script, 0}}]}}
% 	   ]}.

mkPageContents(Str) ->
    {stream, Str}.

 
header() ->
    "%PDF-1.3" ++ [8#015,$%,8#342,8#343,8#317,8#323, 8#015,8#012].

%% Objs = {ObjNo, Startpos}

add_xref(F, Objs) ->
    {ok, P} = file:position(F, cur),
    XrefStart = P,
    L  = ["xref\n0 ",eg_pdf_op:i2s(length(Objs)+1),"\n",xref(0,"65535 f")|
	  lists:map(fun({I,Pos}) -> xref(Pos,"00000 n") end, Objs)],
    file:write(F, L),
    XrefStart.

xref(I, Str) ->
    S = lists:flatten(io_lib:format("~10.10.0w", [I])),
    [S," ", Str,"\r\n"].


add_trailer(F, Objs, Root, Info) ->
    L = ["trailer << /Size ", eg_pdf_op:i2s(length(Objs)+1),
	 " /Root ",eg_pdf_op:i2s(Root), " 0 R ",
	 " /Info ",eg_pdf_op:i2s(Info), " 0 R >>\n"],
    file:write(F, L).

add_start_xref(F, XrefStartPos) ->
    L = ["startxref\n",eg_pdf_op:i2s(XrefStartPos),"\n%%EOF\n"],
    file:write(F, L).


%% xref
%% 0 9
%% 0000000000 65535 f 
%% 0000000033 00000 n 
%% 0000000098 00000 n 
%% 0000000144 00000 n 
%% 0000000203 00000 n 
%% 0000000231 00000 n 
%% 0000000409 00000 n 
%% 0000000721 00000 n 
%% 0000000835 00000 n 
%% trailer
%% <<
%% /Size 9
%% /Root 1 0 R
%% /Info 8 0 R
%% >>
%% startxref
%% 1073
%% %%EOF

pdfloop(PDFC, Stream)->
    receive
	{ensure_font, Fontname} ->
	    {F,_,_} = handle_setfont(PDFC#pdfContext.fonts, Fontname),
	    pdfloop(PDFC#pdfContext{fonts=F}, Stream);
	{stream, {append_text, String}}->
	    StrToB = list_to_binary(convert(PDFC#pdfContext.font_handler, String)),
	    Binary = <<Stream/binary, StrToB/binary, <<" ">>/binary>>,
	    pdfloop(PDFC, Binary);
	{stream, {append, String}}->
	    B = list_to_binary(convert(PDFC#pdfContext.font_handler, String)),
	    Binary = <<Stream/binary, B/binary, <<" ">>/binary>>,
	    pdfloop(PDFC, Binary);
	{page_script, Script} ->
	    %% io:format("New script ~p\n", [Script]),
	    NewScript = handle_pagescript(PDFC#pdfContext.scripts,
					  PDFC#pdfContext.currentpage,
					  Script),
	    pdfloop(PDFC#pdfContext{scripts=NewScript}, Stream);
	{font, {set, Fontname, Size}}->
	    {F,Alias,Fhand} = handle_setfont(PDFC#pdfContext.fonts, Fontname),
	    S = list_to_binary(eg_pdf_op:set_font_by_alias(Alias, Size)),
	    Binary = <<Stream/binary, S/binary>>,
	    pdfloop(PDFC#pdfContext{fonts=F,font_handler=Fhand}, Binary);
	{image, FilePath, Size}->
	    {I,IMG,{W,H},ProcSet} = handle_image(PDFC#pdfContext.images, 
						 FilePath, Size, 
						 PDFC#pdfContext.procset),
	    S = list_to_binary(eg_pdf_op:set_image(W,H, IMG)),
	    Binary = <<Stream/binary, S/binary>>,
	    pdfloop(PDFC#pdfContext{images=I,procset=ProcSet}, Binary);
	{page,{new, PID}}->
	    {Add, PageNo} = 
		handle_newpage(PDFC#pdfContext.pages,
			       PDFC#pdfContext.currentpage,
			       [Stream]),
	    PID ! {page, PageNo},
	    pdfloop(PDFC#pdfContext{pages=Add, currentpage=PageNo}, <<>>);
    	{page,{set,PageNo}}->
	    {NewPages,[NewStream]} = handle_setpage(PDFC#pdfContext.pages,PageNo,
						  PDFC#pdfContext.currentpage, 
						  [Stream]),
	    pdfloop(PDFC#pdfContext{pages=NewPages,currentpage=PageNo}, NewStream);		    
	{page,{get_no, PID}} ->
	    PID ! {page, PDFC#pdfContext.currentpage},
	    pdfloop(PDFC, Stream);    
	{info,Info}->
	    NewInfo = handle_info(PDFC#pdfContext.info, Info),
	    pdfloop(PDFC#pdfContext{info=NewInfo}, Stream);
	{mediabox, Mediabox}->
	    pdfloop(PDFC#pdfContext{mediabox=Mediabox}, Stream);	    
	{export,PID} ->
	    %% add last page if necessary before exporting
	    PDF = case Stream of 
		      <<>> ->		    
			  PageNo = PDFC#pdfContext.pages,
			  handle_export(PDFC);
		      _ ->
			  {Add, PageNo} = handle_newpage(
					    PDFC#pdfContext.pages,
					    PDFC#pdfContext.currentpage,
					    [Stream]),
			  handle_export(PDFC#pdfContext{pages=Add})
	    end,
	    PID ! {export, PDF, PageNo},
	    pdfloop(PDFC, Stream);
	delete ->
	    done;
	X ->
	    io:format("Not yet implemented:~p~n", [X]),
	    pdfloop(PDFC, Stream)
    end.

%% Internals

handle_pagescript(ScriptDict, PageNo, Script)->
    orddict:store(PageNo,Script,ScriptDict).

handle_setpage(Pages, PageNo, Current, Stream)->
    NewPageDict = orddict:store(Current,Stream,Pages),
    NewStream = orddict:fetch(PageNo, NewPageDict),
    {NewPageDict,NewStream}.

handle_newpage(Pages,0,Stream)->
    {Pages,1};
handle_newpage(Pages, Current, Stream )->
    NewPageDict = orddict:store(Current,Stream, Pages),
    {NewPageDict, orddict:size(NewPageDict)+1}.

handle_export(PDFC)->
    %% io:format("~nHere handle_export:~p~n",[PDFC]),
    MF = fun(K,V1,V2) ->
		 {script, V1, V2}
	 end,
    Merged = orddict:merge(MF, PDFC#pdfContext.pages,
			   PDFC#pdfContext.scripts),
    Pages =  lists:map(fun({Key,{script,Val,S}}) ->
			       {page, Val, S};
			  ({Key, Val}) ->
			       {page, Val}
		       end,
		       Merged),
    {Root, Ninfo, Os} = 
	build_pdf(PDFC#pdfContext.info, 
		  PDFC#pdfContext.fonts,
		  dict:to_list(PDFC#pdfContext.images),
		  Pages,
		  PDFC#pdfContext.mediabox,
		  PDFC#pdfContext.procset),
    eg_pdf_lib:export(Ninfo, Os).
%%    Objs = lists:map(fun(I) -> serialise2bin(I) end , Os),
%%    eg_pdf_export:mkdoc(Objs, Root, Ninfo).

%% handle_setfont(FontList, FontName) ->
%%   {FontList1, Alias}
%% Alias = "F" ++ Index
handle_setfont(FontList, FontName)->
    case eg_font_map:handler(FontName) of
	undefined ->
	    io:format("There is no font called:~s~n", [FontName]),
	    io:format("Using Times-Roman"),
	    handle_setfont(FontList, "Times-Roman");
	Handler ->
	    Index = Handler:index(),
	    %% io:format("handler for ~s is ~p index:~p~n",
	    %% [FontName,Handler,Index]),
	    case lists:member(Handler, FontList) of
		true ->
		    {FontList, "F"++ eg_pdf_op:i2s(Index), Handler};
		false ->
		    {[Handler|FontList], "F"++ eg_pdf_op:i2s(Index), Handler}
	    end
    end.

handle_image(ImageDict, FilePath, Size, ProcSet)->
    case dict:find(FilePath, ImageDict) of
	{ok, #image{alias=Alias, width=W, height=H}} ->
	    {ImageDict, Alias, set_size(Size,{W,H}), ProcSet };
	error ->
	    Alias = "Im" ++ 
		eg_pdf_op:i2s(dict:size(ImageDict) + 1),
	    case eg_pdf_image:get_head_info(FilePath) of
		{jpeg_head,{W1, H1, Ncomponents, Data_precision}} ->
		    NewDict =dict:store(FilePath,
					#image{alias  = Alias,
                                               width  = W1,
                                               height = H1},
					ImageDict),
		    {NewDict, Alias, set_size(Size, {W1,H1}),
		     imageBC(Ncomponents, ProcSet) };
		
		A -> 
		    {error_not_yet_implemented_image_format,A}
	    end
    end.

%% Function to scale the image properly if only width or height
%% is set.
set_size({max, W1, H1}, {W2,H2}) -> 
    H3 = trunc(W1*H2/W2),
    W3 = trunc(H1*W2/H2),
    if H3 > H1 ->
	    {W3, H1};
       true ->
	    {W1, H3}
    end;
set_size({undefined,undefined},Size2) -> Size2;
set_size({W1,undefined},{W2,H2}) -> {W1,trunc(W1*H2/W2)};
set_size({undefined,H1},{W2,H2}) -> {trunc(H1*W2/H2),H1};
set_size(Size1,_) -> Size1.

%% Set the images for ProcSet
imageBC(Ncomp,{B,C}) when Ncomp =< 2 -> {imageb,C};
imageBC(Ncomp,{B,C}) when Ncomp > 2 -> {B,imagec}.



handle_info(I,{author,Author})->
    I#info{author=Author};
handle_info(I,{title,Title}) ->
    I#info{title=Title};
handle_info(I,{subject,Subject}) ->
    I#info{subject=Subject};
handle_info(I,{date,{Year,Month,Day}})->
    I#info{creationDate={Year,Month,Day}};
handle_info(I,{keywords,Keywords}) ->
    I#info{keywords=Keywords}.



convert(undefined,S) -> 
    eg_convert:mac2pdf(S);
convert(Mod, S) ->
    case Mod:encoding() of
	"FontSpecific" ->
	    S;
	_ ->
	    eg_convert:pdf2mac(S)
    end.
