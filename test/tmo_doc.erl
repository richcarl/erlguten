%%==========================================================================
%% Copyright (C) 2004 Sean Hinde
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
%% Authors: Sean Hinde <sean.hinde@mac.com>
%% Purpose: Test PDF documents main api
%%==========================================================================

-module(tmo_doc).

-export([test/0, test/1, test/2]).

-define(font_size, 10).

-record(doc_info, {system    = " ",
                   type      = " ",
                   reference = " ",
                   author    = " ",
                   version   = " ",
                   date      = " "}).

% State during main output routine
-record(st,
        {doc_info = #doc_info{},
         toc_num = [], % toc after page numbering
         toc = [],     % toc before page numbering
         y = 735,      % How far up the current page; starting position is 700
         min_y = 60,   % Bottom margin - bottom edge of page to last text line
         max_y = 735,  % Top margin - bottom edge of page to top of text area
         page = 1,     % Current Page Number
         fig = 1,      % Current figure number
         pending_images = []
        }).
             

test() ->
    test("tmo_doc.xml").

test(File_name) ->
    Outfile = filename:rootname(filename:basename(File_name)) ++ ".pdf",
    Xml = eg_xml_lite:parse_file(File_name),
    {value, {xml, Doc}} = lists:keysearch(xml, 1, Xml),
    {Doc_info, History, ToC} = doc_meta(Doc),
    io:format("Doc Info = ~p~n",[Doc_info]),
    PDF = eg_pdf:new(),
    St = #st{doc_info = Doc_info,
             toc = ToC,
             y = 700,
             page = 1},

    St1 = title_page(PDF, Doc_info, St),
    St2 = revision_pages(PDF, History, Doc_info, St1),
    St3 = toc_pages(PDF, ToC, Doc_info, St2),
    St4 = St3#st{y = St#st.max_y},
    St5 = main_content(PDF, Doc, St4),
    io:format("Main Content = done~n",[]),
    page_numbers(PDF, St5),
    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(Outfile,[Serialised]),
    eg_pdf:delete(PDF).

test(File_name, section) ->
    Outfile = filename:rootname(filename:basename(File_name)) ++ ".pdf",
    Xml = eg_xml_lite:parse_file(File_name),
    {value, {xml, Doc}} = lists:keysearch(xml, 1, Xml),
    ToC = doc_toc([Doc]),
    PDF = eg_pdf:new(),
    St = #st{doc_info = #doc_info{},
             toc = ToC,
             y = 735,
             page = 1},
    St5 = main_content(PDF, {document, [], [Doc]}, St),
    io:format("Main Content = done~n",[]),
    page_numbers(PDF, St5),
    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(Outfile,[Serialised]),
    eg_pdf:delete(PDF).

title_page(PDF, Doc_info, S) ->
    eg_pdf:set_author(PDF, Doc_info#doc_info.author),
    eg_pdf:set_title(PDF, Doc_info#doc_info.system),
    eg_pdf:set_subject(PDF, Doc_info#doc_info.type),

    header(PDF, Doc_info#doc_info.system, Doc_info#doc_info.type),

    %% Title in middle of front page
    eg_pdf:ensure_font_gets_loaded(PDF, "Times-Bold"),
    eg_pdf:ensure_font_gets_loaded(PDF, "Courier"),
    eg_pdf:ensure_font_gets_loaded(PDF, "Courier-Bold"),
    eg_pdf:ensure_font_gets_loaded(PDF, "Helvetica"),
    eg_pdf:ensure_font_gets_loaded(PDF, "Times-Italic"),
    eg_pdf:begin_text(PDF),

    Txt = [{raw, Doc_info#doc_info.system ++ "\r\n" ++ Doc_info#doc_info.type}],
    TagMap = {[p], [{default,eg_richText:mk_face("Times-Bold",26,true,
						 default,0)}]},
    
    Norm = eg_xml2richText:normalise_xml({p, [], Txt}, TagMap),
    {p, _, RichText} = Norm,
    {Lines, _, _} =
        eg_line_break:break_richText(RichText, {centered, [400]}),
    lines2pdf(PDF, 100,586,Lines, 52, [400], [0], centered),
    eg_pdf:end_text(PDF),

    %% Document Info
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF, "Times-Roman", 12),
    eg_pdf:set_text_pos(PDF, 100, 425),
    eg_pdf:text(PDF, "Document Number: "),
    eg_pdf:set_text_pos(PDF, 100, 0),
    eg_pdf:text(PDF, Doc_info#doc_info.reference),
    eg_pdf:set_text_pos(PDF, -100, -25),
    eg_pdf:text(PDF, "Author: "),
    eg_pdf:set_text_pos(PDF, 100, 0),
    eg_pdf:text(PDF, Doc_info#doc_info.author),
    eg_pdf:set_text_pos(PDF, -100, -25),
    eg_pdf:text(PDF, "Version: "),
    eg_pdf:set_text_pos(PDF, 100, 0),
    eg_pdf:text(PDF, Doc_info#doc_info.version),
    eg_pdf:set_text_pos(PDF, -100, -25),
    eg_pdf:text(PDF, "Date: "),
    eg_pdf:set_text_pos(PDF, 100, 0),
    eg_pdf:text(PDF, Doc_info#doc_info.date),
    eg_pdf:end_text(PDF),
    S.

revision_pages(PDF, Revisions, Doc_info, S) ->
    %% Revision History 
    eg_pdf:new_page(PDF),
    header(PDF, Doc_info#doc_info.system, Doc_info#doc_info.type),
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF, "Helvetica-Bold", 14),
    eg_pdf:set_text_pos(PDF, 50, 735),
    eg_pdf:text(PDF, "History"),
    eg_pdf:end_text(PDF),
    io:format("Revisions = ~p~n",[Revisions]),

    Header = {row, [], [{cell, [], [{b, [], [{raw, "Version"}]}]},
                        {cell, [], [{b, [], [{raw, "Status"}]}]},
                        {cell, [], [{b, [], [{raw, "Date"}]}]},
                        {cell, [], [{b, [], [{raw, "Changes"}]}]},
                        {cell, [], [{b, [], [{raw, "Author"}]}]}]},
    
    S1 = table(PDF, [Header] ++ Revisions, 50, 475, S#st{y = 725}),
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF, "Helvetica-Bold", 14),
    eg_pdf:set_text_pos(PDF, 50, S1#st.y-25),
    eg_pdf:text(PDF, "References"),
    eg_pdf:end_text(PDF),

    footer(PDF),
    S1#st{page = S#st.page + 1}.    % TODO - number of pages used

%% Output Table of Contents Pages
toc_pages(PDF, ToC, Doc_info, St) ->
    eg_pdf:new_page(PDF),
    header(PDF, Doc_info#doc_info.system, Doc_info#doc_info.type),
    
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF, "Helvetica-Bold", 18),
    eg_pdf:set_text_pos(PDF, 235, 735),
    eg_pdf:text(PDF, "Table of Contents"),
    eg_pdf:end_text(PDF),

    eg_pdf:set_font(PDF, "Times-Roman", 12),
    eg_pdf:set_fill_color(PDF,blue),

    %% Lay out the table of contents. Indent is the indent from the
    %% left of the page. Gap is the offset from the start of the
    %% number to the start of the heading. Space is the leading
    %% between lines. Care has been taken to make sure that space is
    %% consistent even between headers with more that one level
    %% between hem - e.g.1.1 followed by 1.1.1.1 Based on 12Pt font.
    lists:foldl(fun({N, Lvl, Txt}, {PrevLvl, X, Y}) ->
                        I = indent(Lvl),
                        G = gap(Lvl),
                        S = space(PrevLvl, Lvl),
                        eg_pdf:begin_text(PDF),
                        eg_pdf:set_text_pos(PDF, X+I, Y-S),
                        eg_pdf:text(PDF, N),
                        eg_pdf:end_text(PDF),
                        eg_pdf:begin_text(PDF),
                        %% io:format("ToC PDF:~p~n",[{N, Txt}]),
                        TagMap = {[p], 
                                  [{default,
                                    eg_richText:mk_face("Times-Roman",12,
                                                        true,default,0)},
                                   {em,
                                    eg_richText:mk_face("Times-Italic",12,
                                                        true,default,0)},
                                   {code,
                                    eg_richText:mk_face("Courier",12,
                                                        true,default,0)},
                                   {b,
                                    eg_richText:mk_face("Times-Bold",12,
                                                        true,default,0)}]},
                        {p, _, RichText} = 
                            eg_xml2richText:normalise_xml({p, [], Txt}, TagMap),
                        lines2pdf(PDF, X+G+I,Y-S,[RichText],0,[300],[0],
                                  justified),
                        eg_pdf:end_text(PDF),
                        {Lvl, X, Y-S}
                  end, {0,50,700}, ToC),
    eg_pdf:set_fill_color(PDF,black),
    footer(PDF),
    St#st{page = St#st.page + 1}.    % TODO number of pages used

indent(1) -> 0;
indent(2) -> 25;
indent(3) -> 50;
indent(4) -> 85.

gap(1) -> 25;
gap(2) -> 25;
gap(3) -> 35;
gap(4) -> 45.

space(0,_) -> 20;
space(1, _) -> 20;
space(2, 1) -> 20;
space(2, _) -> 17;
space(3, 1) -> 20;
space(3, 2) -> 17;
space(3, _) -> 14;
space(4, 1) -> 20;
space(4, 2) -> 17;
space(4, _) -> 14.


%%%%%%%%%%%%%%%%%%%
%% Main Document Output
%%%%%%%%%%%%%%%%%%%
main_content(PDF, {document, _, Doc}, S) ->
    doc(PDF, Doc, S).

doc(PDF, [{sect1, _, [{title, _, Title}|Sect1]}|T], S) ->
    S1 = title1(PDF, Title, S),
    S2 = sect1(PDF, Sect1, S1),
    doc(PDF, T, S2);
doc(PDF, [{section, _, [{title, _, Title}|Sect1]}|T], S) ->
    S1 = title1(PDF, Title, S),
    S2 = sect1(PDF, Sect1, S1),
    doc(PDF, T, S2);
doc(PDF, [{appendix, _, Appendix}|T], S) ->
    S1 = appendix(PDF, Appendix, S),
    doc(PDF, T, S1);
doc(PDF, [_|T], S) ->
    doc(PDF, T, S);
doc(_PDF, [], S) ->
    S.

appendix(_PDF, _Appendix, S) ->
    S.

sect1(PDF, [{para, _, Para}|T], S) ->
    %% io:format("Para = ~p~n",[Para]),
    S1 = para(PDF, 40, 475, Para, S),
    sect1(PDF, T, S1);
sect1(PDF, [{sect2, _, [{title, _, Title}|Sect2]}|T], S) ->
    S1 = title(PDF, Title, 2, S),
    %% io:format("Sect2 = ~p~n",[Sect2]),
    S2 = sect2(PDF, Sect2, S1),
    sect1(PDF, T, S2);
sect1(PDF, [{section, _, [{title, _, Title}|Sect2]}|T], S) ->
    S1 = title(PDF, Title, 2, S),
    %% io:format("Sect2 = ~p~n",[Sect2]),
    S2 = sect2(PDF, Sect2, S1),
    sect1(PDF, T, S2);
sect1(PDF, [{code, _, Code}|T], S) ->
    %% io:format("Code = ~p~n",[S]),
    S1 = code(PDF, 40, 475, Code, S),
    sect1(PDF, T, S1);
sect1(PDF, [{list, Type, List}|T], S) ->
    %% io:format("List = ~p~n",[S]),
    S1 = list1(PDF, 50, 465, List, Type, S),
    sect1(PDF, T, S1);
sect1(PDF, [{image, [{"src", Path}], _}|T], S) ->
    %% io:format("SRC = ~p~n",[Path]),
    S1 = image(PDF, Path, S),
    sect1(PDF, T, S1);
sect1(PDF, [{table, _, Rows}|T], S) ->
    %% io:format("Table = ~p~n",[Table]),
    S1 = table(PDF, Rows, 80, 475, S),
    sect1(PDF, T, S1);
sect1(PDF, [_|T], S) ->
    sect1(PDF, T, S);
sect1(_PDF, [], S) ->
    S.

sect2(PDF, [{para, _, Para}|T], S) ->
    S1 = para(PDF, 40, 475, Para, S),
    sect2(PDF, T, S1);
sect2(PDF, [{sect3, _, [{title, _, Title}|Sect3]}|T], S) ->
     S1 = title(PDF, Title, 3, S),
     S2 = sect3(PDF, Sect3, S1),
     sect2(PDF, T, S2);
sect2(PDF, [{section, _, [{title, _, Title}|Sect3]}|T], S) ->
     S1 = title(PDF, Title, 3, S),
     S2 = sect3(PDF, Sect3, S1),
     sect2(PDF, T, S2);
sect2(PDF, [{code, _, Code}|T], S) ->
    S1 = code(PDF, 40, 475, Code, S),
    sect2(PDF, T, S1);
sect2(PDF, [{list, Type, List}|T], S) ->
    S1 = list1(PDF, 50, 465, List, Type, S),
    sect2(PDF, T, S1);
sect2(PDF, [{image, [{"src", Path}], _}|T], S) ->
    %% io:format("SRC = ~p~n",[Path]),
    S1 = image(PDF, Path, S),
    sect2(PDF, T, S1);
sect2(PDF, [{table, _, Rows}|T], S) ->
    %% io:format("Table = ~p~n",[Table]),
    S1 = table(PDF, Rows, 80, 475, S),
    sect2(PDF, T, S1);
sect2(PDF, [_|T], S) ->
    sect2(PDF, T, S);
sect2(_PDF, [], S) ->
    S.


sect3(PDF, [{para, _, Para}|T], S) ->
    S1 = para(PDF, 40, 475, Para, S),
    sect3(PDF, T, S1);
sect3(PDF, [{code, _, Code}|T], S) ->
    S1 = code(PDF, 40, 475, Code, S),
    sect3(PDF, T, S1);
sect3(PDF, [{list, Type, List}|T], S) ->
    S1 = list1(PDF, 50, 465, List, Type, S),
    sect3(PDF, T, S1);
sect3(PDF, [{image, [{"src", Path}], _}|T], S) ->
    %% io:format("SRC = ~p~n",[Path]),
    S1 = image(PDF, Path, S),
    sect3(PDF, T, S1);
sect3(PDF, [{table, _, Rows}|T], S) ->
    %% io:format("Table = ~p~n",[Table]),
    S1 = table(PDF, Rows, 80, 475, S),
    sect3(PDF, T, S1);
sect3(PDF, [_|T], S) ->
    sect3(PDF, T, S);
sect3(_PDF, [], S) ->
    S.

title1(PDF, Title, S0) ->
    io:format("Title1 = ~p~n",[Title]),
    S1 = new_page(PDF, true, S0),
    %% If any pending images were output then we must
    %% make another new page for a top level chapter.
    S = if (S1#st.y /= S1#st.max_y) ->
                new_page(PDF, false, S1);
           true ->
                S1
        end,

    [{Num, Lvl, Str}|T] = S#st.toc,
    eg_pdf:rectangle(PDF, 60,S#st.y,500,2, fill),
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF, "Helvetica-Bold", 18),
    eg_pdf:set_text_pos(PDF, 60, S#st.y-25),
    eg_pdf:text(PDF, Num),
    TagMap = {[p], 
              [{default, eg_richText:mk_face("Helvetica-Bold",18,
                                             true,default,0)},
               {em,      eg_richText:mk_face("Helvetica-Bold",18,
                                             true,default,0)},
               {code,    eg_richText:mk_face("Courier-Bold",18,
                                             true,default,0)},
               {b,       eg_richText:mk_face("Helvetica-Bold",18,
                                             true,default,0)}]},
    {p, _, RichText} = 
        eg_xml2richText:normalise_xml({p, [], Title}, TagMap),
    lines2pdf(PDF, 80,S#st.y-25,[RichText],0,[500],[0],justified),
    eg_pdf:end_text(PDF),
    eg_pdf:rectangle(PDF, 60,S#st.y-40,500,2, fill),
    S#st{toc_num = [{Lvl, Num, Str, S#st.page}|S#st.toc_num],
         toc = T,
         y = S#st.y - 50}.

title(PDF, Title, Level, S0) ->
    io:format("Title = ~p~n",[{Title, Level}]),
    S = if (S0#st.y < 80) ->
                new_page(PDF, true, S0);
           true ->
                S0
        end,
    [{Num, Lvl, Str}|T] = S#st.toc,
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF, "Helvetica-Bold", ?font_size + 2),
    eg_pdf:set_text_pos(PDF, 50, S#st.y-20),
    eg_pdf:text(PDF, Num),
    TagMap = {[p], 
              [{default, eg_richText:mk_face("Helvetica-Bold",?font_size + 2,
                                             true,default,0)},
               {em,      eg_richText:mk_face("Helvetica-Bold",?font_size + 2,
                                             true,default,0)},
               {code,    eg_richText:mk_face("Courier-Bold",?font_size + 2,
                                             true,default,0)},
               {b,       eg_richText:mk_face("Helvetica-Bold",?font_size + 2,
                                             true,default,0)}]},
    {p, _, RichText} = 
        eg_xml2richText:normalise_xml({p, [], Title}, TagMap),
    Indent = case Level of
                 1 -> 70;
                 2 -> 80;
                 3 -> 90
             end,
    lines2pdf(PDF, Indent,S#st.y-20,[RichText],0,[500],[0],justified),
    eg_pdf:end_text(PDF),
    
    S#st{toc_num = [{Lvl, Num, Str, S#st.page}|S#st.toc_num],
         toc = T,
         y = S#st.y - 25}.

para(PDF, Indent, Width, Para0, S) ->
    Para = skip_white(Para0),                   % Remove leading ws
    TagMap = {[para], [{default, eg_richText:mk_face("Times-Roman",?font_size,
                                                     true,default,0)},
                       {em,      eg_richText:mk_face("Times-Italic",?font_size,
                                                     true,default,0)},
                       {code,    eg_richText:mk_face("Courier",?font_size,
                                                     true,default,0)},
                       {b,       eg_richText:mk_face("Times-Bold",?font_size,
                                                     true,default,0)}]},
    %%io:format("Para = ~p~n",[Para]),
    Norm = eg_xml2richText:normalise_xml({para, [], Para}, TagMap),
    %% io:format("Norm1 = ~p~n",[Norm]),
    {para, _, RichText} = Norm,
    Widths =  [Width],
    Off = [Indent],
    {Lines, _, _} = case eg_line_break:break_richText(RichText,
                                                      {justified, Widths}) of
                        impossible ->
                            io:format("Cannot break line are widths ok~n"),
                            {[],[],[]};
                        {Lines1,A,B} ->
                            {Lines1, A, B}
                    end,
    %% io:format("Lines1 = ~p~n",[Lines]),
    S1 = output_lines(PDF, Lines, Widths, Off, Indent, Width, justified, S),
    S1#st{y = S1#st.y - 5}.

list1(PDF, Indent, Width, Items, Type0, S) ->
    Type = case Type0 of
               [{"type", "numbered"}] -> 1;
               [{"type", "bulleted"}] -> bulleted;
               []                   -> bulleted
           end,
    list2(PDF, Indent, Width, Items, Type, S).

list2(PDF, Indent, Width, [{item, _, Item}|T], Type, S) ->
    %% io:format("List = ~p~n",[Item]),
    {S1, Type1} = item(PDF, Indent, Width, Item, Type, S),
    list2(PDF, Indent, Width, T, Type1, S1);
list2(PDF, _, _, [], _, S) ->
    images(PDF, S).    % We did not allow pending images during the
                       % list, so we should give them a chance here.


item(PDF, Indent, Width, [{para, _, _Para} | _T] = P, Type, S0) ->
    S = if ((S0#st.y - 14) < S0#st.min_y) ->
                new_page(PDF, false, S0);
           true ->
                S0
        end,
    eg_pdf:begin_text(PDF),
    Type1 = if is_integer(Type) ->
                    eg_pdf:set_text_pos(PDF, 80, S#st.y-?font_size-2),
                    eg_pdf:set_font(PDF, "Times-Roman", ?font_size),
                    eg_pdf:text(PDF, eg_pdf_op:n2s(Type) ++ "."),
                    Type + 1;
               Type == bulleted ->
                    eg_pdf:set_text_pos(PDF, 80, S#st.y-14),
                    eg_pdf:set_font(PDF, "Times-Roman", 16),
                    eg_pdf:text(PDF, [225]),              % Bullet Char
                    bulleted
            end,
    eg_pdf:end_text(PDF),
    {S1, T1} = item_paras(PDF, Indent, Width-10, P, S),
    item(PDF, Indent, Width, T1, Type1, S1);
item(_PDF, _, _, [], Type, S0) ->
    {S0, Type};
item(PDF, Indent, Width, Item0, Type, S0) ->
    S = if ((S0#st.y - 14) < S0#st.min_y) ->
                new_page(PDF, false, S0);
           true ->
                S0
        end,
    %% io:format("Item0 = ~p~n",[Item0]),
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF, "Times-Roman", 16),
    Type1 = if is_integer(Type) ->
                    eg_pdf:set_text_pos(PDF, 80, S#st.y-?font_size-2),
                    eg_pdf:set_font(PDF, "Times-Roman", ?font_size),
                    eg_pdf:text(PDF, eg_pdf_op:n2s(Type) ++ "."),
                    Type + 1;
               Type == bulleted ->
                    eg_pdf:set_text_pos(PDF, 80, S#st.y-14),
                    eg_pdf:set_font(PDF, "Times-Roman", 16),
                    eg_pdf:text(PDF, [225]),              % Bullet Char
                    bulleted
            end,
    eg_pdf:end_text(PDF),
    Item = skip_white(Item0),                   % Remove leading ws
    TagMap = {[item], [{default, eg_richText:mk_face("Times-Roman",?font_size,
                                                     true,default,0)},
                       {em,      eg_richText:mk_face("Times-Italic",?font_size,
                                                     true,default,0)},
                       {code,    eg_richText:mk_face("Courier",?font_size,
                                                     true,default,0)},
                       {b,       eg_richText:mk_face("Times-Bold",?font_size,
                                                     true,default,0)}]},
    %% io:format("code = ~p~n", [Para]),
    Norm = eg_xml2richText:normalise_xml({item, [], Item}, TagMap),
    %% io:format("Norm1 = ~p~n",[Norm]),
    {item, _, RichText} = Norm,
    Widths =  [Width-10],
    Off = [Indent],
    {Lines, _, _} = eg_line_break:break_richText(RichText, {justified, Widths}),
    %% io:format("Output = ~p~n",[{Lines,Widths,Off,Indent,Width,justified,S}]),
    S1 = output_lines(PDF, Lines, Widths, Off, Indent, Width-10, justified, S),
    %% io:format("Output Done~n"),
    {S1#st{y = S1#st.y - 5}, Type1}.

item_paras(PDF, Indent, Width, [{para, _, Para}|T], S) ->
    S1 = para(PDF, Indent, Width, Para, S),
    item_paras(PDF, Indent, Width, T, S1);
item_paras(_PDF, _Indent, _Width, [_|_T] = Items, S) ->
    {S, Items};
item_paras(_, _, _, [], S) ->
    {S, []}.

code(PDF, Indent, Width, Para, S) ->
    TagMap = {[code], 
              [{default,eg_richText:mk_face("Courier",?font_size,false,
					    default,0)}]},
    %% io:format("code = ~p~n", [Para]),
    Norm = eg_xml2richText:normalise_xml({code, [], Para}, TagMap),
    %% io:format("Norm1 = ~p~n",[Norm]),
    {code, _, RichText} = Norm,
    Widths =  [Width],
    Off = [Indent],
    {Lines, _W, _O} = eg_line_break:break_richText(RichText,
                                                   {preformatted, Widths}),
    %% io:format("Lines1 = ~p~n",[{Lines, W, O}]),
    S1 = output_lines(PDF, Lines, Widths, Off, Indent, Width,
                      left_justified, S),
    S1#st{y = S1#st.y - 5}.

output_lines(PDF, Lines, Widths, Off, Indent, Width, Just, S) ->
    Y = S#st.y,
    Min_y = S#st.min_y,
    Y_req = length(Lines) * (?font_size + 2),
    %% io:format("Input1 = ~p~n",[{Off, Y, Y_req, Lines, Min_y}]),
    if Y_req =< (Y - Min_y)  ->
            Code = eg_richText2pdf:richText2pdf(Indent, Y, Just, 0, Lines, 
                                                (?font_size+2), Widths, Off),
            %% io:format("Code = ~p~n",[Code]),
            eg_pdf:begin_text(PDF),
            eg_pdf:append_stream(PDF, Code),
            eg_pdf:end_text(PDF),
            S#st{y = S#st.y - Y_req};
       true ->
            Left =  (Y - Min_y),
            This_page_lines = if Left =< 14 -> 0;
                                 true -> Left div (?font_size+2)
                              end,
            {TPL, NPL} = lists:split(This_page_lines, Lines),
            Code = eg_richText2pdf:richText2pdf(Indent, Y, Just, 0, TPL, 
                                                (?font_size+2), Widths, Off),
            %% io:format("Code = ~p~n",[Code]),
            eg_pdf:begin_text(PDF),
            eg_pdf:append_stream(PDF, Code),
            eg_pdf:end_text(PDF),
            S1 = new_page(PDF, true, S),
            output_lines(PDF, NPL, Widths, Off, Indent, Width, Just, S1)
    end.
 
%% Output an image if there is room.  If the image will fit widthways
%% without scaling based on 1 pixel per Pt then put it like that,
%% otherwise scale it down to size.  If the image is too long to fit
%% on the current page then queue it to appear on the next available
%% page.
image(PDF, Path, S0) ->
    S = space_before(10, S0),
    case eg_pdf_image:get_head_info(Path) of
        {jpeg_head,{W, H, _Ncomponents, _Data_precision}} ->
           {W0, H0} = if (W =< 435) ->
                              {W, H};
                         true ->
                              {trunc(W*W/435),trunc(H*W/435)}
                      end,
            {W1, H1} = if (H0 < 660) ->
                               {W0, H0};
                          true ->
                              {trunc(W0*H0/660),trunc(H0*H0/660)}
                      end,
            io:format("W1, H1}: ~p~n",[{W1, H1, S#st.y}]),
            if H1 > (S#st.y - S#st.min_y - 20) ->
                       S#st{pending_images = [{img, Path}|S#st.pending_images]};
               true ->
                    X = 218 - trunc(W1/2) + 50,
                    io:format("Image~n"),
                    eg_pdf:image(PDF, Path, {X, S#st.y-H1}, {width, W1}),
                    eg_pdf:begin_text(PDF),
                    eg_pdf:set_text_pos(PDF, X + W1/2, S#st.y-H1 - 8),
                    eg_pdf:text(PDF, "Figure " ++ eg_pdf_op:n2s(S#st.fig)),
                    eg_pdf:end_text(PDF),
                    S#st{y = S#st.y - H1 - 20,
                         fig = S#st.fig + 1}
            end;
        _Other ->
            io:format("Error, "
                      "image format not supported or image not found: ~p~n",
                      [Path]),
            S
    end.

images(PDF, S) ->
    images(PDF, S#st.pending_images, S).

images(PDF, [{img, Path}|T], S) ->
    S1 = image(PDF, Path, S#st{pending_images = T}),
    io:format("Length = ~p~n",[{length(S1#st.pending_images),
                                length(S#st.pending_images)}]),
    if length(S1#st.pending_images) == length(S#st.pending_images) ->
            eg_pdf:new_page(PDF),
            io:format("New page~n"),
            header(PDF, S1),
            footer(PDF),
            images(PDF, [{img, Path}|T], S1#st{page = S1#st.page + 1,
                                               y = S1#st.max_y});
       true ->
            images(PDF, T, S1#st{pending_images = T})
    end;
images(_PDF, [], S) ->
    S.

new_page(PDF, Images, S) ->
    io:format("Page = ~p~n", [S#st.page+1]),
    eg_pdf:new_page(PDF),
    header(PDF, S),
    footer(PDF),
    if (S#st.pending_images == []) or (Images == false) ->
            %{_, Bin} = process_info(self(), backtrace),
            %io:format("New page = ~p~n",[binary_to_list(Bin)]),
            S#st{page = S#st.page + 1,
                 y = S#st.max_y};
       true ->
            images(PDF, S#st.pending_images, S#st{page = S#st.page + 1,
                                                  y = S#st.max_y})
    end.
    
header(PDF, #st{doc_info = I}) ->
    header(PDF, I#doc_info.system, I#doc_info.type).

header(PDF, Title, Subtitle) ->
    eg_pdf:image(PDF,'tmobile.jpg',{50,790},{height,29}),

%     eg_pdf:set_fill_gray(PDF,0.75),
%     eg_pdf:rectangle(PDF, 40,780,515,2, fill),
%     eg_pdf:set_fill_gray(PDF,0.0),

    %% Header
    eg_pdf:begin_text(PDF),
    {L1, W1, O1} = xml2lines("<p><b>" ++ Title ++"</b></p>",200,14,1,
                             preformatted),
    lines2pdf(PDF, 345,818,L1, 14, W1, O1, right_justified),
    
    {L2, W2, O2} = xml2lines("<p><b>" ++ Subtitle++ "</b></p>",200,14,1,
                             preformatted),
    lines2pdf(PDF, 345,802,L2, 14, W2, O2, right_justified),
     eg_pdf:end_text(PDF).

footer(PDF) ->
    eg_pdf:set_fill_gray(PDF,0.75),
    eg_pdf:rectangle(PDF, 40,45,515,2, fill),
    eg_pdf:set_fill_gray(PDF,0.0).


page_numbers(PDF, S) ->
    page_numbers(PDF, S#st.page, eg_pdf_op:n2s(S#st.page)).

page_numbers(_PDF, 1, _) ->
    ok;
page_numbers(PDF, N, Tot) ->
    R = eg_pdf:set_page(PDF, N),
    io:format("R = ~p~n",[R]),
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF,"Helvetica", ?font_size),
    Str = "Page " ++ eg_pdf_op:n2s(N) ++ " of " ++ Tot,
    Width = eg_pdf:get_string_width(PDF,"Helvetica", ?font_size, Str),
    eg_pdf:set_text_pos(PDF, 555 - Width, 30),
    eg_pdf:text(PDF, Str),
    eg_pdf:end_text(PDF),
    page_numbers(PDF, N-1, Tot).

%% Creating tables
%%
%% 1. work out number of columns
%% 2. split space evenly, leaving an extra 4 Pts for white space around each,
%%    and an extra 1 Pt for each vertical Line
%% 2 bis. OR Turn each cell into RTF as if it had the full width to use.

%%           Then find a bunch of metrics about the table which will
%%           help us find a good set of column widths.

%%           Find the longest word in each column. This sets the
%%           minimum column width. 

%%           If the sum of the longest words is greater than the total
%%           width avaialable then issue a warning, but carry on with
%%           the table extending off the page to the right (really
%%           trick would be to switch to landscape mode automatically
%%           :)

%%           Ideally we want to fit into the minimum vertical space,
%%           so if there is a solution where every or most rows can
%%           fit on one line this would be very nice.

%%            Could do proportional split based on total volume of text
%%            in each column
%%
%%           We could find the longest cell for each column

%%   If all cells in a column will fit on a single line within what
%%   would be an even split between the columns, allow that column to
%%   take up the space it needs to fit everything one one line? OK,
%%   but at the limit might really penalise another column with lots
%%   of text.

%% 3. Convert the lext into RTF lines for each cell
%% 4. For each row work out the number of lines required 
%%    (largest number of lines).
%% 5. Output the table, drwing lines as we go
%%
%% TODO - Parameterise by: Font size, Spacing around text, table width
%%        Clever algorithm to arrange column widths
%%        Do page breaks in the middle of a table
%% @spec table(PDF, Rows, X, Y) ->
%%          Total_Y
% table(PDF, Rows, S) ->
%     table(PDF, Rows, 45, S#st.y
%         ).

table(PDF, Rows, X, W0, S0) ->
    S = space_before(10, S0),
    Cols = max_length(Rows,0),      % Number of cols is max cols of all rows.
    Col_width = W0 div Cols,
    W = W0 - 5*Cols,

    RTF_words = lists:map(fun(Row) -> 
                                row2rtf(Row, lists:duplicate(Cols, W)) 
                        end, Rows),

    %% Find the longest single word in each column. Start with a minimum col
    %% size of 20000 milliPts. This gives reasonable minimum column widths
    Longest_words = tuple_to_list(
                      longest_words(RTF_words, erlang:make_tuple(Cols, 28000))),
    io:format("Longest word = ~p~n",[Longest_words]),
    Min_tab_width = lists:sum(Longest_words) + Cols * 5000,
    if Min_tab_width div 1000 + Cols > W -> % round up each col to next Pt
            io:format("*** Warning *** "
                      "Table will not fit into available width ~p~n", 
                      [Min_tab_width div Cols + Cols]);
       true -> ok
    end,
    _Text_width = Col_width - 4 - 1, % leave space for lines + ws

    %% Sum the total length of text in each column. This gives a
    %% general measure of the size of a column.
    Volumes = word_volumes(RTF_words, erlang:make_tuple(Cols, 0)),
    io:format("Volumes = ~p~n",[Volumes]),

    %% Find the longest single cell in each column. This does two
    %% things - it allows us to contract the whole table width if all
    %% columns fit completely within the overall width on a single
    %% line. It also might allow us to squeeze things around so that
    %% some columns are all one liners...

    Max_cell_widths = longest_cell(Volumes, erlang:make_tuple(Cols, 0)),
    io:format("Max Cell Widths = ~p~n",[Max_cell_widths]),

    Col_volume = sum_cells(Volumes, lists:duplicate(Cols, 0)),
    io:format("Column volumes = ~p~n",[Col_volume]),

    %% Try to share the column widths out respecting the minimums,
    %% but in proportion to the Volumes

    %% 1. Distribute by volume.
    I1 = lists:map(fun(Col) ->
                           Col / lists:sum(Col_volume) * W * 1000
                   end, Col_volume),
    io:format("I1 = ~p~n",[I1]),

    %% 2. Increase any columns which have been given less than their minimum
    {I2, Lost} = ensure_minimums(I1, Longest_words, 0, []),
    io:format("I2 = ~p~n",[{Lost, I2}]),

    %% 3. Try to give out any reductions amongst the other columns,
    %% not allowing them to go below minimum.
    I3 = distribute_reduction(I2, Longest_words, Lost),
    io:format("I3 = ~p~n",[I3]),


    %% 4. Reduce any cols which have been given more space than they
    %% need, noting which ones they are.

    %% 5. Try to give out any extra amongst cols which need it in
    %% proportion to their volume, not increasing the table width
    %% beyond the max width. Go back to step 4 to make sure we have
    %% not given any column too much. This recursion is handled by
    %% expand/3.

    I5bis = expand(I3, Max_cell_widths, W*1000),

    %% 6. Normalise, rounding up to the nearest Pt.
    I6 = lists:map(fun({fixed, Val}) ->
                           round(Val / 1000) + 1;
                      (Val) ->
                           round(Val / 1000) + 1
                   end, I5bis),
    io:format("I6 = ~w~n",[I6]),

    RTFRows = lists:map(fun(Row) ->
                                %% io:format("RTFRow = ~p~n",[Row]),
                                row2rtf(Row, I6)
                        end, Rows),
    % io:format("RTFRows = ~p~n",[RTFRows]),
    Heights = lists:map(fun(Row) -> max_row_lines(Row,0) end, RTFRows),
    % io:format("Heights = ~p~n",[Heights]),

    %% Re-add the 2 Pt gap and 1 Pt line width for each column
    I7 = lists:map(fun(Text_w) -> Text_w + 5 end, I6),

    %% Don't start a table near the bottom of a page so we don't get
    %% only a top line, and because it don't look good to start it at
    %% the bottom.
    S1 = if S#st.y - 26 < S#st.min_y ->
                 new_page(PDF, false, S);
            true ->
                 S
         end,
    S2 = rows(PDF, Heights, RTFRows, X, I7, Cols, false, S1),
    %% Now we've finished the table, flush any pending images from
    %% before the start of the table
    images(PDF, S2).

rows(PDF, [H|T],[Row|Rows],X,Col_widths, Cols, Mid_row, S) ->
    %% TODO - Work out when to draw top line - i.e. when we are really
    %% at the start of a row even in a multipage row
    if Mid_row == false ->
            eg_pdf:rectangle(PDF, X,S#st.y,lists:sum(Col_widths) + 1,1, fill);
       true -> ok
    end,
    if S#st.y - (H*?font_size) =< S#st.min_y ->
            This_page_lines = (S#st.y - S#st.min_y) div ?font_size,
            {TPR, NPR} = split_row(This_page_lines, Row),
            S1 = row(PDF, TPR, X, Col_widths, This_page_lines, Cols, S),
            %% io:format("NPR = ~p~n", [NPR]),
            %% Draw a line at the bottom as 
            if NPR == [] ->
                    eg_pdf:rectangle(PDF, X,S1#st.y,lists:sum(Col_widths) + 1,1,
                                     fill);
               true -> ok
            end,
            Mid_row_2 = NPR /= [],
            %% We need a new page, but don't want pending images
            %% appearing in the middle of the table hence 'false'.
            S2 = new_page(PDF, false, S1),
            rows(PDF, [H - This_page_lines|T], 
                         [NPR|Rows], X, Col_widths, Cols, Mid_row_2, S2);
       true ->
            Y = S#st.y,
            S1 = row(PDF, Row, X,Col_widths,H, Cols, S),
            rows(PDF, T, Rows, X, Col_widths, Cols, false,
                 S1#st{y = Y - (H*?font_size)-5})
    end;
rows(PDF, [], [], X, Col_widths, _Cols, _Mid_row, S) ->
    %% Draw final line under table
    eg_pdf:rectangle(PDF, X,S#st.y,lists:sum(Col_widths) + 1,1, fill),
    S.

row(PDF, [{Lines, Width, Off}|Cells], X, [Col_width|T], Height, Cols, S) ->
    eg_pdf:rectangle(PDF, X,S#st.y,1,-(Height*?font_size)-5, fill),
    eg_pdf:begin_text(PDF),
    lines2pdf(PDF, X+3,S#st.y,Lines, ?font_size, Width, Off, justified),
    eg_pdf:end_text(PDF),
    row(PDF, Cells, X+Col_width, T, Height, Cols-1, S);
row(PDF, [], X, [], Height, 0, S) ->
    eg_pdf:rectangle(PDF, X,S#st.y,1,-(Height*?font_size)-5, fill),
    S;
row(PDF, [], X, [Col_width|T], Height, Cols, S) ->
    eg_pdf:rectangle(PDF, X,S#st.y,1,-(Height*?font_size)-5, fill),
    row(PDF, [], X+Col_width, T, Height, Cols-1, S).


row2rtf({row, _, Row}, Col_widths) ->
    TagMap = {[cell], [{default, eg_richText:mk_face("Times-Roman",?font_size,
                                                     true,default,0)},
                       {em,      eg_richText:mk_face("Times-Italic",?font_size,
                                                     true,default,0)},
                       {code,    eg_richText:mk_face("Courier",?font_size,
                                                     true,default,0)},
                       {b,       eg_richText:mk_face("Times-Bold",?font_size,
                                                     true,default,0)}]},
    row2rtf1(Row, Col_widths, TagMap);
row2rtf({header, _, Row}, Col_widths) ->
    TagMap = {[cell], [{default, eg_richText:mk_face("Times-Bold",?font_size,
                                                     true,default,0)},
                       {em,      eg_richText:mk_face("Times-Bold",?font_size,
                                                     true,default,0)},
                       {code,    eg_richText:mk_face("Courier-Bold",?font_size,
                                                     true,default,0)},
                       {b,       eg_richText:mk_face("Times-Bold",?font_size,
                                                     true,default,0)}]},
    row2rtf1(Row, Col_widths, TagMap).


row2rtf1([Cell|T], [Col_width|T1], TagMap) ->
    %%io:format("Cell = ~p~n",[{Col_width, Cell}]),
    Norm = eg_xml2richText:normalise_xml(Cell, TagMap),
    {cell, _, RichText} = Norm,
    %%io:format("Norm = ~p~n",[RichText]),
    {Lines, _,_} =
        eg_line_break:break_richText(RichText, {justified, [Col_width]}),
    %% io:format("Lines = ~p~n",[Lines]),
    [{Lines, [Col_width], [0]}|row2rtf1(T, T1, TagMap)];
row2rtf1(_, [Cw|T], TagMap) ->
    [{[], [Cw], [0]}|row2rtf1([], T, TagMap)];
row2rtf1([], [], _) ->
    [].


max_length([{_Tag, _, Row} | Rows], Max) ->
    if  length(Row) > Max -> max_length(Rows, length(Row));
        true              -> max_length(Rows, Max)
    end;
max_length([], Max) ->
    Max.

max_row_lines([{Row,_,_} | Rows], Max) ->
    if  length(Row) > Max -> max_row_lines(Rows, length(Row));
        true              -> max_row_lines(Rows, Max)
    end;
max_row_lines([], Max) ->
    Max.


%% Find the longest word in each column
longest_words([Row|T], Array) ->
    A1 = lws(Row, 1, Array),
    longest_words(T, A1);
longest_words([], A) ->
    A.

lws([{Lines, _, _}|T], N, A) ->
    Lw = lw(Lines, 0),
    if (element(N, A) < Lw) ->
            lws(T, N+1, setelement(N, A, Lw));
       true ->
            lws(T, N+1, A)
    end;
lws([], _, A) ->
    A.

lw([{richText, Words}|T], L) ->
    L1 = lw1(Words, L),
    lw(T, L1);
lw([], L) ->
    L.

lw1([{word, L, _, _}|T], L0) ->
    if(L > L0) ->
            lw1(T, L);
      true ->
            lw1(T, L0)
    end;
lw1([_|T], L) ->
    lw1(T, L);
lw1([], L) ->
    L.
    
%% Sum the total lengths of all text per cell
word_volumes(RTF, Arr) ->
    lists:foldl(fun(Row, A1) ->
                        {A2, _} = row_volumes(Row, Arr),
                        A1 ++ [tuple_to_list(A2)]
                end, [], RTF).

row_volumes(Row, Arr) ->
    %% io:format("Vols ~p~n", [Arr]),
    lists:foldl(fun({L, _, _}, {A1, Col_num}) ->
                        Vol = line_volume(L),
                        Old = element(Col_num, A1),
                        New = setelement(Col_num, A1, Old + Vol),
                        {New, Col_num + 1}
                end, {Arr, 1}, Row).

line_volume(Line) ->
    %% io:format("Line~n"),
    lists:foldl(fun({richText, Words}, V) ->
                        words_volume(Words, V)
                end, 0, Line).

words_volume(Words, Vol) ->
    %% io:format("Word ~p~n", [{Vol, Words}]),
    lists:foldl(fun({word, L, _, _}, V) ->
                        V + L;
                   ({space, L, _}, V) ->
                        V + L
                end, Vol, Words).

%% Given the output of word_volumes/2, find the max of each column.
longest_cell(Arr_list, Arr) ->
    lists:foldl(fun(Row, A) ->
                        max_vals(Row, A)
                end, tuple_to_list(Arr), Arr_list).

max_vals([H|T], [H1|T1]) ->
    if H > H1 ->
            [H|max_vals(T, T1)];
       true ->
            [H1|max_vals(T, T1)]
    end;
max_vals([], []) ->
    [].

sum_cells(Arr_list, Arr) ->    
    lists:foldl(fun(Row, A) ->
                        sum_vals(Row, A)
                end, Arr, Arr_list).

sum_vals([H|T], [H1|T1]) ->
    [H+H1|sum_vals(T, T1)];
sum_vals([], []) ->
    [].

ensure_minimums([H|T], [H1|T1], Lost, Res) ->
    if H > H1 ->
            ensure_minimums(T, T1, Lost, Res ++ [H]);
       true ->
            ensure_minimums(T, T1, Lost + H1 - H, Res ++ [{fixed, H1}])
    end;
ensure_minimums([], [], Lost, Res) ->
    {Res, Lost}.

%% Recursively go through giving out any saved in earlier steps and
%% then ensuring that we have not given out too much, until all
%% columns are fixed, or

expand(Init, Max_cell_widths, W) ->
    io:format("Init = ~p~n",[{Init, Max_cell_widths, W}]),
    {I1, Gained} = ensure_maximums(Init, Max_cell_widths, 0, []),
    io:format("I1 Gained = ~p~n",[{Gained, I1}]),
    Sum = lists:foldl(fun({fixed, Col}, Sum) ->
                              Sum + Col;
                         (Col, Sum) ->
                              Sum + Col
                      end, 0, I1),
    io:format("Sum = ~p~n",[Sum]),
    Fixed = is_fixed(Init),
    io:format("Fixed = ~p~n",[Sum]),
    io:format("W = ~p~n",[W]),
    if (Sum >= W - 1) or Fixed ->
            I1;
       true ->
            I2 = distribute_increase(I1, Sum, W),
            io:format("I2 Increase = ~p~n",[I2]),
            expand(I2, Max_cell_widths, W)
    end.
            

ensure_maximums([{fixed, H}|T], [_Max_needed|T1], Gain, Res) ->
    ensure_maximums(T, T1, Gain, Res ++ [{fixed, H}]);
ensure_maximums([H|T], [Max_needed|T1], Gain, Res) ->
    io:format("ensure_maximums - ~p~n",[{Gain , H , Max_needed}]),
    if Max_needed =< H ->
            ensure_maximums(T, T1, Gain + H - Max_needed,
                            Res ++ [{fixed, Max_needed}]);
       true ->
            ensure_maximums(T, T1, Gain, Res ++ [H])
    end;
ensure_maximums([], [], Gain, Res) ->
    {Res, Gain}.

distribute_reduction(Cols, Minimums, Lost) ->
    Sum = lists:foldl(fun({fixed, _Col}, Sum) ->
                              Sum;
                         (Col, Sum) ->
                              Sum + Col
                      end, 0, Cols),
    distribute_reduction(Cols, Minimums, Sum, Lost).

distribute_reduction([{fixed, Col}|T], [_Min|T1], Sum, Lost) ->
    [{fixed, Col}|distribute_reduction(T, T1, Sum, Lost)];
distribute_reduction([Col|T], [Min|T1], Sum, Lost) ->
    Reduced = Col - Col/Sum*Lost,
    if Reduced < Min -> %% We can't take enough away. Darn!
            New_lost = Lost - (Min - Reduced),
            New_lost1 = if New_lost < 0 ->
                                0;
                           true ->
                                New_lost
                        end,
            [{fixed, Min}|distribute_reduction(T, T1, Sum, New_lost1)];
       true ->
            [Reduced|distribute_reduction(T, T1, Sum, Lost)]
    end;
distribute_reduction([], [], _Sum, _Lost) ->
    [].

distribute_increase(Cols, Sum, W) ->
    Gain = W - Sum,
    To_share = lists:foldl(fun ({fixed, _Col}, Sum1) -> Sum1;
                               (Col, Sum1)           -> Sum1 + Col
                           end, 0, Cols),
    distribute_increase1(Cols, Gain, To_share).
    
distribute_increase1([{fixed, Col}|T], Gain, To_share) ->
    [{fixed, Col}|distribute_increase1(T, Gain, To_share)];
distribute_increase1([Col|T], Gain, To_share) ->
    Inc = Col/To_share * Gain,
    io:format("Inc = ~p~n",[{Col, To_share, Gain}]),
    [Col + Inc|distribute_increase1(T, Gain, To_share)];
distribute_increase1([],_,_) ->
    [].

is_fixed([{fixed, _}|T]) ->
    is_fixed(T);
is_fixed([_|_]) ->
    false;
is_fixed([]) ->
    true.

%% Split a table row into two rows where the first has no more
%% than Height number of lines in any cell, and the second row
%% contains the remaining lines.
%% TPR - This Page Rows
%% NPR - Next Page Rows
%% TPL - this page lines etc
split_row(Height, Cells) ->
    lists:foldl(fun({Cell, W, O}, {TPR, NPR}) ->
                        {TPL, NPL} = if length(Cell) =< Height ->
                                             {Cell, []};
                                        true ->
                                             lists:split(Height, Cell)
                                     end,
                        {TPR ++ [{TPL, W, O}], NPR ++ [{NPL, W, O}]}
                end, {[], []}, Cells).


%% %% Create a clickable link on the page.
%% %% We need to define an object for each link of the form:
%% %% 46 0 obj
%% %% <</Subtype/Link
%% %% /BS <</W 0>>
%% %% /Border [0 0 0]
%% %% /Rect [71.2 468.2 263.1 478.1]
%% %% /Dest (d0e272)
%% %% >>
%% link(PDF, X,Y, Text, Link_to) ->
%%     [].


%% Generic routines for paragraph formatting
%%

xml2lines(Para, Len, PtSize, NLines, Justification) ->
    Xml = parse_xml_para_str(Para),
    %% io:format("XML: ~p~n",[Xml]),
    lines(Xml, Len, PtSize, NLines, Justification).


lines(Xml, Len, _PtSize, NLines, Justification) ->
    TagMap = {[p], 
              [{default, eg_richText:mk_face("Times-Roman",?font_size,
                                             true,default,0)},
               {em,      eg_richText:mk_face("Times-Italic",?font_size,
                                             true,default,0)},
               {code,    eg_richText:mk_face("Courier",?font_size,
                                             true,default,0)},
               {b,       eg_richText:mk_face("Times-Bold",?font_size,
                                             true,default,0)}]},
    %%io:format("TagMap: ~p~n",[TagMap]),
   % ensure_fonts_are_loaded(PDF, TagMap),
    Norm = eg_xml2richText:normalise_xml(Xml, TagMap),
    {p, _, RichText} = Norm,
    %% io:format("Norm: ~p~n",[Norm]),
    Widths = lists:duplicate(NLines, Len),
    Off = lists:duplicate(NLines, 0),
    case eg_line_break:break_richText(RichText, {Justification, Widths}) of
        impossible ->
            io:format("Cannot break line are widths ok~n"),
            {[],[],[]};
        {Lines,_A,_B} ->
            %% io:format("Lines = ~p~n",[Lines]),
            %io:format("Other things = ~p~n",[{A,B}]),
            {Lines, Widths, Off}
    end.

lines2pdf(PDF, X,Y,Lines, Leading, Widths, Off, Justification) ->
    %% io:format("Input = ~p~n",[{X, Y, Justification, 0, Lines, 
%%                               Leading, Widths, Off}]),
    Code = eg_richText2pdf:richText2pdf(X, Y, Justification, 0, Lines, 
                                        Leading, Widths, Off),
    %io:format("Code = ~p~n",[Code]),
    eg_pdf:append_stream(PDF, Code).
    

parse_xml_para_str(Str) ->
    [{xml, XmlPara}] = eg_xml_lite:parse_all_forms(Str),
    XmlPara.

%% Insert some vertical space unless we are at the top of the page
space_before(Pts, S) ->
    if S#st.y == S#st.max_y ->
            S;
       true ->
            S#st{y = S#st.y - Pts}
    end.


%% Traverse a valid TMO docbook type variant of things pulling out all
%% the pre-amble and meta information.
%% 
doc_meta({document, _, Doc}) ->
    Doc_Info = doc_info(Doc),
    History = doc_history(Doc),
    ToC =  doc_toc(Doc),
    {Doc_Info, History, ToC}.

%% Search out the document info to go on the title page
doc_info(Doc) ->
    %io:format("Doc = ~p~n",[Doc]),
    case lists:keysearch(documentinfo, 1, Doc) of
        {value, {_, _, Info}} ->
            lists:foldl(fun(I, Acc) -> doc_info(I, Acc) end, #doc_info{}, Info);
        false ->
            #doc_info{}
    end.

doc_info({system, _, [{raw, Txt}]}, Inf) ->
    Inf#doc_info{system = Txt};
doc_info({type, _, [{raw, Txt}]}, Inf) ->
    Inf#doc_info{type = Txt};
doc_info({reference, _, [{raw, Txt}]}, Inf) ->
    Inf#doc_info{reference = Txt};
doc_info({author, _, [{raw, Txt}]}, Inf) ->
    Inf#doc_info{author = Txt};
doc_info({version, _, [{raw, Txt}]}, Inf) ->
    Inf#doc_info{version = Txt};
doc_info({date, _, [{raw, Txt}]}, Inf) ->
    Inf#doc_info{date = Txt}.

%% Make up the input to the table generating routines from the doc
%% history.
doc_history(Doc) ->
    case lists:keysearch(history, 1, Doc) of
        {value, {history, _, Versions}} ->
            lists:map(fun(V) -> doc_history1(V) end, Versions);
        false ->
            []
    end.

doc_history1({version, _, Version}) ->
    [{vsn, _, Vsn},
     {status, _, Status},
     {date, _, Date},
     {changes, _, Changes},
     {author, _, Author}] = Version,
    {row, [], [{cell, [], Vsn}, 
               {cell, [], Status},
               {cell, [], Date}, 
               {cell, [], Changes}, 
               {cell, [], Author}]}.
    

%% Traverse the whole document pulling out the section titles and
%% assigning a section level. Also validate that section nesting rules
%% are obeyed (in the absense of any DTD stuff in Joe's parser).
doc_toc(Doc) ->
    {Acc, _} = doc_toc(Doc, 1),
    io:format("ToC = ~p~n",[Acc]),
    number_toc(Acc).

doc_toc(Doc, N) ->
    lists:foldl(fun(E, Lvl) -> doc_toc1(E, Lvl) end, {[],N}, Doc).

doc_toc1({Sect1, _, [{title, _, Title}|S1]}, {Acc, 1}) when Sect1 == sect1;
                                                            Sect1 == section ->
    {Titles, _N} = doc_toc(S1, 2),
    {Acc ++ [{1, Title}|Titles], 1};
doc_toc1({sect1, _, [{title, _, _Title}|_]}, {_Acc, _}) ->
    exit({error, "sect1 not a top level"});

doc_toc1({Sect2, _, [{title, _, Title}|S2]}, {Acc, 2}) when Sect2 == sect2;
                                                            Sect2 == section ->
    {Titles, _N} = doc_toc(S2, 3),
    {Acc ++ [{2, Title}|Titles], 2};
doc_toc1({sect2, _, [{title, _, _Title}|_]}, {_Acc, _}) ->
    exit({error, "sect2 not at second level"});

doc_toc1({Sect3, _, [{title, _, Title}|S3]}, {Acc, 3}) when Sect3 == sect3;
                                                            Sect3 == section ->
    {Titles, _N} = doc_toc(S3, 3),
    {Acc ++ [{3, Title}|Titles], 3};
doc_toc1({sect3, _, [{title, _, _Title}|_]}, {_Acc, _}) ->
    exit({error, "sect3 not at second level"});

doc_toc1(_, Acc) ->
    Acc.


%% Given a list of section titles with the section level for each one,
%% generate the actual section number using Legal notation "1" "1.1"
%% "1.2" "2.1" etc
number_toc(Toc) ->
    number_toc(Toc, [0]).

number_toc([{N, Txt}|T], Count) ->
    C2 = inc_count(N, Count),
    C3 = sect_num_to_list(C2),
    [{C3, N, Txt}|number_toc(T, C2)];
number_toc([], _) ->
    [].

inc_count(N, Count) ->
    Lvl = length(Count),
    if Lvl == N ->
            inc_last(Count);
       N < Lvl ->
            Curr = lists:sublist(Count, N),
            inc_last(Curr);
       N > Lvl ->
            Count ++ [1]
    end.

inc_last(L) ->
    Len = length(L),
    Start = lists:sublist(L, Len-1),
    End = lists:nth(Len,L),
    Start ++ [End + 1].
                    
            
sect_num_to_list([I]) ->
    integer_to_list(I);
sect_num_to_list([I|T]) ->
    integer_to_list(I) ++ "." ++ sect_num_to_list(T).
    

skip_white(X = [{raw, [H|T]}|T1]) ->
    case is_white_or_nl(H) of
	true  -> skip_white([{raw, T}|T1]);
	false -> X
    end;
skip_white([{raw, []}|T1]) ->
    T1;
skip_white(Else) ->
    Else.

is_white($\s) -> true;
is_white($\t) -> true;
is_white(_)   -> false.

is_white_or_nl($\n) -> true;
is_white_or_nl($\r) -> true;
is_white_or_nl(X)   -> is_white(X).
