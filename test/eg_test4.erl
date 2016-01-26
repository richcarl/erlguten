%% -*- coding: latin-1 -*-
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
%% Purpose: Grid planning sheet
%%==========================================================================

-module(eg_test4).

-export([test/0]).


-define(TOP_Y, 820).
-define(FONT_H, 18).
-define(LINE_H, 20). % FONT_H + 2
-define(FONT_Hs, 12).
-define(LINE_Hs, 14). % FONT_Hs + 2
-define(BLOCK_H, 104). % LINE_H + LINE_Hs * 6 (4 lines + top & bottom pad)


test()->
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,a4),
    eg_pdf:set_page(PDF,1),
    eg_pdf:show_grid(PDF, a4),
    eg_pdf:set_font(PDF,"Times-Roman", 36),
    Base = 575,
    eg_pdf:moveAndShow(PDF, 50,Base, "A4 template planning sheet-"),
    eg_pdf:moveAndShow(PDF, 50,Base-50, "Use this sheet to see the"),
    eg_pdf:moveAndShow(PDF, 50,Base-100,
                           "co-ordinates of your objects."),
    eg_pdf:set_font(PDF, "GoodCityModern", 60),
    eg_pdf:moveAndShow(PDF, 50, 675, "ErlGuten fonts"),
    eg_pdf:set_font(PDF,"Times-Italic", 16),
    eg_pdf:moveAndShow(PDF, 50, 625, "The above is similar to"
		" Gutenbergs original font"),
    eg_pdf:moveAndShow(PDF, 50, 610, "used in the 42 line bible - see "
			"page 2 for more examples"),

    eg_pdf:set_font(PDF,"Times-Roman", 24),
    eg_pdf:moveAndShow(PDF, 50, 400, "The following pages display all "
			"the available"),
    eg_pdf:moveAndShow(PDF, 50, 350, "fonts and all their printable "
			"characters"),
    eg_pdf:moveAndShow(PDF, 50, 300,
                           "latin-1 codes 32-126 and 160-255."),
    eg_pdf:moveAndShow(PDF, 50, 250, "line 1 =  32 - 79"),
    eg_pdf:moveAndShow(PDF, 50, 200, "line 2 =  80 - 126"),
    eg_pdf:moveAndShow(PDF, 50, 150, "line 3 = 160 - 208"),
    eg_pdf:moveAndShow(PDF, 50, 100, "line 4 = 209 - 255"),

    PageNo = 2,
    eg_pdf:new_page(PDF),
    eg_pdf:set_page(PDF,PageNo),
    Fonts = lists:sort(eg_font_map:all_fonts()),
    showem(PDF, ?TOP_Y, Fonts, PageNo),
    {Serialised, _PageCount} = eg_pdf:export(PDF),
    file:write_file("eg_test4.pdf",[Serialised]),
    eg_pdf:delete(PDF).

showem(_PDF, _, [], _) ->
    true;
showem(PDF, Y, FontL, PageNo) when Y < ?BLOCK_H ->
    eg_pdf:new_page(PDF),
    eg_pdf:set_page(PDF,PageNo + 1),
    showem(PDF, ?TOP_Y, FontL, PageNo + 1);
showem(PDF, Y, [H|T], PageNo) ->
    eg_pdf:set_font(PDF, "Times-Roman", 10),
    X1 = 20,
    eg_pdf:moveAndShow(PDF, 20,Y, H),
    eg_pdf:set_font(PDF,H, ?FONT_H),
    X2 = X1 + 120,
    eg_pdf:moveAndShow(PDF, X2, Y,
                           "abcdefg ABCDEFG 1234567890 åäö ÅÄÖ"),

    eg_pdf:set_font(PDF,H, ?FONT_Hs),
    X3 = X1 + 20,
    eg_pdf:moveAndShow(PDF, X3, Y - (?LINE_Hs * 2), line1()),
    eg_pdf:moveAndShow(PDF, X3, Y - (?LINE_Hs * 3), line2()),
    eg_pdf:moveAndShow(PDF, X3, Y - (?LINE_Hs * 4), line3()),
    eg_pdf:moveAndShow(PDF, X3, Y - (?LINE_Hs * 5), line4()),

    showem(PDF, Y- ?BLOCK_H, T, PageNo).


line1() -> lists:seq(32, 79).
line2() -> lists:seq(80, 126).
line3() -> lists:seq(160, 208).
line4() -> lists:seq(209, 255).
