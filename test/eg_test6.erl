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
%% Authors:   Joe Armstrong <joe@sics.se>
%% Purpose: Grid planning sheet
%%==========================================================================

-module(eg_test6).

-export([test/0]).

%% ============================================================================

test()->
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,a4),
    eg_pdf:set_page(PDF,1),
    %% eg_pdf:set_font(PDF, "GoodCityModern", 40),
    eg_pdf:set_font(PDF, "Victorias-Secret", 40),
    eg_pdf:moveAndShow(PDF, 50, 700, "Hello Joe from Gutenburg"),

    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file("eg_test6.pdf",[Serialised]),
    eg_pdf:delete(PDF).

