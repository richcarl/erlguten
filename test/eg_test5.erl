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
%%==========================================================================

-module(eg_test5).

-export([test/0, test/1]).


test() ->
    test(9).

test(1) -> 
    text(1,11);
test(2) ->
    %% Noraml para test - fits exactly into three lines
    eg_line_break:break_richText(text(1,11), {justified, [190,190,190]});
test(3) ->
    %% This has some splill
    eg_line_break:break_richText(text(1,11), {justified, [190,190]});
test(4) -> 
    eg_line_break:break_richText(text(1,11), {ragged, [190,190,190,190]});
test(5) -> 
    %% fits exactly
    eg_line_break:break_richText(text(1,11), 
				 {preformatted, [200,200,200,200,200,200,200]});
test(6) -> 
    %% Has some spill
    eg_line_break:break_richText(text(1,11), {preformatted, [200,200,200]});
test(7) -> 
    eg_line_break:break_richText(text(1,11), 
				 {centered, [210,210,210,210,210,210,210,210]});
%% test the line breaker
test(8) ->
    W1 = eg_richText:mk_test_word("Have"),
    W2 = eg_richText:mk_test_word("a"),
    W3 = eg_richText:mk_test_word("splended"),
    W4 = eg_richText:mk_test_word("opportunity"),
    eg_line_break:make_partitions([W1,W2,W3,W4], eg_hyphen_rules_en_GB);
test(9) ->
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,a4),
    eg_pdf:set_page(PDF,1),
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF, "Times-Roman", 24),
    demo(PDF, 0, 350, 475, 12, 13, 0, 180),
    demo(PDF, 45, 325, 600, 11, 12, 0, 190),
    demo(PDF, 90, 250, 575, 11, 12, 0, 190),
    demo(PDF, 135, 150, 600, 10, 11, 0, 190),
    demo(PDF, 180, 300, 200, 14, 16, 0, 130),
    demo(PDF, 225, 150, 375, 11, 12, 0, 130),
    demo(PDF, 270, 125, 500, 9, 10, 0, 60),
    demo(PDF, 315, 375, 375, 12, 13, 10, 120),
    eg_pdf:end_text(PDF),
    eg_pdf:show_grid(PDF, a4),
    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file("eg_test5.pdf",[Serialised]),
    eg_pdf:delete(PDF).

demo(PDF, Rot, X, Y, PointSize, Leading, Offset, Width) ->
    Widths = [Width-Offset|lists:duplicate(30,Width)],
    Off = [Offset|lists:duplicate(30,0)],
    {Lines,_,_} = eg_line_break:break_richText(text(1,Rot,PointSize), 
					       {justified, Widths}),
    Code = eg_richText2pdf:richText2pdf(X, Y, justified, Rot, Lines, 
					Leading, Widths, Off),
    io:format("Code=~p~n",[Code]),
    eg_pdf:append_stream(PDF, Code).

% for(0, _F) ->    
%     true;
% for(N, F) ->
%     F(N),
%     for(N-1, F).

% justified_para(Leading, Offset, Width, N) ->
%     justified_para(Leading, Offset, Width, 0, N).

% justified_para(Leading, Offset, Width, Rot, N) ->
%     {justified, Leading, Rot,
%      [Offset|lists:duplicate(N-1, 0)],
%      [Width-Offset|lists:duplicate(N-1,Width)]}.

% ragged_para(Leading, Offset, Width, N) ->
%     {ragged, Leading, 
%      [Offset|lists:duplicate(N-1, 0)],
%      [Width-Offset|lists:duplicate(N-1,Width)]}.

text(N, Pts) ->
    text(N, 0, Pts).

text(1, Rot, Pts) ->
    eg_richText:str2richText("Rotation =" ++ eg_pdf_op:i2s(Rot) ++ 
" Hello joe how are you today?
May I take this opportunity
of saying
that my favorite color is blue.
Have a nice day,
from Mr. C. Computer.", Pts).

