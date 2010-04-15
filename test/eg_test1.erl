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
%% Purpose: Test PDF documents main api
%%==========================================================================

-module(eg_test1).

-export([test/0]).

test()->
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,a4),
    eg_pdf:set_author(PDF,"Mikael Karlsson"),
    eg_pdf:set_title(PDF, "Test of PDF API"),
    eg_pdf:set_subject(PDF,"PDF is cool, but Erlang is cooler"),
    eg_pdf:set_keywords(PDF,"Erlang, PDF, Gutenberg"),
%%    eg_pdf:set_date(PDF,2003,3,31),
    draft(PDF),
    eg_pdf:new_page(PDF),
    draft(PDF),

    eg_pdf:set_page(PDF,1),
    eg_pdf:image(PDF,'joenew.jpg',{390,440},{height,140}),
    eg_pdf:image(PDF,'joenew.jpg',{390,200},{width,140}),
    eg_pdf:image(PDF,'joenew.jpg',{190,300},{140,140}),
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF, "Times-Italic", 240),
    eg_pdf:set_text_pos(PDF, 60,600),
    eg_pdf:textbr(PDF, "Wow"),
    eg_pdf:set_font(PDF, "Times-Roman", 42),
    eg_pdf:set_text_pos(PDF, 60,-40),
    eg_pdf:set_text_leading(PDF,60),
    eg_pdf:text(PDF, "Welcome home "),
    eg_pdf:set_text_rise(PDF, 20),
    eg_pdf:textbr(PDF, "Joe, "),
    eg_pdf:set_text_rise(PDF, 0),
    eg_pdf:textbr(PDF, "hope you had a"),
    eg_pdf:textbr(PDF, "nice trip"),
    eg_pdf:end_text(PDF),

    eg_pdf:set_page(PDF,2),

    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF,"Times-Roman", 24),
    eg_pdf:set_text_pos(PDF,60,750),
    eg_pdf:set_text_leading(PDF,26),
    eg_pdf:textbr(PDF, "Times-Roman 24 pt"),
    eg_pdf:set_font(PDF, "Times-Italic", 16),
    eg_pdf:textbr(PDF,"Times-Italic 16 pt"),
    eg_pdf:set_font(PDF, "Courier", 6),
    eg_pdf:textbr(PDF, "Courier 6 pt"),
    eg_pdf:set_text_leading(PDF,14),
    eg_pdf:set_font(PDF,"Blahonga", 12),
    eg_pdf:textbr(PDF, "The blahonga font will fall back to Times-Roman"),
    eg_pdf:textbr(PDF, "This is a check of ( ) \\ escape chars"),
    eg_pdf:kernedtext(PDF, 
		   [ "This is a test of Kerning: A", 120, "W", 
		     120, "A", 95, "Y again" ]),
    eg_pdf:break_text(PDF),
    eg_pdf:textbr(PDF, "This is a text without Kerning: AWAY again"),
    eg_pdf:break_text(PDF),
    eg_pdf:end_text(PDF),

    eg_pdf:new_page(PDF),
    eg_pdf:line(PDF,100,100,200,100),
    eg_pdf:bezier(PDF,100,100,100,200,200,200,200,100),
    eg_pdf:path(PDF,stroke),
    eg_pdf:set_fill_color_RGB(PDF,0.5,0.7,0.2),
    eg_pdf:bezier(PDF,300,100,300,200,500,200,500,100),
    eg_pdf:path(PDF,close_fill_stroke),
    eg_pdf:set_fill_color(PDF,purple),
    eg_pdf:bezier(PDF,{300,400},{300,450},{500,450},{500,400}),
    eg_pdf:bezier_c(PDF,{500,350},{400,350},{300,400}),
    eg_pdf:path(PDF,fill),
    eg_pdf:set_dash(PDF,dash),
    eg_pdf:set_fill_color(PDF,slateblue),
    eg_pdf:line(PDF,100,250,400,250),
    eg_pdf:poly(PDF,[{100,300},{150,350},{200,350},{250,300}]),
    eg_pdf:path(PDF,fill_stroke),
    eg_pdf:set_dash(PDF,solid),
    eg_pdf:set_stroke_color(PDF,khaki),
    eg_pdf:circle(PDF, {200,200}, 200),
    eg_pdf:path(PDF, stroke),
    eg_pdf:set_stroke_color(PDF, {16#00,16#FF,16#00}),
    eg_pdf:circle(PDF, {200,300}, 50),
    eg_pdf:path(PDF, stroke),
    eg_pdf:ellipse(PDF, {200,300}, {50,100}),
    eg_pdf:path(PDF, stroke),
    eg_pdf:ellipse(PDF, {200,300}, {100,50}),
    eg_pdf:path(PDF, stroke),
    eg_pdf:circle(PDF, {200,300}, 100),
    eg_pdf:path(PDF, stroke),
    eg_pdf:grid(PDF,[50,100,150],[600,700,800]),
    eg_pdf:round_rect(PDF,{300,600},{200,100},20),
    eg_pdf:path(PDF, stroke),
    eg_pdf:rectangle(PDF,{300,600},{200,100}, stroke),
    eg_pdf:new_page(PDF),
    colortest(PDF),
    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file("eg_test1.pdf",[Serialised]),
    eg_pdf:delete(PDF).

%% Write a DRAFT text rotated in the background
draft(PDF)->
    eg_pdf:save_state(PDF),
    pageno(PDF),
    eg_pdf:set_fill_gray(PDF,0.75),
    eg_pdf:rectangle(PDF, 100,800,410,5, fill),
    eg_pdf:rectangle(PDF, 100,42,410,5,fill_then_stroke),
    eg_pdf:translate(PDF,150,650),
    eg_pdf:mirror_yaxis(PDF,300),
    eg_pdf:rotate(PDF,300),
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF,"Helvetica-Bold", 160),
    eg_pdf:set_text_rendering(PDF, fill),
    eg_pdf:set_text_pos(PDF, 0,0),
    eg_pdf:textbr(PDF, "DRAFT"),
    eg_pdf:end_text(PDF),
    eg_pdf:restore_state(PDF).

pageno(PDF)->
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF,"Times-Roman", 11),
    A = eg_pdf:get_page_no(PDF),
    Str = "Page " ++ eg_pdf_op:n2s(A),
    Width = eg_pdf:get_string_width(PDF,"Times-Roman", 11, Str),
    case A rem 2 of
	0 ->
	    eg_pdf:set_text_pos(PDF, 100, 50);
	1 ->
	    eg_pdf:set_text_pos(PDF, 510 - Width, 50)
    end,
    eg_pdf:text(PDF, "Page " ++ eg_pdf_op:n2s(A)),
    eg_pdf:end_text(PDF).


colortest(PDF)->
    D=50,S=750, M=60,
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S),
    colortest1(PDF,0,[white,silver,gray,black,maroon,red,fuchsia,purple,lime,
		   green,olive,yellow,navy,blue,teal,aqua]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S-D),
    colortest1(PDF,0,[blue2,blue3,blue4,blueviolet,cornflowerblue,darkorchid,
		      darkslateblue,dodgerblue,lightskyblue,mediumblue, 
		      mediumpurple, mediumslateblue,midnightblue,purpleblue,
		     royalblue,skyblue2, slateblue]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S-2*D),
    colortest1(PDF,0,[aquamarine4,cadetblue,darkturquoise,lightblue,
		      lightseagreen,lightslategray,lightsteelblue,
		      mediumturquoise,paleturquoise,powderblue,skyblue,
		     steelblue,turquoise]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S-3*D),
    colortest1(PDF,0,[antiquewhite3,antiquewhite4,azure3, beige,darkslategray,
		     gainsboro,honeydew,slategray,thistle
		     ]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S-4*D),
    colortest1(PDF,0,[aquamarine,chartreuse,darkgreen,darkseagreen,forestgreen,
		     green2,green3,green4,greenyellow,lawngreen,limegreen,
		     mediumaquamarine,mediumseagreen,mediumspringgreen,
		     olivedrab,palegreen]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S-5*D),
    colortest1(PDF,0,[seagreen,springgreen,yellowgreen]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S-6*D),
    colortest1(PDF,0,[magenta,magenta2,magenta3,magenta4,mediumorchid,orchid,
		     plum,violet]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S-7*D),
    colortest1(PDF,0,[brown,burlywood,chocolate,coral,darkgoldenrod,darkorange,
		      darksalmon,deeppink,firebrick,gold,goldenrod,hotpink,
		      indianred,lightcoral,lightpink,lightsalmon]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S-8*D),
    colortest1(PDF,0,[maroon0,orange,orangered,palevioletred,peachpuff,peru,
		     pink,red2,red3,red4,rosybrown,salmon,sandybrown,sienna,
		     tomato,violetred]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S-9*D),
    colortest1(PDF,0,[aliceblue,azure,floralwhite,ghostwhite,ivory,lavender,
		     lavenderblush,lightcyan,lightyellow,linen,mintcream,
		     mistyrose,oldlace,seashell,snow,whitesmoke]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S-10*D),
    colortest1(PDF,0,[antiquewhite,bisque,blancedalmond,comsilk,darkkhaki,
		     darkolivegreen,khaki,lemonchiffon,lightgoldenrod,
		     lightgoldenrodyellow]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S-11*D),
    colortest1(PDF,0,[moccasin,palegoldenrod,papayawhip,
		     tan,wheat,yellow2,yellow3,yellow4]),

    eg_pdf:restore_state(PDF).

colortest1(_PDF,_N,[])->
    [];
colortest1(PDF,N,[H|T])->
    eg_pdf:set_fill_color(PDF,H),
    eg_pdf:rectangle(PDF,{0,20},{20,20}),
    eg_pdf:path(PDF,fill_stroke),
    eg_pdf:set_fill_color(PDF,black),
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF,"Times-Roman", 8),
    eg_pdf:set_text_pos(PDF,0,(N rem 2)*10),
    eg_pdf:text(PDF,atom_to_list(H)),
    eg_pdf:end_text(PDF),
    eg_pdf:translate(PDF,30,0),
    colortest1(PDF,N+1,T).
