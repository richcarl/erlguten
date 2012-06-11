%%==========================================================================
%% Copyright (C) 2003-2004 Joe Armstrong, Mikael Karlsson 
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
%%            Mikael Karlsson <mikael.karlsson@creado.com>
%% Purpose: PDF content stream operators
%%==========================================================================

-module(eg_pdf_op).

-export([arc/4,
         begin_text/0,
         bezier/4, bezier/8, bezier_c/3, bezier_v/2, bezier_y/2,
         break_text/0,
         circle/2,
         color/1,
         ellipse/2,
         end_text/0,
         get_string_width/3,
         grid/2,
         image/1, image/2, image/3,
         image1/2,
         kernedtext/1,
         line/1, line/2, line/4,
         lines/1,
         mirror_xaxis/1, mirror_yaxis/1,
         move_to/1,
         path/1,
         poly/1,
         rectangle/2, rectangle/3, rectangle/4, rectangle/5,
         restore_state/0,
         rotate/1,
         round_rect/3,
         save_state/0,
         scale/2,
         set_char_space/1,
         set_dash/1, set_dash/2,
         set_fill_color/1, set_fill_color_CMYK/4, set_fill_color_RGB/3,
         set_fill_gray/1,
         set_font/2,
         set_font_by_alias/2,
         set_image/3,
         set_line_cap/1,
         set_line_join/1,
         set_line_width/1,
         set_miter_limit/1,
         set_stroke_color/1, set_stroke_color_CMYK/4, set_stroke_color_RGB/3,
         set_stroke_gray/1,
         set_text_leading/1,
         set_text_pos/2,
         set_text_rendering/1,
         set_text_rise/1,
         set_text_scale/1,
         set_word_space/1,
         skew/2,
         text/1,
         textbr/1,
         transform/6,
         translate/2,

	 %% internal erlguten utility functions
	 i2s/1,
	 a2s/1,
	 n2s/1,
	 f2s/1,
	 flatten/1
	]).


%% Text commands

begin_text()->"\nBT\n".
    
end_text() -> "\nET\n".
     
break_text() -> " T*\n".
    
text(Text) ->
    A = escapePdfText(Text),
    ["(",A,")"," Tj\n"].
    
textbr(Text)-> [ text(Text), break_text() ].
    
kernedtext(Text)-> ["[ ",kernedtext1(Text)," ] TJ\n"].
    
kernedtext1([]) ->[];
kernedtext1([H|T]) when is_list(H)->  
    A = escapePdfText(H),
    ["(",A,") ",kernedtext1(T)];
kernedtext1([H|T]) when is_integer(H) ->
    [i2s(H)," ",kernedtext1(T)].

set_text_pos(X, Y) -> [ n2s([X,Y]), " Td " ].
		      
set_text_leading(L) -> [n2s(L)," TL "].


set_text_rendering(fill)->
    set_text_rendering(0);
set_text_rendering(stroke) ->
    set_text_rendering(1);
set_text_rendering(fill_then_stroke) ->
    set_text_rendering(2);
set_text_rendering(MODE) when is_integer(MODE)->
    [i2s(MODE)," Tr\n"].

set_char_space(CS)      -> [n2s(CS)," Tc "].
    
set_word_space(WS) -> [n2s(WS)," Tw "].

set_text_scale(SC) -> [i2s(SC)," Tz "].

set_text_rise(RISE)-> [i2s(RISE)," Ts "].

set_font_by_alias(FontAlias, Size) ->
    	    [" /",FontAlias, " ",i2s(Size)," Tf "].

set_font(Name, Size) ->
    case eg_font_map:handler(Name) of
	undefined ->
	    set_font("Times-Roman", Size);
	M ->
	    [" /F", i2s(M:index()), " ",i2s(Size)," Tf "]
    end.

%% -- This function is a bit expensive, but will stick to the public interface.
get_string_width(Fontname, PointSize, Str)->
    {richText, Inline} = eg_richText:str2richText(Fontname, PointSize, 0,
                                                  default, true, Str),
    trunc(lists:foldl(fun(A, Accu) -> eg_richText:width(A) + Accu end,
                      0, Inline)
          / 1000).

%% Graphics operators
    
path(close)                      -> " h ";
path(stroke)                     ->" S ";
path(close_stroke)               ->" s ";
path(fill)                       -> " f ";
path(fill_even_odd)              ->" f* ";
path(fill_stroke)                ->" B ";
path(fill_then_stroke)           ->" B "; %% Same as above, backwards compat.
path(fill_stroke_even_odd)       ->" B* ";
path(close_fill_stroke)          ->" b ";
path(close_fill_stroke_even_odd) ->" b* ";
path(endpath)                    -> " n ".

move_to({X,Y})->[n2s([X,Y])," m "].
    

line({FromP,ToP})->
    [move_to(FromP), line_append(ToP), " S\n"].

line({X1,Y1},{X2, Y2})->
    line({{X1,Y1},{X2,Y2}}).

line(X1,Y1,X2,Y2)->
    line({{X1,Y1},{X2,Y2}}).

line_append({X1,Y1})-> 
    [n2s([X1,Y1])," l "].
    

lines(LineList)-> 
    lists:map( fun(A) -> line(A) end, LineList ).

%% Poly paths should be stroked/closed/filled with separate
%% command.
poly([{X1,Y1}|PolyList])->
    [ move_to({X1,Y1}) | poly1(PolyList)].

poly1(PolyList) ->
    lists:map ( fun(A) -> line_append(A) end, PolyList ).

%% Grid assumes sorted XLists and YList, minimum value first
grid(XList, YList)->
    XMin = hd(XList),
    XMax = lists:nth(length(XList),XList),
    YMin = hd(YList),
    YMax = lists:nth(length(YList),YList),
    [
     lines([{{XMin,Y}, {XMax,Y}}|| Y <- YList ]),
     lines([{{X,YMin}, {X,YMax}}|| X <- XList ])
    ].

%% Bezier paths should be stroked/closed/filled with separate
%% command.
bezier(X1,Y1,X2,Y2,X3,Y3,X4,Y4)->
    bezier({X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4}).

bezier(Point1, Point2, Point3, Point4)->
    [move_to(Point1), bezier_c( Point2, Point3, Point4 ) ].

bezier_c({X1,Y1},{X2,Y2},{X3,Y3})->
    [n2s([X1,Y1,X2,Y2,X3,Y3]), " c\n"].

bezier_v({X2,Y2}, {X3,Y3})-> 
    [n2s([X2,Y2,X3,Y3]), " v\n"].

bezier_y({X1,Y1}, {X3,Y3})->
    [n2s([X1,Y1,X3,Y3]), " y\n"].
    

arc(_X1,_Y1,_X2,_Y2)->
    tobedone.

circle({X,Y}, R)->
    ellipse({X,Y}, {R,R}).
    
ellipse({X, Y}, {RX, RY})->
  [ move_to({X+RX, Y}),
    bezier_c({X+RX,       Y+RY*mpo()}, {X+RX*mpo(), Y+RY      }, {X,    Y+RY}),
    bezier_c({X-RX*mpo(), Y+RY      }, {X-RX,       Y+RY*mpo()}, {X-RX, Y   }),
    bezier_c({X-RX,       Y-RY*mpo()}, {X-RX*mpo(), Y-RY      }, {X,    Y-RY}),
    bezier_c({X+RX*mpo(), Y-RY      }, {X+RX,       Y-RY*mpo()}, {X+RX, Y   })
  ] .

mpo()-> 0.552284749.   %% MultiPlier Object :-)


%% If Stroke Type is not appended in arguments, explicit
%% stroke command "path(StrokeType)" has to be executed.
rectangle({X,Y},{WX,WY})->
    rectangle(X,Y,WX,WY).
      
rectangle({X,Y},{WX,WY}, StrokeType) ->
    rectangle(X,Y,WX,WY,StrokeType).

rectangle(X,Y,WX,WY) ->
    [n2s([X,Y,WX,WY])," re "].

rectangle(X,Y,WX,WY,StrokeType)->
    [rectangle(X,Y,WX,WY), path(StrokeType) ].


round_rect({X,Y}, {W, H}, R) ->
    [ move_to({X+R,Y}),
      line_append({X+W-R, Y}),
      bezier_c( {X+W-R+R*mpo(), Y}, {X+W, Y+R*mpo()}, {X+W,Y+R} ),
      line_append({X+W,Y+H-R}),
      bezier_c( {X+W, Y+H-R+R*mpo()}, {X+W-R+R*mpo(), Y+H}, {X+W-R, Y+H}),
      line_append({X+R, Y+H}),
      bezier_c( {X+R*mpo(), Y+H}, {X, Y+H-R+R*mpo()}, {X, Y+H-R} ),
      line_append({X, Y+R}),
      bezier_c( {X, Y+R*mpo()}, {X+R*mpo(), Y}, {X+R, Y} )
     ].


%% Line styles
set_line_width(W) -> [n2s(W)," w\n"].

set_line_cap(flat_cap)-> " 0 J\n";
set_line_cap(round_cap) -> " 1 J\n";
set_line_cap(square_cap) -> " 2 J\n";
set_line_cap(Mode) -> n2s(Mode) ++ " J\n".
     
set_line_join(miter_join) -> " 0 j\n";
set_line_join(round_join) -> " 1 j\n";
set_line_join(bevel_join) -> " 2 j\n";
set_line_join(Mode) -> n2s(Mode) ++ " j\n".
    
set_miter_limit(Limit)-> [n2s(Limit)," M\n"].
    
set_dash(Array, Phase) ->
    ["[",n2s(Array),"] ",n2s(Phase)," d\n"].

set_dash(solid)   -> set_dash([],0);
set_dash(dash)    -> set_dash([6,2],0);
set_dash(dot)     -> set_dash([2],0);
set_dash(dashdot) -> set_dash([6,2,2,2],0);
set_dash(A) when is_list(A) -> set_dash(A,0);
set_dash(_)       -> set_dash([],0).    %% Fall back to solid line


%% Graphics state
save_state() -> 
    "\nq\n".
    
restore_state() ->
    "\nQ\n".

%% Change geometry
transform(A, B, C, D, E, F)->
    [n2s([A,B,C,D,E,F])," cm\n"].

translate(X,Y)->
    transform(1,0,0,1,X,Y).

scale(ScaleX, ScaleY) when is_integer(ScaleX), is_integer(ScaleY)->
    transform(ScaleX, 0, 0, ScaleY, 0, 0).

rotate(90)->  " 0 1 -1 0 0 0 cm\n";
rotate(180)-> " 1 0 0 1 0 0 cm\n";
rotate(270)-> " 0 -1 1 0 0 0 cm\n";
rotate(Angle)->
    RadianAngle = Angle * math:pi()/180, 
    C = math:cos( RadianAngle ),
    S = math:sin( RadianAngle ),
    transform(C, S, -S, C, 0, 0).


skew(XScewAngle, YScewAngle)->
    TanA = math:tan(XScewAngle * math:pi()/180),
    TanB = math:tan(YScewAngle * math:pi()/180),
    transform(1, TanA, TanB, 1, 0, 0).

mirror_yaxis(Xtranslate)->
    [ translate(Xtranslate,0), scale(-1,1) ].

mirror_xaxis(Ytranslate)->
    [ translate(0,Ytranslate), scale(1,-1) ].

%% Changing colors
%% Color value range 0 - 1
set_fill_color_CMYK(C,M,Y,K)->
    [n2s([C,M,Y,K])," k\n"].

set_stroke_color_CMYK(C,M,Y,K)->
    [n2s([C,M,Y,K])," K\n"].

%% Color value range 0 - 1
set_fill_color_RGB(R,G,B)->
    [n2s([R,G,B])," rg\n"].

set_stroke_color_RGB(R,G,B)->
    [n2s([R,G,B]), " RG\n"].

%% Color is Name |{R,G,B}, Name = atom(), 0 < R,G,B < 255 
set_fill_color(Color)->
    {R,G,B} = color(Color),
    set_fill_color_RGB(R/255,G/255,B/255).

set_stroke_color(Color)->
    {R,G,B} = color(Color),
    set_stroke_color_RGB(R/255,G/255,B/255).


%% HTML standard
color(white)  ->{16#FF,16#FF,16#FF};
color(silver) ->{16#C0,16#C0,16#C0};
color(gray)   ->{16#80,16#80,16#80};
color(black)  ->{16#00,16#00,16#00};
color(maroon) ->{16#80,16#00,16#00};
color(red)    ->{16#FF,16#00,16#00};
color(fuchsia)->{16#FF,16#00,16#FF};
color(purple) ->{16#80,16#00,16#80};
color(lime)   ->{16#00,16#FF,16#00};
color(green)  ->{16#00,16#80,16#00};
color(olive)  ->{16#80,16#80,16#00};
color(yellow) ->{16#FF,16#FF,16#00};
color(navy)   ->{16#00,16#00,16#80};
color(blue)   ->{16#00,16#00,16#FF};
color(teal)   ->{16#00,16#80,16#80};
color(aqua)   ->{16#00,16#FF,16#FF};

%% Nice to have colours, collected from
%% http://vela.astro.ulg.ac.be/Vela/Colors/rgb.html

%% blue  B-R>32, B-G>32
color(blue2)            -> {16#00,16#00,16#EE};
color(blue3)            -> {16#00,16#00,16#CD};
color(blue4)            -> {16#00,16#00,16#8B};
color(blueviolet)       -> {16#8A,16#2B,16#E2};
color(cornflowerblue)   -> {16#64,16#95,16#ED};
color(darkorchid)       -> {16#99,16#32,16#CC};
color(darkslateblue)    -> {16#48,16#3D,16#8B};
color(darkviolet)       -> {16#94,16#00,16#D3};
color(deepskyblue)      -> {16#00,16#BF,16#FF};
color(dodgerblue)       -> {16#1E,16#90,16#FF};
color(lightskyblue)     -> {16#87,16#CE,16#FA};
color(mediumblue)       -> {16#00,16#00,16#CD};
color(mediumpurple)     -> {16#93,16#70,16#DB};
color(mediumslateblue)  -> {16#7B,16#68,16#EE};
color(midnightblue)     -> {16#19,16#19,16#70};
color(purpleblue)       -> {16#A0,16#20,16#F0};
color(royalblue)        -> {16#41,16#69,16#E1};
color(skyblue2)         -> {16#7E,16#C0,16#EE};
color(slateblue)        -> {16#6A,16#5A,16#CD};

%% cyan abs(B-G)>33
color(aquamarine4)      -> {16#45,16#8B,16#74};
color(cadetblue)        -> {16#5F,16#9E,16#A0};
color(darkturquoise)    -> {16#00,16#CE,16#D1};
color(lightblue)        -> {16#AD,16#D8,16#E6};
color(lightseagreen)    -> {16#20,16#B2,16#AA};
color(lightslategray)   -> {16#77,16#88,16#99};
color(lightsteelblue)   -> {16#B0,16#C4,16#DE};
color(mediumturquoise)  -> {16#48,16#D1,16#CC};
color(paleturquoise)    -> {16#AF,16#EE,16#EE};
color(powderblue)       -> {16#B0,16#E0,16#E6};
color(skyblue)          -> {16#87,16#CE,16#EB};
color(steelblue)        -> {16#46,16#82,16#B4};
color(turquoise)        -> {16#40,16#E0,16#D0};

%% gray abs(R-G)<33, abs(R-B)<33, abs(B-G)<33
color(antiquewhite3)    -> {16#CD,16#C0,16#B0};
color(antiquewhite4)    -> {16#8B,16#83,16#78};
color(azure3)           -> {16#C1,16#CD,16#CD};
color(beige)            -> {16#F5,16#F5,16#DC};
color(darkslategray)    -> {16#2F,16#4F,16#4F};
color(gainsboro)        -> {16#DC,16#DC,16#DC};
color(honeydew)         -> {16#F0,16#FF,16#F0};
color(slategray)        -> {16#70,16#80,16#90};
color(thistle)          -> {16#D8,16#BF,16#D8};
color(lightgray)        -> {16#D3,16#D3,16#D3};

%% green  G-R>32, G-B>32
color(aquamarine)       -> {16#7F,16#FF,16#D4};
color(chartreuse)       -> {16#7F,16#FF,16#00};
color(darkgreen)        -> {16#00,16#64,16#00};
color(darkseagreen)     -> {16#8F,16#BC,16#8F};
color(forestgreen)      -> {16#22,16#8B,16#22};
color(green2)           -> {16#00,16#EE,16#00};
color(green3)           -> {16#00,16#CD,16#00};
color(green4)           -> {16#00,16#8B,16#00};
color(greenyellow)      -> {16#AD,16#FF,16#2F};
color(lawngreen)        -> {16#7C,16#FC,16#00};
color(limegreen)        -> {16#32,16#CD,16#32};
color(mediumaquamarine) -> {16#66,16#CD,16#AA};
color(mediumseagreen)   -> {16#3C,16#B3,16#71};
color(mediumspringgreen)-> {16#00,16#FA,16#9A};
color(olivedrab)        -> {16#6B,16#8E,16#23};
color(palegreen)        -> {16#98,16#FB,16#98};
color(seagreen)         -> {16#4E,16#8B,16#57};
color(springgreen)      -> {16#00,16#FF,16#7F};
color(yellowgreen)      -> {16#9A,16#CD,16#32};

%% Magenta  abs(R-B) < 33
color(magenta)          -> {16#FF,16#00,16#FF}; %% (html fuchsia)
color(magenta2)         -> {16#EE,16#00,16#EE};
color(magenta3)         -> {16#CD,16#00,16#CD};
color(magenta4)         -> {16#8B,16#00,16#8B};
color(mediumorchid)     -> {16#BA,16#55,16#D3};
color(orchid)           -> {16#DA,16#70,16#D6};
color(plum)             -> {16#DD,16#A0,16#DD};
color(violet)           -> {16#EE,16#82,16#EE};

%% Red   R-G>32, R-B>32 
color(brown)            -> {16#A5,16#2A,16#2A};
color(burlywood)        -> {16#DE,16#B8,16#87};
color(chocolate)        -> {16#D2,16#69,16#1E};
color(coral)            -> {16#FF,16#7F,16#50};
color(darkgoldenrod)    -> {16#B8,16#86,16#0B};
color(darkorange)       -> {16#FF,16#8C,16#00};
color(darksalmon)       -> {16#E9,16#96,16#7A};
color(deeppink)         -> {16#FF,16#14,16#93};
color(firebrick)        -> {16#B2,16#22,16#22};
color(gold)             -> {16#FF,16#D7,16#00};
color(goldenrod)        -> {16#DA,16#A5,16#20};
color(hotpink)          -> {16#FF,16#69,16#B4};
color(indianred)        -> {16#CD,16#5C,16#5C};
color(lightcoral)       -> {16#F0,16#80,16#80};
color(lightpink)        -> {16#FF,16#B6,16#C1};
color(lightsalmon)      -> {16#FF,16#A0,16#7A};
color(maroon0)          -> {16#B0,16#30,16#60};
color(orange)           -> {16#FF,16#A5,16#00};
color(orangered)        -> {16#FF,16#45,16#00};
color(palevioletred)    -> {16#DB,16#70,16#93};
color(peachpuff)        -> {16#FF,16#DA,16#B9};
color(peru)             -> {16#CD,16#85,16#3F};
color(pink)             -> {16#FF,16#C0,16#CB};
color(red2)             -> {16#EE,16#00,16#00};
color(red3)             -> {16#CD,16#00,16#00};
color(red4)             -> {16#8B,16#00,16#00};
color(rosybrown)        -> {16#BC,16#8F,16#8F};
color(salmon)           -> {16#FA,16#80,16#72};
color(sandybrown)       -> {16#F4,16#A4,16#60};
color(sienna)           -> {16#A0,16#52,16#2D};
color(tomato)           -> {16#FF,16#63,16#47};
color(violetred)        -> {16#D0,16#20,16#90};

%% White R > 223, G > 223, B > 223
color(aliceblue)        -> {16#F0,16#F8,16#FF};
color(azure)            -> {16#F0,16#FF,16#FF};
color(floralwhite)      -> {16#FF,16#FA,16#F0};
color(ghostwhite)       -> {16#F8,16#F8,16#FF};
color(ivory)            -> {16#FF,16#FF,16#F0};
color(lavender)         -> {16#E6,16#E6,16#FA};
color(lavenderblush)    -> {16#FF,16#F0,16#F5};
color(lightcyan)        -> {16#E0,16#FF,16#FF};
color(lightyellow)      -> {16#FF,16#FF,16#E0};
color(linen)            -> {16#FA,16#F0,16#E6};
color(mintcream)        -> {16#F5,16#FF,16#FA};
color(mistyrose)        -> {16#FF,16#E4,16#E1};
color(oldlace)          -> {16#FD,16#F5,16#E6};
color(seashell)         -> {16#FF,16#F5,16#EE};
color(snow)             -> {16#FF,16#FA,16#FA};
color(whitesmoke)       -> {16#F5,16#F5,16#F5};

%% Yellow  abs(R-G) < 33
color(antiquewhite)     -> {16#FA,16#EB,16#D7};
color(bisque)           -> {16#FF,16#E4,16#DB};
color(blancedalmond)    -> {16#FF,16#EB,16#CD};
color(comsilk)          -> {16#FF,16#F8,16#DC};
color(darkkhaki)        -> {16#BD,16#B7,16#6B};
color(darkolivegreen)   -> {16#55,16#6B,16#2F};
color(khaki)            -> {16#F0,16#E6,16#8C};
color(lemonchiffon)     -> {16#FF,16#FA,16#CD};
color(lightgoldenrod)   -> {16#EE,16#DD,16#82};
color(lightgoldenrodyellow) -> {16#FA,16#FA,16#FA};
color(moccasin)         -> {16#FF,16#E4,16#B5};
color(palegoldenrod)    -> {16#EE,16#E8,16#AA};
color(papayawhip)       -> {16#FF,16#EF,16#D5};
color(tan)              -> {16#D2,16#B4,16#8C};
color(wheat)            -> {16#F5,16#DE,16#B3};
color(yellow2)          -> {16#EE,16#EE,16#00};
color(yellow3)          -> {16#CD,16#CD,16#00};
color(yellow4)          -> {16#8B,16#8B,16#00};
color({R,G,B})          -> {R,G,B}.
			   
    
%% color(_) ->{0,0,0}.
    
%% Gray 0.0-Black 1.0-White)
set_fill_gray(Gray)->
    [n2s(Gray)," g\n"].

set_stroke_gray(Gray)->
    [n2s(Gray)," G\n"].


%% Image
set_image(Width, Height, IMGName)->
    [ n2s([Width, 0, 0, Height, 0, 0]), " cm\n", %% scale
		  " /", IMGName, " Do\n" ].      %% Draw image


%% image1(...) (and therefore image(...) ) appear to be non-working
%% eg_pdf_assemble:pdfloop(...) code is never run.

%% Images
%% image(PID, FilePath )
%% image(PID, FilePath, Size)
%% image(PID, FilePath, Pos, Size)
%% Pos is {X,Y}
%% Size is {width, W} | {height, H} | {W,H}

image(FilePath)->
    save_state(),
    image1(FilePath, {size,{undefined,undefined}}),
    restore_state().
    
image(FilePath,Size)->
    save_state(),
    image1(FilePath, Size),
    restore_state().
    
image(FilePath, {X,Y}, Size) when is_integer(X), is_integer(Y)->
    save_state(),
    translate(X,Y),
    image1(FilePath, Size),
    restore_state().

image1(FilePath, {width, W})->
    image1(FilePath, {size,{W,undefined}});
image1(FilePath, {height, H}) ->
    image1(FilePath, {size,{undefined,H}});
image1(FilePath, {W, H}) when is_integer(W), is_integer(H)->
    image1(FilePath, {size,{W,H}});
image1(_FilePath, {size,_Size})->
%%    PID ! {image, FilePath, Size}.
    to_be_done.

%% ----------------------------------------------------------------------------
%% Internals

escapePdfText([]) -> [];
escapePdfText([$(|Rest]) -> [$\\,$( | escapePdfText(Rest)];
escapePdfText([$)|Rest]) -> [$\\,$) | escapePdfText(Rest)];
escapePdfText([$\\|Rest]) -> [$\\,$\\ | escapePdfText(Rest)];
escapePdfText([C|Rest]) when is_list(C)-> [ escapePdfText(C) |
					    escapePdfText(Rest)];
escapePdfText([C|Rest]) -> [ C | escapePdfText(Rest)].

i2s(I) ->
    integer_to_list(I).

a2s(A) ->
    atom_to_list(A).

n2s(A) when is_float(A)   -> f2s(A);
n2s(A) when is_integer(A) -> i2s(A);
n2s([])                   -> [];
n2s([H|T])                -> [n2s(H)," "|n2s(T)].

f2s(I) when is_integer(I) ->
    i2s(I);
f2s(F) ->    
    remove_leading_blanks(flatten(io_lib:format("~8.2f", [F]))).

remove_leading_blanks([$\s|T]) -> remove_leading_blanks(T);
remove_leading_blanks(X)       -> X.

flatten(L) ->
    binary_to_list(list_to_binary(L)).
