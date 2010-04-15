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
%% Last Edit: 2003-03-11
%% Purpose: Test cases
%%==========================================================================


%% This is for the brave who want to try writing PDF by hand
%% The ouput of this program is a total mess :-)
%% To run this program evaluate
%%  eg_test2:test() - this makes a two page PDF document
%%                 called default.pdf


-module(eg_test2).

-include("../src/eg.hrl").

-export([test/0]).


%% ============================================================================
    
test() ->
    Info = #info{creator="Erlang", 
		 creationDate={{2003,2,15},{15,20,11}},
		 producer="mkPdf", 
		 author="",
		 title="",
		 subject="",
		 keywords="ErlangKeyword"},
    Fonts = ["Times-Roman", "Times-Italic", "Courier"],
    %% = FR FI FC
    Pages = [page(1),page(2),page(3)], 
    Pages1 = zap(Pages),
    MediaBox={0,0,595,842},
    Fonts1 = lists:map(fun(I) -> eg_font_map:handler(I) end, Fonts),
    eg_pdf_assemble:make_pdf_file("eg_test2.pdf",
					Info, Fonts1, Pages1, MediaBox).

zap(Pages) ->
    FR = cmd("Times-Roman"),
    FI = cmd("Times-Italic"),
    FC = cmd("Courier"),
    lists:map(fun({page,I}) -> {page, xform(I, FR, FI, FC)} end, Pages).

cmd(F) ->
    M = eg_font_map:handler(F),
    "/F" ++ eg_pdf_op:i2s(M:index()) ++ " ".

xform("/FR " ++ T, FR, FI, FC) -> FR ++ xform(T, FR, FI, FC);
xform("/FI " ++ T, FR, FI, FC) -> FI ++ xform(T, FR, FI, FC);
xform("/FC " ++ T, FR, FI, FC) -> FC ++ xform(T, FR, FI, FC);
xform([H|T], FR, FI, FC)       -> [H|xform(T, FR, FI, FC)];
xform([], _, _, _) -> [].

%% Two pages of PDF
page(1) ->	       	
    {page, 
"
0.6000 g 0.6000 G
20 750 m 100 750 l S
BT
  /FI 240 Tf 
-1.0000 -0.0000 0.0000 -1.0000 600 600 Tm  (Wow)Tj
 
  /FR 14 Tf
  1 0 0 1 20 750 Tm
  18    0 Td (Line 1 indent 36 point) Tj
  -18 -16 Td (line 2 indebt 0) Tj
  0   -16 Td (line 3 indent 0) Tj
  0   -16 Td (line 4 indent 0) Tj
  12  -16 Td (line 5 indent 12 points) Tj
  -12 -16 Td (line 6 indent 0 ) Tj

ET
"
};
%% page 2 has a lot of junk so I can check my 
%% computation of units is correct
page(2) ->
    {page,
"
BT

%% width of a=444 b=500  space=250
%% (aaa bbb) = 3*444+3*500+250 = 3082*16/1000 = 49.31 (50+49.31) = 99.31
        
/FR 16 Tf 18 TL 1 0 0 1 50 450 Tm [(aaa bbb) 0 ] TJ

ET

50 475 m 
50 400 l
99.31 400 l 
99.31 475 l S

%% same as above (width = 3082-300) = 2782*16/1000 = 44.51 + 200 = 244.51

BT
/FR 16 Tf 18 TL 1 0 0 1 200 450 Tm [(aa)300(a) ( bbb) 0 ] TJ
ET

200 475 m 
200 400 l
244.51 400 l 
244.51 475 l S

%% normal width = 3082 (extra) = 
%% total = 3082 + 880 = 3962 => *16/1000 = 63.39 + 300 = 363.39

BT
/FR 16 Tf 18 TL 1 0 0 1 300 450 Tm 52 Tw [(aaa  bbb) 0 ] TJ
ET

%% Calculate (aaa  bbb) = 3*444+3*500+2*250
%% = 3332 *16/1000 = 53.31 + 2*52 = 157.31

300 475 m 
300 400 l
457.31 400 l 
457.31 475 l S

BT

  /FR 24  Tf 20 750 Td  (Times-Roman 24 pt) Tj 
  26 TL T*
  /FI 16 Tf   (Times-Italic 16)Tj
  T*
  /FC 6 Tf   (Courier 6)Tj
  T*
  /FR 12 Tf (The tuple) Tj 
  /FI 12 Tf ( person, ) Tj
  /FR 12 Tf (for example, is written,) Tj
  /FC 12 Tf ({person,\"joe\"}) Tj
  /FR 12 Tf ( and represents a person.) Tj


/FR 16 Tf
18 TL
1 0 0 1 50 650 Tm
(Hello) Tj
T*
(Joe) Tj
0.60000 g
0.60000 G

1 0 0 1 100 600 Tm
(100 x 600 0.6 gray text) Tj

1 0 0 rg
1 0 0 RG

1 0 0 1 150 550 Tm
(150 x 550 red text) Tj

0.9659 0.2588 -0.2588 0.9659 100 500 Tm
(100x400 red text with 15 degree rotation) Tj

-1 0 0 -1 200 500 Tm
(Upside down text) Tj

0 0 1 rg
0 0 1 RG
0 1 -1 0 30 500 Tm
(90 degree blue text) Tj

0 1 0 rg
0 1 0 RG

1 0 0 1 350 650 Tm
(Testing Hello World - abcWAdef   ) Tj
T*
[ (T) 70 (esting) (T) 300 (esting)  -3000 (abcW) 120 (Adef.) ] TJ
 T*
   1 Tr
   (PDF )Tj
   -5 Ts (is ) Tj
   -10 Ts (really ) Tj
   -15 Ts (fun ) Tj
   -20 Ts (stuff ) Tj
ET

450 720 m
150 720 l
S      

1.0 0.0  0.0 RG
0 1 0  rg

500 700 50 75 re B

"
};
page(3) ->
    {page,
"
% experiment with text rotation and special effects
% NOTE xpdf does not display this correctly acrobat does :-)
BT

/FR 80 Tf 

% translate to 50 650
1 0 0 1 50 650 Tm (ABC)Tj

/FC 60 Tf 
% translate 300 650 scale x by 0.2
1 0 0 1 20 800 Tm (courier unscaled) Tj
0.8 0 0 1 20 750 Tm (courier scaled)Tj

/FR 80 Tf 

% 10 degree rotation cos(10) = 0.9848 sin(10)=0.1736
% matric = cos sin -sin cos X Y

0.9848 0.1736 -0.1736 0.9848 50 550 Tm (ROT 10) Tj

%%  10 degress skew
%%  tan(10) = 0.176327
% matrix = 1 tan(A) tan(B) 1 0 0]

1 0.1763 0.1763 1 50 480 Tm (Skew 10) Tj

-1 0 0 1 300 400 Tm (Mirror) Tj

-1 0 0 1 300 320 Tm (rorriM) Tj

1 0 0 1 300 500 Tm 1 0 0 rg (Red) Tj 0 0 0 rg (Black) Tj

ET
"
}.






