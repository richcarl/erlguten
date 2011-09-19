%%==========================================================================
%% Copyright (C) 2004 Johan Bevemyr
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
%% Author: Johan Bevemyr <jb@son.bevemyr.com>
%%==========================================================================

-module(kd_test1).

-export([test/0]).

-define(OCRFONT, "OCR-B-Digits").
-define(OCRSIZE, 10).

test() ->
    OCR = "25042950342",
    INr = OCR,
    IDate = "2004-12-28",
    CNr = "6612221454",
    Shipping = "29,00",
    Total = 14471.00,
    Vat = "45,60",
    AtLatest = "2004-12-29",
    ONr = "194587",
    ODate = "2004-11-30",
    Items = [{"1543245","Krall Diana - Live At The Montreal Jazz Festival",
	      "1", "199,00", "199,99"},
	     {"7833","Alison Krauss - New Favorite",
	      "2", "39,00", "78,00"}],
    BGNr = "237-4825",
    test({"Johan Bevemyr",
	  "Gösta Tamms väg 16",
	  "191 33 Sollentuna"},
	 INr,IDate,CNr,
	 ONr, ODate,
	 AtLatest,
	 Items,
	 Shipping,
	 Total,
	 Vat, OCR, BGNr).
	 
test(Address, Nr, Date, PNr, ONr, ODate, ByDate, Items,
     Shipping, Amount, Vat, OCR, BGNr)->
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,a4),
    eg_pdf:set_author(PDF,"Kreditor AB"),
    eg_pdf:set_title(PDF, "Faktura"),
    eg_pdf:set_subject(PDF,"Faktura"),
    eg_pdf:set_keywords(PDF,"Faktura"),

    eg_pdf:set_page(PDF,1),

    %% grid
    % grid(PDF),

    %% ebutikens logga
    their_logo(PDF, 400, 780),

    %% vår logga
    our_logo(PDF, 400, 750),

    %% allmän info, org nr etc
    our_info(PDF, BGNr, 400, 730),

    %% Address till kund
    address(PDF, Address, 50, 700),
    
    %% Faktura, Faktura nr, Datum, personnummer
    invoice_nr(PDF, Nr, Date, PNr, ONr, ODate, 35, 800),

    %% Box med spec
    eg_pdf:set_line_width(PDF, 2),
    eg_pdf:rectangle(PDF,{35,350},{515,250}, stroke),
    eg_pdf:set_line_width(PDF, 1),
    eg_pdf:line(PDF,  35, 583, 550, 583),

%     eg_pdf:line(PDF,  80, 600,  80, 350),
%     eg_pdf:line(PDF, 420, 600, 420, 350),
%     eg_pdf:line(PDF, 455, 600, 455, 350),
%     eg_pdf:line(PDF, 485, 600, 485, 350),

    %% Rubriker
    B0 = 587,
    eg_pdf:set_font(PDF, "Helvetica-Bold", 8),
    eg_pdf:moveAndShow(PDF, 40, B0, to_mac("Artikelnr")),
    eg_pdf:moveAndShow(PDF, 90, B0, to_mac("Artikelbenämning")),
    eg_pdf:moveAndShow(PDF, 430, B0, to_mac("Antal")),
    eg_pdf:moveAndShow(PDF, 468, B0, [8#210|"-pris"]),
    eg_pdf:moveAndShow(PDF, 512, B0, to_mac("Belopp")),

    %% Artiklar
    items(PDF, Items, 570, 40, 90, 450, 490, 540),

    %% Senast tillhanda
    at_latest(PDF, ByDate, 35, 335),

    %% Betalningssätt
    method_of_pay(PDF, "Faktura", 35, 320),

    %% OCR
    use_ocr(PDF, OCR, 35, 305),

    %% Frakt
    shipping(PDF, 400, 335, Shipping),

    %% Summa
    total(PDF, 400, 320, Amount),

    %% Moms
    vat(PDF, 400, 305, Vat),

    X = 0,

    %% Add AVI
    bg_avi(PDF, X, 0),

    %% fill  it in
    bg_avi_bgnr(PDF, X, 0, BGNr),

    %% fill  it in
    bg_avi_recipient(PDF, X, 0, "Kreditor AB"),

    %% fill  it in
    bg_avi_from(PDF, X, 0, Address),

    %% fill  it in
    bg_avi_ocrline(PDF, 0, 0, BGNr, Amount, OCR),

    %% fill  it in
    bg_avi_lastdate(PDF, X, 0, ByDate),

    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file("kd_test1.pdf",[Serialised]),
    eg_pdf:delete(PDF).


%% grid(PDF) ->
%%     eg_pdf:save_state(PDF),
%%     eg_pdf:set_fill_gray(PDF,0.75),
%%     eg_pdf:set_stroke_gray(PDF,0.75),
%%     eg_pdf:show_grid(PDF, a4),
%%     eg_pdf:restore_state(PDF).


their_logo(PDF, X, Y) ->
    eg_pdf:image(PDF,'ebutik.jpg',{X,Y},{width, 150}).


our_logo(PDF, X, Y) ->
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF, "Victorias-Secret", 20),
    eg_pdf:set_text_pos(PDF, X, Y),
    eg_pdf:textbr(PDF, "Kreditor AB"),
    eg_pdf:end_text(PDF).
    

address(PDF, {Name, Street, City}, X, Y) ->
    eg_pdf:set_font(PDF, "Courier", 12),
    eg_pdf:moveAndShow(PDF, X, Y, to_mac(Name)),
    eg_pdf:moveAndShow(PDF, X, Y-20, to_mac(Street)),
    eg_pdf:moveAndShow(PDF, X, Y-40, to_mac(City)).


our_info(PDF, BGNr, X, Y) ->
    eg_pdf:set_font(PDF, "Helvetica", 7),
    eg_pdf:moveAndShow(PDF,X,Y,
                           "Kreditor AB, Box XXX, 123 34 Stockholm"),
    eg_pdf:moveAndShow(PDF,X,Y-10,
                           "Org nr 556021-4566 (F-skattebevis)"),
    eg_pdf:moveAndShow(PDF,X,Y-20, "Momsreg nr SE556354321345"),
    eg_pdf:moveAndShow(PDF,X,Y-30,
                        "Betala till: BG "++BGNr++" PG 474 70 02-6").

invoice_nr(PDF, Nr, Date, PNr, ONr, ODate, X, Y) ->
    eg_pdf:set_font(PDF, "Helvetica-Bold", 15),
    eg_pdf:moveAndShow(PDF, X, Y, "FAKTURA"),
    eg_pdf:set_font(PDF, "Helvetica", 10),
    eg_pdf:moveAndShow(PDF, X, Y-20, "Datum"),
    eg_pdf:moveAndShow(PDF, X+70, Y-20, "Kundnr"),
    eg_pdf:moveAndShow(PDF, X+70+70+25, Y-20, "Fakturanr"),

    eg_pdf:set_font(PDF, "Courier", 10),
    eg_pdf:moveAndShow(PDF, X, Y-30, Date),
    eg_pdf:moveAndShow(PDF, X+70, Y-30, PNr),
    eg_pdf:moveAndShow(PDF, X+70+70+25, Y-30, Nr),

    eg_pdf:set_font(PDF, "Helvetica", 10),
    eg_pdf:moveAndShow(PDF, X, Y-50, "Orderdatum"),
    eg_pdf:moveAndShow(PDF, X+70, Y-50, "Ordernr"),

    eg_pdf:set_font(PDF, "Courier", 10),
    eg_pdf:moveAndShow(PDF, X, Y-60, ODate),
    eg_pdf:moveAndShow(PDF, X+70, Y-60, ONr).


use_ocr(PDF, OCR, X, Y) ->
    eg_pdf:set_font(PDF, "Helvetica", 9),
    Str = to_mac("Ange alltid OCR-numret: "),
    eg_pdf:moveAndShow(PDF, X, Y, Str),
    Width = eg_pdf:get_string_width(PDF, "Helvetica", 9, Str),
    eg_pdf:set_font(PDF, ?OCRFONT, ?OCRSIZE),
    eg_pdf:moveAndShow(PDF, X+Width+15, Y, OCR).

method_of_pay(PDF, Method, X, Y) ->
    eg_pdf:set_font(PDF, "Helvetica", 9),
    eg_pdf:moveAndShow(PDF, X, Y, to_mac("Betalningssätt:  "++Method)).

at_latest(PDF, ByDate, X, Y) ->
    eg_pdf:set_font(PDF, "Helvetica-Bold", 9),
    eg_pdf:moveAndShow(PDF, X, Y, "Betalas senast:  "++ByDate).

shipping(PDF, X, Y, Shipping) ->
    FontSize = 9,
    eg_pdf:set_font(PDF, "Helvetica", FontSize),
    eg_pdf:moveAndShow(PDF, X, Y, "Frakt/Porto"),
    Width = eg_pdf:get_string_width(PDF,"Helvetica", FontSize, Shipping),
    eg_pdf:moveAndShow(PDF, X+140-Width, Y, Shipping).

total(PDF, X, Y, Amount) ->
    FontSize = 9,
    eg_pdf:set_font(PDF, "Helvetica-Bold", FontSize),
    eg_pdf:moveAndShow(PDF, X, Y, "Summa"),
    Total = lists:flatten(
	      io_lib:format("~w,~2.0.0w",
			    [trunc(Amount),trunc(Amount*100) rem 100])),
    Width = eg_pdf:get_string_width(PDF,"Helvetica-Bold", FontSize, Total),
    eg_pdf:moveAndShow(PDF, X+140-Width, Y, Total).
    
vat(PDF, X, Y, Vat) ->
    FontSize = 9,
    eg_pdf:set_font(PDF, "Helvetica", FontSize),
    eg_pdf:moveAndShow(PDF, X, Y, "Varav moms"),
    Width = eg_pdf:get_string_width(PDF,"Helvetica", FontSize, Vat),
    eg_pdf:moveAndShow(PDF, X+140-Width, Y, Vat).


items(PDF, Is, Y, X1, X2, X3, X4, X5) ->
    eg_pdf:set_font(PDF, "Helvetica", 9),
    fmt_items(PDF, Is, Y, X1, X2, X3, X4, X5, {"Helvetica",9}).
    
fmt_items(_PDF,[], _Y, _X1, _X2, _X3, _X4, _X5, _Font) -> ok;
fmt_items(PDF, [{INr, Str, Nr, Price, Total}|Is],
          Y, X1, X2, X3, X4, X5, F) ->
    eg_pdf:moveAndShow(PDF, X1, Y, INr),
    eg_pdf:moveAndShow(PDF, X2, Y, to_mac(Str)),
    moveAndShowRight(PDF, F, X3, Y, Nr),
    moveAndShowRight(PDF, F, X4, Y, Price),
    moveAndShowRight(PDF, F, X5, Y, Total),
    fmt_items(PDF, Is, Y-10, X1, X2, X3, X4, X5, F).

%% Str is drawn to the left of X (X is the _right_ alignment side of the Str
%% text box)
moveAndShowRight(PDF, {Font,Size}, X, Y, Str) when is_integer(X),
						   is_integer(Y) ->
    Width = eg_pdf:get_string_width(PDF, Font, Size, Str),
    eg_pdf:moveAndShow(PDF, X-Width, Y, Str);
moveAndShowRight(_,_,_,_,_)  ->
    ok.

bg_avi(PDF, X, Y) ->
    eg_pdf:set_line_width(PDF, 1.5),
    eg_pdf:line(PDF, X+32, Y+99, X+32+545, Y+99),
    eg_pdf:line(PDF, X+32, Y+85, X+32+545, Y+85),
    eg_pdf:set_line_width(PDF, 0.3),
    eg_pdf:line(PDF, X+32+201, Y+85, X+32+201, Y+85-6.2),
    eg_pdf:line(PDF, X+32+265, Y+85, X+32+265, Y+85-6.2),
    eg_pdf:line(PDF, X+32+294.7, Y+85, X+32+294.7, Y+85-6.2),
    eg_pdf:line(PDF, X+32+284.8, Y+99, X+32+284.8, Y+99+25.5),
    eg_pdf:line(PDF, X+32+284.8+79, Y+99+25.5, X+32+284.8+79, Y+99+25.5-6.2),
    eg_pdf:line(PDF, X+32+342, Y+276, X+32+342, Y+249),
    eg_pdf:line(PDF, X+32+430, Y+276, X+32+430, Y+249),
    eg_pdf:set_line_width(PDF, 1),
    eg_pdf:line(PDF, X+32+284.8, Y+99+25.5, X+32+545, Y+99+25.5),
    eg_pdf:line(PDF, X+32, Y+249, X+32+545, Y+249),
    eg_pdf:line(PDF, X+32, Y+276, X+32+545, Y+276),
    eg_pdf:set_font(PDF, "Helvetica", 6),
    eg_pdf:moveAndShow(PDF, X+32+41, 85-6.2+Y, "Referensnummer"),
    eg_pdf:moveAndShow(PDF, X+32+202.6, 85-6.2+Y, "Kronor"),
    eg_pdf:moveAndShow(PDF, X+32+266.4, 85-6.2+Y, to_mac("Öre")),
    eg_pdf:moveAndShow(PDF, X+32+287.7, Y+99+25.5-6.2,
                        to_mac("Till bankgiro")),
    eg_pdf:moveAndShow(PDF, X+32+367, Y+99+25.5-6.2,
                        to_mac("Betalningsmottagare")),
    eg_pdf:set_font(PDF, "Helvetica", 8),
    eg_pdf:moveAndShow(PDF, X+32+18.5, 87+Y,
                        to_mac("VAR GOD GÖR INGA ÄNDRINGAR")),
    eg_pdf:moveAndShow(PDF, X+32+182.8, 87+Y,
			to_mac("MEDDELANDEN KAN INTE LÄMNAS PÅ AVIN")),
    eg_pdf:moveAndShow(PDF, X+32+372, 87+Y,
			to_mac("DEN AVLÄSES MASKINELLT")),
    eg_pdf:set_font(PDF, "Helvetica", 6),
    eg_pdf:moveAndShow(PDF, X+32+347, Y+276-6.2,
			to_mac("Inbet avgift (ifylls av banken)")),
    eg_pdf:moveAndShow(PDF, X+32, Y+276-8.2,
			to_mac("Sista betalningsdag")),
    eg_pdf:set_font(PDF, "Helvetica-Bold", 12),
    eg_pdf:moveAndShow(PDF, X+32+282, Y+276+4,
			to_mac("INBETALNING/GIRERING AVI")),
    eg_pdf:set_font(PDF, "Times-Bold", 12),
    eg_pdf:moveAndShow(PDF, X+32, Y+276+4,
			to_mac("bankgirot")),
    eg_pdf:set_font(PDF, "Helvetica-Bold", 24),
    eg_pdf:moveAndShow(PDF, X+32+461, Y+249+6, to_mac("OCR")).

bg_avi_bgnr(PDF, X, Y, BGNr) ->
    eg_pdf:set_font(PDF, "Helvetica", 10),
    eg_pdf:moveAndShow(PDF, X+32+293.3, 103.4+Y, BGNr).

bg_avi_recipient(PDF, X, Y, Rec) ->
    eg_pdf:set_font(PDF, "Helvetica", 10),
    eg_pdf:moveAndShow(PDF, X+32+370, 103.4+Y, Rec).

bg_avi_from(PDF, X, Y, {Name,Street,City}) ->
    eg_pdf:set_font(PDF, "Helvetica", 10),
    eg_pdf:moveAndShow(PDF, X+32, 184+Y, to_mac(Name)),
    eg_pdf:moveAndShow(PDF, X+32, 184+Y-15, to_mac(Street)),
    eg_pdf:moveAndShow(PDF, X+32, 184+Y-15-15, to_mac(City)).

bg_avi_lastdate(PDF, X, Y, LastDate) ->
    eg_pdf:set_font(PDF, "Helvetica", 10),
    eg_pdf:moveAndShow(PDF, X+32, 249+6+Y, to_mac(LastDate)).

bg_avi_ocrline(PDF, X, Y, BGNr, Amount, OCR) ->
    eg_pdf:set_font(PDF, "Helvetica", 8),
    eg_pdf:moveAndShow(PDF, X+32+293.3, 187+Y,
			to_mac("OBS! Uppge alltid nedanstående "
			       " referensnummer om betalning sker på")),
    eg_pdf:moveAndShow(PDF, X+32+293.3, 187+Y-10,
			to_mac("annat sätt än med detta inbetalningskort")),
    eg_pdf:set_font(PDF, ?OCRFONT, ?OCRSIZE),
    eg_pdf:moveAndShow(PDF, X+32+293.3, 187+Y-10-15, OCR),

    Checksum = chk10(trunc(Amount*100)),
    Kr = trunc(Amount),
    Ore = trunc(Amount*100) rem 100,
    BGNr2 = lists:delete($-, BGNr),
    Txt = lists:flatten(
	    io_lib:format("H  # ~25.s #~8.w ~2.0.0w   ~p >~25.s#41#    ",
			  [OCR, Kr, Ore, Checksum, BGNr2])),
    %% Width = eg_pdf:get_string_width(PDF, ?OCRFONT, ?OCRSIZE, Txt),
    eg_pdf:moveAndShow(PDF, X+5, 48.2+Y, Txt).

to_mac(Str) ->
    F = fun($å) -> 8#214;
	   ($ä) -> 8#212;
	   ($ö) -> 8#232;
	   ($Å) -> 8#201;
	   ($Ä) -> 8#200;
	   ($Ö) -> 8#205;
	   (X)  -> X
	end,
    [F(X) || X <- Str].

chk10(Int) ->
    chk10(Int, 2, 0).
chk10(0, _, Acc) ->
    case Acc rem 10 of
	0 -> 0;
	Rem -> 10 - Rem
    end;
chk10(Int, Mult, Acc) ->
    S0 = (Int rem 10) * Mult,
    S1 = if S0 > 9 -> S0 - 9;
	    S0 =< 9 -> S0
	 end,
    chk10(Int div 10, 1 + (Mult rem 2), Acc + S1).
