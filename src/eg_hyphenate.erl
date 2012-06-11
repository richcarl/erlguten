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
%% Purpose: Hyphenation program - adapted from TeX
%%==========================================================================

-module(eg_hyphenate).

-export([partitions/2, 
	 word/2, 
	 test/1
	]).

-define(TO_SHORT, 4).


test(1) -> word("hyphenation", eg_hyphen_rules_en_GB);
test(2) -> partitions("supercalifragilisticexpialidocious", 
		      eg_hyphen_rules_en_GB);
test(4) -> test_text(eg_hyphen_rules_sv_SE, text_sv());
test(5) -> test_text(eg_hyphen_rules_en_GB, text_en());
test(6) -> test_text(eg_hyphen_rules_nb_NO, text_nb());
test(7) -> test_text(eg_hyphen_rules_fi_FI, text_fi());
test(8) -> test_text(eg_hyphen_rules_da_DK, text_da()).


test_text(Rules, Text) -> 
    io:format("====================================~n",[]),
    lists:foreach(
      fun(W) -> io:format("~s ~s~n", [word(W, Rules), W]) 
      end,
      get_words(Text)).


get_words(Text) ->
    [Str || Str <- string:tokens(Text, " ,."), length(Str) > ?TO_SHORT].


%% ============================================================================

%% text from: Svenska Dagbladet (2009-08-05) 
%%            http://www.svd.se/nyheter/inrikes/artikel_3310817.svd
text_sv() ->
    "Ali Mehrabi och hans kompisar skulle överlämna ett brev och en "
	"film till UD igår. De fick träffa Anders Jörle, presschef för "
	"UD:s presstjänst, han skulle se till att utrikesminister Carl "
	"Bildt fick materialet. I brevet vädjar ungdomarna till ministern "
	"att inte erkänna Mahmoud Ahmadinejad som Irans president. "

	"Folkpartiets utrikespolitiske talesperson, Birgitta Ohlsson, "
	"tycker att Sverige borde bli tuffare i sin kritik mot den Iranska "
	"regimen och att man borde stödja demokratirörelsen i landet. "
	"Hon påpekar dock att Sverige representerade EU på "
	"presidentinstallationen och att meningarna där var delade när "
	"det gäller ceremonin.".

%% text from: BBC (2009-08-05) 
%%            http://news.bbc.co.uk/2/hi/middle_east/8184240.stm
text_en() ->
    "Mahmoud Ahmadinejad has been sworn in for a second term as Iran's "
	"president, after weeks of post-election unrest. "
	"In an address after the ceremony, he criticised foreign powers "
	"who have cast doubt on the validity of the election, saying Iran "
	"would resist them. "
	"Opposition supporters protesting outside parliament were met by "
	"hundreds of riot police. "
	"Germany, France, Britain and the US all said they would not be "
	"sending letters of congratulation to Mr Ahmadinejad. "
	"At least 30 people died during the street protests which followed "
	"the 12 June poll.".

%% text from: dagbladet.no (2009-08-05) 
%%            http://www.dagbladet.no/2009/08/05/kultur/the_beatles/
%%            stanley_kubrick/musikk/film/7520522/
text_nb() ->
    "Hvordan hadde du forholdt deg til en \"Ringenes Herre\"-filmatisering "
	"med Paul McCartney som Frodo Baggins, John Lennon som Gandalf the "
	"Grey og Stanley Kubrick som regissør? "
	"Det høres ut som et meget surrealistisk og uvirkelig prosjekt, men "
	"det kunne fort ha vært en realitet. "
	"Tolkien-eksperten Michael D. C. Drout skriver blant annet i boka "
	"si \"J.R.R. Tolkien encyclopedia\" hvordan The Beatles prøvde å "
	"lage film av Tolkien-romanen \"Ringenes Herre\" etter at "
	"rettighetene var solgt til United Artists i 1968. "
	"Tanken deres var at The Beatles selv skulle spille hovedrollene "
	"og lage filmmusikken.".
    
%% text from: jyllandsposten (2009-08-05)
%%            http://jp.dk/udland/usa/article1772552.ece
text_da() ->
    "To amerikanske journalister tilbageholdt i Nordkorea i 140 dage "
	"brød sammen ved synet af deres familie i USA. Det er godt at "
	"være fri, siger de. "
	"I første omgang så Hana Lee lidt forvirret ud. "
	"Den fireårige pige havde ikke set sin mor i 140 dage. "
	"Men så knugede hun sig fast til den amerikanske journalist Euna "
	"Lee og ville ikke give slip igen."
	"Sådan var scenen onsdag eftermiddag dansk tid, da Euna Lee og "
	"Laura Ling ankom til lufthavnen i Burbank, Californien."
	"De to journalister har været tilbageholdt i Nordkorea, siden de "
	"i marts blev anholdt ved grænsen mellem Nordkorea og Kina - og "
	"siden blev idømt 12 år i en arbejdslejr for at have overskredet "
	"grænsen ulovligt.".

%% text from: wikipedia (2009-08-06) (About finnish person numbers)
%%            http://fi.wikipedia.org/wiki/Henkil%C3%B6tunnus
text_fi() ->
    "Suomessa henkilötunnus (hetu) annetaan Suomen kansalaisille sekä "
	"Suomessa pysyvästi tai pitkäaikaisesti (vähintään vuoden) "
	"oleskeleville ulkomaalaisille. Tunnus voidaan anomuksesta myöntää "
	"myös tilapäisesti oleskeleville henkilöille. Tunnuksen myöntää "
	"Väestörekisterikeskus. Henkilötunnusta ei voi käyttää luotettavasti "
	"henkilöllisyyden todisteena kahdesta syystä: ensinnäkin se ei ole "
	"salainen tieto, ja toiseksi se ei ole vaihdettavissa, "
	"kuten vaikkapa verkkopankin salasana. Valitettavasti "
	"henkilötunnusta, etenkin sen loppuosaa, usein luullaan "
	"eräänlaiseksi \"salasanaksi\" vaikkapa pankkikorttiostoksia "
	"tehdessä tai terveyskeskuksessa asioidessa, mikä johtaa helposti "
	"väärinkäytöksiin.".


%% ============================================================================

partitions(W, Rules) -> partitions(word(W, Rules), [], [], Rules).

partitions([$-|T],  B, L, Rules) -> partitions(T, B, [{lists:reverse(B), 
						       remove_hyphens(T)}|L], 
					       Rules);
partitions([H|T], B, L, Rules)   -> partitions(T, [H|B], L, Rules);
partitions([], _, L, _)          -> lists:reverse(L).


remove_hyphens([$-|T]) -> remove_hyphens(T);
remove_hyphens([H|T])  -> [H|remove_hyphens(T)];
remove_hyphens([])     -> [].

word(L, Rules) when length(L) > ?TO_SHORT ->
    case Rules:exception(L) of
	no ->
	    L1 = word([$.|L] ++ ".", 0, [], Rules),
	    L2 = lists:sort(keep_odd(L1)),
	    %% io:format("L2=~p~n",[L2]),
	    W1 = make_word(L2, 1, L),
	    remove_singleton(W1);
	Str ->
	    Str
    end;
word(X, _) ->
    X.


%% -AAA... => AAA..
%% X-AAA.. => AAA..
%% ...-B   => ...B
%% ...B-   => ...B

remove_singleton([H,$-|T]) -> [H|remove_singleton1(T)];
remove_singleton([$-|T])   -> remove_singleton1(T);
remove_singleton(X)        -> remove_singleton1(X).

remove_singleton1([$-])   -> [];
remove_singleton1([$-,H]) -> [H];
remove_singleton1([H|T])  -> [H|remove_singleton1(T)];
remove_singleton1([])     -> [].

make_word([{Pos,_C}|T], Pos, L) ->
    [$-|make_word(T, Pos, L)];
make_word(S=[{Pos,_}|_], Pos1, [H|T]) when Pos1 < Pos ->
    [H|make_word(S, Pos1+1, T)];
make_word([], _, L) ->
    L.

word([], _, L, _) ->
    L;
word(T, N, L, Rules) ->
    case Rules:hyphens(T) of
	[] ->
	    word(tl(T), N+1, L, Rules);
	M ->
	    M1 = lists:map(fun({Pos,Val}) -> {Pos+N,Val} end, M),
	    %% io:format("~s ~p => ~p~n", [T, M, M1]),
	    L1 = merge(M1, L),
	    word(tl(T), N+1, L1, Rules)
    end.


merge([], L)          -> L;
merge([{Pos,C}|T], L) -> merge(T, merge1(Pos, C, L)).

merge1(Pos, C, [])                       -> [{Pos,C}];
merge1(Pos, C, [{Pos,C1}|T]) when C > C1 -> [{Pos,C}|T];
merge1(Pos, _C, [{Pos,C1}|T])             -> [{Pos,C1}|T];
merge1(Pos, C, [H|T])                    -> [H|merge1(Pos, C, T)].

keep_odd(L) ->    
    lists:filter(fun({_Pos,Count}) -> odd(Count) end, L).

odd(X) ->
    (X rem 2) == 1.

    
