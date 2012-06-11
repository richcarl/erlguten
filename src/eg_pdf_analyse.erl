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
%% Purpose: Analyse a PDF file, manipulate PDF files, pack and unpack files
%% into PDF files
%% =========================================================================

-module(eg_pdf_analyse).


%% eg_pdf:pack(Pdf, File, Pdf1)
%%   Packs file File into Pdf producing Pdf1
%% eg_pdf:unpack(Pdf) 
%%   Extracts any file that was packed into the Pdf file

-export([batch/1, pack/3, unpack/1, cvt/1]).

-export([test_pack/0, test_unpack/0, debug/1, debug/2]).

-export([open/1, close/1, tables/1,
	 find_xref_pointer/1, find_trailer_pointer/1, get_xref/2,
	 get_trailer/2, read_obj/2, get_tables/1, get_object/3]).

%% exports
%% open(File) -> {ok, Pdf} | error
%% close(Pdf) -> true.
%% find_xref_pointer(Pdf) -> Integer | EXIT
%% find_trailer_pointer(Pdf) -> Int | exit
%% get_xref(Pdf, Start) -> xref().
%% get_trailer(Pdf, TrailerPointer) -> trailer().
%% read_obj(Pdf, Ptr) -> object().
%% get_tables(Pdf) -> {XrefPtr, xref(), trailer()}.
%% get_object(Pdf, Ptr, Xref) -> Obj


%% ============================================================================

batch([pack,A,B,C]) ->
    pack(atom_to_list(A), atom_to_list(B), atom_to_list(C)),
    erlang:halt();
batch([unpack,A]) ->
    unpack(atom_to_list(A)),
    erlang:halt();
batch(_A) ->
    io:format("Usage pack In File Out | Unpack Pdf~n",[]),
    erlang:halt().

test_pack() ->
    pack("/home/joe/work/thesis/src/extensions.pdf", "tiny",
	 "tmp.pdf").

test_unpack() -> unpack("tmp.pdf").


%% Parse pdf file
%% Note probably doesn't work work with "linearized" PDF

%% open(File) -> Pdf
%% get_tables(Pdf) -> {Trailer, Blocks}
%% read_object(Pdf, Pos) -> Object. 

%% example object (binary)
%% 3 0 obj <<
%% /Length 4 0 R
%% /Filter /FlateDecode
%% >>
%% stream
%% kjdhfksdvhsvsdvhdsk
%% ...
%% ....sdafsadfsdafendstream
%% endobj
%% 4 0 obj
%% 1878
%% endobj

%% This says that object 3 0 's length will be found later (in object 4)
%% It's a stream object
%% Object 4 just contains the length of object 3

%% The xfref table at the end of the file is
%%
%% xref
%% 0 73
%% 0000000000 65535 f 
%% 0000002093 00000 n 
%% 0000001980 00000 n 
%% ...
%%
%% 0000027327 00000 n 
%% 0000027378 00000 n 
%% trailer
%% <<
%% /Size 73
%% /Root 71 0 R
%% /Info 72 0 R
%% >>
%% startxref
%% 27473
%% %%EOF

-define(IN(X,Min,Max), X >= Min, X =< Max).

tables(File) ->
    Pdf = open(File),
    {XrefPtr, Xref, Trailer} = T = get_tables(Pdf),
    io:format("Tables=~p~n",[T]),
    Blocks = lists:reverse(
	       lists:map(fun(I) -> get_xref_entry(I, Xref, Pdf) end, Xref)),
    %% io:format("Blocks=~p~n",[Blocks]),
    close(Pdf),
    {{xrefprt,XrefPtr},
     {trailer, Trailer},
     {xref, Xref},
     {blocks, Blocks}}.

debug(In) ->
    debug(In ++ ".pdf", In ++ ".out").

debug(In, Out) ->
    Tables = tables(In),
    {ok, O} = file:open(Out, [write]),
    io:format(O, "~p~n",[Tables]),
    file:close(O).

open(File) ->
    {ok, Pdf} = file:open(File, [binary, raw, read]),
    Pdf.

close(Pdf) ->
    file:close(Pdf).

%% get_tables(Pdf) ->
%%   {XrefPtr, xref(), trailer()}

get_tables(Pdf) ->
    XrefPtr    = find_xref_pointer(Pdf),
    Xref       = get_xref(Pdf, XrefPtr),
    TrailerPtr = find_trailer_pointer(Pdf),
    Trailer    = get_trailer(Pdf, TrailerPtr),
    {XrefPtr, Xref, Trailer}.

get_xref_entry({Block,n,Pos,_G}, Xref, Pdf) ->
    io:format("getting block ~p at ~p~n", [Block, Pos]),
    O = read_object(Pdf, Pos, Xref),
    io:format("Block=~p~n",[O]),
    O;
get_xref_entry(_X, _, _) ->
    %io:format("Here X=~p~n",[X]),
    void.

%% Get object from it's point and the Xref tables

get_object(Pdf, {ptr,I,J}, Xref) ->
    P = find_obj_ptr(I, J, Xref),
    get_xref_entry(P, Xref, Pdf).

find_obj_ptr(I, J, [P={I,n,_,J}|_]) -> P;
find_obj_ptr(I, J, [_|T])           -> find_obj_ptr(I, J, T);
find_obj_ptr(_I, _J, [])              -> exit(pointer_error).

%% Objects
%%  N M obj ... endobj
%%  N M Dict stream ... endstream endobj

read_object(F, Pos, Xref) ->
    case read_obj(Pos, F) of
	{I, Str1, Pos1} when is_integer(I)->
	    case read_obj(Str1, Pos1, F) of
		{J, Str2, Pos2} when is_integer(J) ->
		    case read_obj(Str2, Pos2, F) of
			{obj, Str3, Pos3} ->
			    {Obj, Str4, Pos4} = read_obj(Str3, Pos3, F),
			    io:format("Read: ~p ~p ~p~n",[I,J,Obj]),
			    %% io:format("SSSS=~p~n",[Str4]),
			    read_after_object(I, J, Obj, F, Pos4, Str4, Xref);
			_Other ->
			    exit({pdf,expected,keword, obj, Pos2})
		    end;
		_ ->
		    exit({pdf,expected,integer, Pos1})
	    end;
	_ ->
	    exit({pdf,expected,integer, Pos})
    end.

read_after_object(I, J, Obj, F, Pos, _Str, Xref) ->
    case read_obj(Pos, F) of
	{endobj, _, _} ->
	    {{obj,I,J},Obj};
	{stream, _, Pos1}->
	    io:format("Found a stream~n"),
	    ensure(F, Pos1-6,"stream"),
	    %% Find the Length indictaor in the
	    %% Obj
	    Length = get_length_from_dict(Obj),
	    io:format("Legth=~p Xref=~p~n",[Length, Xref]),
	    Length1 = case Length of
			  {ptr,I1,J1} ->
			      {{obj,I1,J1},Len} = get_object(F, 
							     {ptr,I1,J1}, 
							     Xref),
			      Len;
			  Other ->
			      Other
		      end,
	    io:format("Length1=~p ~n",[Length1]),
	    %% Read the stream into a binary Bin
	    {ok, Bin} = file:pread(F, Pos1+1, Length1),
	    File = "stream_" ++ i2s(I)++"_"++i2s(J) ++ "_" ++ i2s(Length1),
	    file:write_file(File, Bin),
	    {{obj, I,J},{stream,Length1,File,element(2, Obj)}}
    end.

i2s(I) ->
    integer_to_list(I).

cvt(Bin) ->
    L = binary_to_list(Bin),
    case all_printable(L) of
	true ->
	    {str, L};
	false ->
	    {binary, Bin}
    end.

all_printable(L) ->
    lists:all(fun is_printable/1, L).

is_printable(I) ->
    (I > 31) and (I < 256).

    

%% find_xref_pointer(Pdf) -> Int | EXIT
%%   Finds the xref pointer at the end of the file
%%   The end of a PDF file looks like this
%%   ...
%%   startxref
%%   27473
%%   %%EOF
%%
%%   In this case 27473 is returned

find_xref_pointer(F) ->
    %% io:format("Find xref_pointer:~p~n",[F]),
    {ok, Pos} = file:position(F, {eof,-50}),
    %% io:format("Pos=:~p~n",[Pos]),
    {ok, Pos} = file:position(F, {eof,-50}),
    Data = read_str(F, Pos, 50),
    S = start_xref1(Data),
    %% io:format("S=~p~n",[S]),
    isolate_xref(S, []).

start_xref1("startxref\n" ++ T) -> T;
start_xref1("startxref\r" ++ T) -> T;
start_xref1([_|T])              -> start_xref1(T);
start_xref1([])                 -> exit({corrupt_pdf,startxref}).

isolate_xref("\n%%EOF" ++ _, L) -> list_to_integer(lists:reverse(L));
isolate_xref("\r%%EOF" ++ _, L) -> list_to_integer(lists:reverse(L));
isolate_xref([H|T], L)          -> isolate_xref(T, [H|L]);
isolate_xref(X, Y)              -> exit({corrupt_pdf, X,Y}).

%%______________________________________________________________________

%% Pos *should* point to
%% xref
%% NNN KKK

get_xref(F, Pos) ->
    %% First confirm that we are pointing at xref
    ensure(F, Pos, "xref"),
    get_xref_blocks(F, Pos+5, []).

get_xref_blocks(F, Pos, L) ->
    Data = read_str(F, Pos, 80),
    Line = get_next_line(Data),
    case Line of
	"trailer" ++ _ ->
	    %% io:format("Found the trailer~n"),
	    L;
	_ ->
	    case erl_scan:string(Line) of
		{ok, [{integer,_,Start},{integer,_,N}], _} ->
		    io:format("Start block=~p Number=~p~n",[Start,N]),
		    {L1, Pos1} = get_blocks(F, Pos+length(Line)+1,Start,N,L),
		    get_xref_blocks(F, Pos1, L1);
		_ ->
		    exit({pdf_corrupt,get_xref_args,Line})
	    end
    end.

get_blocks(F, Pos, Start, N, L) ->
    %% Pos points to the start of the first block
    Len = 20*N,
    Data = read_str(F, Pos, Len),
    %% io:format("read:~n~s~n",[Data]),
    %% io:format("L=~p~n",[L]),
    L1 = unpack_blocks(Data, Start, L),
    {L1, Pos + Len}.

unpack_blocks([N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,_,
	       G1,G2,G3,G4,G5,_,
	       S,_,_|T], Start, L) ->
    N = list_to_integer([N1,N2,N3,N4,N5,N6,N7,N8,N9,N10]),
    G = list_to_integer([G1,G2,G3,G4,G5]),
    Status = case S of
		 $n -> n;
		 $f -> f;
		 _  -> exit({pdf,bad_code,S})
	     end,
    %% io:format("Here:~p~n",[{N,G,Status,Start}]),
    %% io:format("T=~p~n",[T]),
    unpack_blocks(T, Start+1, [{Start,Status,N,G}|L]);
unpack_blocks([], _, L) ->
    L.

get_next_line([$\n|_]) -> [];
get_next_line([$\r|_]) -> [];
get_next_line([H|T])   -> [H|get_next_line(T)];
get_next_line([])      -> exit({pdf_corrupt,missing,nl}).

    
read_str(File, Pos, Len) ->
    case file:pread(File, Pos, Len) of
	eof -> eof;
	{ok, Bin} -> binary_to_list(Bin)
    end.

%%----------------------------------------------------------------------
%% find_trailer_pointer(Pdf)
%%   Return a pointer to the trailer

find_trailer_pointer(F) ->  
    %% A bit yucky
    Guess = 512,
    {ok, Pos} = file:position(F, {eof,-Guess}),
    Data = read_str(F, Pos, Guess),
    %% Data is not the last Guess Bytes of the files
    Data1 = lists:reverse(Data),
    find_trailer_pointer(Data1, 0, Pos+Guess, F).

find_trailer_pointer(">>" ++ T, Level, Pos, F) ->
    find_trailer_pointer(T, Level+1, Pos-2, F);
find_trailer_pointer("<<" ++ T, Level, Pos, F) ->
    find_trailer_pointer(T, Level-1, Pos-2, F);
find_trailer_pointer("reliart" ++ _T, 0, Pos, _F) ->
    %% yes
    Pos - 7;
find_trailer_pointer(Str, Level, Pos, F) when length(Str) < 8 -> 
    %% Need yet more stuff
    Data = read_str(F, Pos - 512, 512),
    find_trailer_pointer(Str ++ lists:reverse(Data), Level, Pos, F);
find_trailer_pointer([_H|T], 0, Pos, F) ->
    find_trailer_pointer(T, 0, Pos-1, F);
find_trailer_pointer([_H|T], Level, Pos, F) when Level > 0  ->
    find_trailer_pointer(T, Level, Pos-1, F);
find_trailer_pointer([], Level, _Pos, _F) ->
    exit({corrupt_pdf,find_trailer_pointer, Level}).

%%----------------------------------------------------------------------
%% get_trailer(F, Pos) ->

get_trailer(F, Pos) ->
    ensure(F, Pos, "trailer"),
    case read_obj("", Pos+8, F) of
	{{dict, D}, _, _} ->
	    {trailer, D};
	Other ->
	    exit({pdf_bad_trailer, Other})
    end.


ensure(F, Pos, Str) ->
    S1 = read_str(F, Pos, 80),
    case prefix(Str, S1) of
	true ->
	    %% io:format("Ensure check worked found ~s at ~p~n",[Str, Pos]),
	    true;
	false ->
	    exit({corrupt_pdf, pos, Pos, expecting, Str, found, S1})
    end.

prefix([], _) -> true;
prefix([H|T], [H|T1]) -> prefix(T, T1);
prefix(_, _) -> false.

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% most funcs look like
%%   Parse(Str, Pos, F) -> {Obj, Str', Pos'}

read_obj(Pos, F) ->
    read_obj("", Pos, F).

read_obj(Str, Pos, F) ->
    %% io:format("read obj at:~p~n",[Pos]),
    case skip_blanks(Str, Pos, F) of
	{Str1, Pos1} ->
	    %% io:format("read obj1 at:~p str=~p~n",[Pos1,Str1]),
	    case Str1 of
		[X|_] when ?IN(X,$0,$9) -> get_int_or_float(Str1, Pos1, F);
		[$+|_]                  -> get_int_or_float(Str1, Pos1, F);
		[$-|_]                  -> get_int_or_float(Str1, Pos1, F);
		[$(|Str3]               -> gs(Str3, Pos1+1, F);
                "<<" ++ Str3            -> get_dict(Str3,Pos1+2,F);
		">>" ++ Str3            -> {dictEnd, Str3, Pos1+2};
		"true" ++ Str3          -> {true, Str3, Pos1+4};
		"false" ++ Str3         -> {false, Str3, Pos1+5};
		"null" ++ Str3          -> {null, Str3, Pos1+4};
		"obj" ++ Str3           -> {obj, Str3, Pos1+3};
		"endobj" ++ Str3        -> {endobj, Str3, Pos1+6};
		"stream" ++ Str3        -> {stream, Str3, Pos1+6};
		"<" ++ Str3             -> get_hex_string(Str3, Pos1+1, F);
		[$/|Str3]               -> get_named_object(Str3, Pos1+1, F);
		[$[|Str3]               -> get_array(Str3, Pos1+1, F);
		[$]|Str3]               -> {arrayEnd, Str3, Pos1+1};
		[$R|Str3]               -> {r, Str3, Pos1+1};
		[$%|Str3]               -> 
		    {_, Str4, Pos4} = get_comment(Str3, Pos1+1, F),
		    read_obj(Str4, Pos4, F);
		_               -> exit({pdf_error, read_obj1, Str1, Pos1})
	    end
    end.

get_comment(_Str, _Pos, _F) ->
    exit({nyi,get_comment}).

get_hex_string(Str, Pos, F) -> get_hex_string(Str, Pos, F, []).

get_hex_string(">" ++ Str, Pos, _F, L) ->
    {{string, lists:reverse(L)}, Str, Pos+1};
get_hex_string([X,$>|Str], Pos, _F, L) ->
    {{string, lists:reverse([hex2byte(X,$0)|L])}, Str, Pos+2};
get_hex_string([X,Y|T], Pos, F, L) ->
    get_hex_string(T, Pos+2, F, [hex2byte(X,Y)|L]);
get_hex_string(Str, Pos, F, L) ->
    case read_str(F, Pos+length(Str), 1024) of
	eof  -> exit({pdf,eof_in_hex_string});
	Str1 -> get_hex_string(Str ++ Str1, Pos, F, L)
    end.

get_length_from_dict({dict, D}) -> gld(D);
get_length_from_dict(_) -> exit({pdf,stream_has_no_dict}).

gld([{"Length",L}|_]) -> L;
gld([_|T])            -> gld(T);
gld([])               -> exit({pdf,stream,no_length_in_dict}).

get_dict(Str, Pos, F) -> 
    get_dict(Str, Pos, F, []).

get_dict(Str, Pos, F, L) ->
    %% io:format("In get_dict:~p~n",[L]),
    case read_obj(Str, Pos, F) of
	{dictEnd, Str1, Pos1} ->
	    %% ensure(F, Pos1-2,">>"),
	    {{dict, parse_dict(lists:reverse(L))}, Str1, Pos1};
	{Obj, Str1, Pos1} ->
	    get_dict(Str1, Pos1, F, [Obj|L])
    end.

parse_dict([{name,X}|T]) ->
    {Obj, T1} = get_next_obj(T),
    [{X,Obj}|parse_dict(T1)];
parse_dict([]) -> [];
parse_dict([H|_]) ->
    exit({pdferror, expecting_name_in_dict, found,H}).

get_next_obj([I,J,r|T]) when is_integer(I), is_integer(J) ->
    {{ptr,I,J}, T};
get_next_obj([H|T]) ->
    {H, T}.

get_array(Str, Pos, F) -> 
    get_array(Str, Pos, F, []).

get_array(Str, Pos, F, L) ->
    case read_obj(Str, Pos, F) of
	{arrayEnd, Str1, Pos1} ->
	    {{array, parse_array(lists:reverse(L))}, Str1, Pos1};
	{Obj, Str1, Pos1} ->
	    get_array(Str1, Pos1, F, [Obj|L])
    end.

parse_array([]) -> [];
parse_array(T) ->
    {Obj, T1} = get_next_obj(T),
    [Obj|parse_array(T1)].

%% gs == get_string
%% gs(Str, Pos, F) -> {Obj, Str', Pos}



gs(Str, Pos, F) -> gs(Str, Pos, F, 0, []).
    
gs([$)|T], Pos, _F, 0, L)   -> {{string, lists:reverse(L)}, T, Pos+1};
gs([$)|T], Pos, F, Lev, L) -> gs(T, Pos+1, F, Lev - 1, [$)|L]);
gs([$(|T], Pos, F, Lev, L) -> gs(T, Pos+1, F, Lev + 1, [$(|L]);
gs([$\\,$n|T], Pos, F, Lev, L)  -> gs(T, Pos+2, F, Lev, [$\n|L]);
gs([$\\,$r|T], Pos, F, Lev, L)  -> gs(T, Pos+2, F, Lev, [$\r|L]);
gs([$\\,$t|T], Pos, F, Lev, L)  -> gs(T, Pos+2, F, Lev, [$\t|L]);
gs([$\\,$b|T], Pos, F, Lev, L)  -> gs(T, Pos+2, F, Lev, [$\b|L]);
gs([$\\,$f|T], Pos, F, Lev, L)  -> gs(T, Pos+2, F, Lev, [$\f|L]);
gs([$\\,$(|T], Pos, F, Lev, L)  -> gs(T, Pos+2, F, Lev, [$(|L]);
gs([$\\,$)|T], Pos, F, Lev, L)  -> gs(T, Pos+2, F, Lev, [$)|L]);
gs([$\\,$\\|T], Pos, F, Lev, L) -> gs(T, Pos+2, F, Lev, [$\\|L]);
gs([$\\,$\n|T], Pos, F, Lev, L) -> gs(T, Pos+2, F, Lev, L);
gs([$\\,$\r|T], Pos, F, Lev, L) -> gs(T, Pos+2, F, Lev, L);
gs([$\\,O1,O2,O3|T], Pos, F, Lev, L) when ?IN(O1, $0,$7),
				    ?IN(O2, $0,$7),
				    ?IN(O3, $0,$7) ->
    gs(T, Pos+4, F, Lev, [o2i([O1,O2,O3])|L]);
gs([$\\,O1,O2|T], Pos, F, Lev, L) when ?IN(O1, $0,$7), ?IN(O2, $0,$7) ->
    gs(T, Pos+3, F, Lev, [o2i([O1,O2])|L]);
gs([$\\,O1|T], Pos, F, Lev, L) when ?IN(O1, $0,$7) ->
    gs(T, Pos+2, F, Lev, [o2i([O1])|L]);
gs([$\\,H|T], Pos, F, Lev, L) ->
    gs(T, Pos+2, F, Lev, [H|L]);
gs([$\\], Pos, F, Lev, L) ->						   
    case read_str(F, Pos+1, 1024) of
	eof -> exit({pdf,eof_in_string});
	Str -> gs([$\\] ++ Str, Pos, F, Lev, L)
    end;
gs([H|T], Pos, F, Lev, L) ->
    gs(T, Pos+1, F, Lev, [H|L]);
gs([], Pos, F, Lev, L) ->
    case read_str(F, Pos, 1024) of
	eof -> exit({pdf,eof_in_string});
	Str -> gs(Str, Pos, F, Lev, L)
    end.

%% Fetches something like /adadadda
%% get_named_object(S, Pos, F) -> {Obj, Str', Pos}

get_named_object(Str, Pos, F) ->
    get_named_object(Str, Pos, F, []).

get_named_object(S=[H|T], Pos, F, L) ->
    case is_name_delim(H) of
	true  -> {{name, lists:reverse(L)}, S, Pos};
	false -> get_named_object(T, Pos+1, F, [H|L])
    end;
get_named_object([], Pos, F, L) ->
    case read_str(F, Pos, 1024) of
	eof -> exit({pdf,eof_in_name});
	Str -> get_named_object(Str, Pos, F, L)
    end.

is_name_delim(X) ->
    is_delim(X) or is_white(X).

is_delim($))  -> true;
is_delim($()  -> true;
is_delim($<)  -> true;
is_delim($>)  -> true;
is_delim($[)  -> true;
is_delim($])  -> true;
is_delim(${)  -> true;
is_delim($})  -> true;
is_delim($/)  -> true;
is_delim($%)  -> true;
is_delim(_)   -> false.

is_white($ )  -> true;
is_white($\n) -> true;
is_white($\r) -> true;
is_white($\t) -> true;
is_white(_)   -> false.
    
    
skip_blanks(S=[H|T], Pos, F) ->
    case is_white(H) of
	true  -> skip_blanks(T, Pos+1, F);
	false -> {S, Pos}
    end;
skip_blanks([], Pos, F) ->
    case read_str(F, Pos, 1024) of
	eof  -> {"", Pos};
	Str  -> skip_blanks(Str, Pos, F)
    end.

%%----------------------------------------------------------------------
%% get_int_or_float(Str, Pos, F) -> {Obj, Str', Pos}

get_int_or_float(Str, Pos, F) -> gi(Str, Pos, F, int, []).

gi([$+|T], Pos, F, Type, L) -> gi(T, Pos+1, F, Type, [$+|L]);
gi([$-|T], Pos, F, Type, L) -> gi(T, Pos+1, F, Type, [$-|L]);
gi([$.|T], Pos, F, _Type, L) -> gi(T, Pos+1, F, float, [$.|L]);
gi([H|T], Pos, F, Type, L) when ?IN(H, $0, $9) ->
    gi(T, Pos+1, F, Type, [H|L]);
gi([], Pos, F, Type, L) ->
    case read_str(F, Pos, 1024) of
	eof -> gi_final(Type, lists:reverse(L), Pos, "");
	Str -> gi(Str, Pos, F, Type, L)
    end;
gi(Str, Pos, _F, Type, L) ->
    gi_final(Type, lists:reverse(L), Pos, Str).

gi_final(int, L, Pos, Str) ->
    case (catch list_to_integer(L)) of
	{'EXIT', _} -> exit({pdf,bad_integer,L,Pos});
        I -> {I, Str, Pos}
    end;
gi_final(float, L, Pos, Str) ->
    case (catch list_to_float(L)) of
	{'EXIT', _} -> exit({pdf,bad_real,L,Pos});
        I -> {I, Str, Pos}
    end.

%%----------------------------------------------------------------------

o2i(L) -> o2i(L, 0).

o2i([H|T], N) -> o2i(T, N*8 + H - $0);
o2i([], N)    -> N.
    
hex2byte(H1, H2) -> hex2int(H1) * 16 + hex2int(H2).

hex2int(X) when ?IN(X, $0, $9) -> X - $0;
hex2int(X) when ?IN(X, $a, $f) -> X - $a + 10;
hex2int(X) when ?IN(X, $A, $F) -> X - $A + 10;
hex2int(_X) -> exit({pdf,bad_hex_digit_in_string}).


%%----------------------------------------------------------------------
%% Code to add and rempve files from a PDF file

%% Pack(Pdf, File, Pdf1)
%%   Packs file File into Pdf producing Pdf1

pack(In, Extra, Out) ->
    Pdf = open(In),
    {XrefPtr, _, Trailer} = get_tables(Pdf),
    file:close(Pdf),
    Max  = from_trailer(Trailer, "Size"),
    Root = from_trailer(Trailer, "Root"),
    Size = fsize(In),
    case file:read_file(Extra) of
	{ok, Bin} ->
	    Bin1 = term_to_binary({erlpdf, Extra, Bin}),
	    Addon = make_addon(Size, Bin1, Max, Root, XrefPtr),
	    {ok, BinIn} = file:read_file(In),
	    file:write_file(Out, [BinIn,Addon]);
	_ ->
	    exit(noAdditionalFile)
    end.

unpack(File) ->
    Pdf = open(File),
    {_, Xref, Trailer} = get_tables(Pdf),
    Info = from_trailer(Trailer, "Info"),
    case get_object(Pdf, Info, Xref) of
	{_, _, {stream,_Len,Bin,Dict}} ->
	    {string, "erlangAddedFile"} = from_dict("Comment", Dict),
	    case (catch binary_to_term(Bin)) of
		{erlpdf, FileName, Bin2} ->
		    file:close(Pdf),
		    write_file(FileName, Bin2);
		_ ->
		    exit(pdf_not_erlang)
	    end;
	_ ->
	    exit(Pdf)
    end.

write_file(File, Bin) ->
    case exists(File) of
	true ->
	    write_file(File ++ ".tmp", Bin);
	false ->
	    io:format("Writing:~p~n",[File]),
	    file:write_file(File, [Bin])
    end.

from_trailer({trailer, L}, Key) ->
    from_dict(Key, L).

from_dict(Key, [{Key,Val}|_]) -> Val;
from_dict(Key, [_|T])         -> from_dict(Key, T);
from_dict(Key, [])            -> exit({missing_key, Key}).

    
-include_lib("kernel/include/file.hrl").

fsize(F) ->
    {ok, Info} = file:read_file_info(F),
    Info#file_info.size.

exists(F) ->
    case file:read_file_info(F) of
	{ok, _} -> true;
	_ -> false
    end.

make_addon(Size, Bin, Max, {ptr,Root,Gen}, XrefPtr) ->
    Block1 = 
	b([i(Max)," 0 obj <<\n",
	   "/Length ", i(size(Bin)),"\n",
	   "/Comment (erlangAddedFile)\n",
	   ">>\n",
	   "stream\n",
	   Bin,"endstream\n",
	   "endobj\n"]),
    Xref = 
	b(["xref\n",
	   i(Max)," 1\n",
	   i10(Size)," 00000 n \n"]), 
    Trailer =
	b(["trailer\n",
	   "<<\n",
	   "/Size ",i(Max+1), "\n",
	   "/Root ",i(Root), " ", i(Gen)," R\n",
	   "/Prev ", i(XrefPtr), "\n",
	   "/Info ", i(Max), " 0 R\n",
	   ">>\n"]),
    Final = 
	b(["startxref\n",
	   i(Size+size(Block1)),"\n",
	   "%%EOF\n"]),
    All = [Block1, Xref, Trailer,Final],
    file:write_file("debug", [All]),
    All.

b(X) -> list_to_binary(X).

i(X) -> integer_to_list(X).

i10(X) ->
    S = i(X),
    pad(10-length(S)) ++  S.

pad(N) when N > 0 -> [$0|pad(N-1)];
pad(_)            -> [].









