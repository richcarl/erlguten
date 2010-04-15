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
%% Authors:   Joe Armstrong   <joe@sics.se>
%%            Mikael Karlsson <mikael.karlsson@creado.com>
%% Purpose: Erlguten PDF library routines
%%==========================================================================

%% @doc ErlGuten PDF library routines
%% @type pdfobjects() = [pdfobject()]
%% @type pdfobject() = {objkey(), pdftype()}
%% objkey() = {obj, Ref, Gen}
%% Ref = integer()
%% Gen = 0
%% @type pdftype() = dict() | stream() | ptr() | name() | array()
%%                |  pdfstring() | boolean() | date() | number() | null()
%%  dict()    = {dict, [{string(), pdftype()}]}
%%  stream()  = {stream, dict(), deeplist() | binary()}
%%            | {stream, deeplist()| binary()}
%%  name()    = {name, string}
%%  ptr()     = {ptr, Ref, Gen}
%%  array()   = {array, [pdftype()]}
%%  boolean() = true | false
%%  null()    = null
%%  date()    = {date, Date}
%%  Date      = {Year, Month, Day} | {{Year, Month, Day},{Hour, Min, Sec}}
%%  Ref       = integer()
%%  pdfstring() = {string, string()} | {hexstring, string()}
%%  deeplist()  = deeplist() | string()

-module(eg_pdf_lib).

-export([find_in_dict/2, get_next_ref/1, get_ref/1, 
	 make_object/2, 
	 make_object_key/1, search_object/2, store_object/2, add_object/2, 
	 delete_object/2, pdf_item/1, pdf_item/2, 
	 store_in_dict/2, serialise2bin/1, serialise/1 ]).

-export([pdf_object_dict_item/2, is_pdf_object_type/2,  
	 get_objects_of_type/2, export/2]).

-export([draw_box/6, showGrid/2, moveAndShow/4, moveAndShow/5,
	 moveAndShowRight/5, moveAndShowRot/5, code128/1]).

%% @spec search_object(Key, Objects::pdfobjects()) -> {value, Object} | false
%% Key = objkey() | ptr() | integer()
%% Object = pdfobject()
%% @doc Find PDFItem related object reference. 
%% All pdftypes are <code>{Key, Value}</code> tuples where the Key indicates
%% the type.

search_object(Ref, Objects) when is_integer(Ref) ->
    search_object({obj, Ref, 0}, Objects);
search_object({ptr,I,J} , Objects) ->
    search_object({obj, I, J}, Objects);
search_object({obj,_,_} = Key, Objects) ->
    lists:keysearch(Key, 1, Objects).

%% One shall not be able to remove items from the list, since the new Key
%% generation is based on the Objects list length, so delete is
%% implemented just to set an object to null
%% @spec delete_object(Key::objkey(), Objects::pdfobjects()) -> pdfobjects()
%% Key = objkey() | ptr() | integer()
%% @doc deletes a PDF object

delete_object(Ref, Objects) when is_integer(Ref) ->
    delete_object({obj, Ref, 0}, Objects);
delete_object({ptr,I,J} , Objects) ->
    delete_object({obj, I, J}, Objects);
delete_object(Key, Objects) ->
    case lists:keymember(Key, 1, Objects) of
       true ->
	   Object = {Key, null},
	   lists:keyreplace(Key, 1, Objects, Object);
       false ->
	   Objects
   end.

%% @spec store_object(Objects, Objects1::pdfobjects()) -> pdfobjects()
%% Objects = Object | [Object]
%% Object = pdfobject()
%% @doc Returns a copy of Objects1 with Objects added or, if the object's 
%% reference already is present, replacing the old object.
store_object({Key, PDFItem} = Object, Objects) ->
   case lists:keymember(Key, 1, Objects) of
	true ->
	    lists:keyreplace(Key, 1, Objects, Object);
	false ->
	    [ Object | Objects ]
    end;

store_object( [ Object| NewObjects], Objects ) ->
    store_object(NewObjects, store_object( Object, Objects));
store_object([], Objects) ->
    Objects.

%% @spec add_object(PDFItem::pdftype(), Objects::pdfobjects()) ->
%%           {Ref, NewObjects}
%% Ref = integer()
%% NewObjects = pdfobjects()
%% @doc Makes a new object from PDFItem, and adds it to Objects .
%% Returns the Reference number for the added object and the new Object list 
add_object(PDFItem, Objects) ->
    Ref = get_next_ref(Objects),
    NewObjects = store_object( make_object( Ref, PDFItem ), Objects ),
    { Ref, NewObjects }.


%% @spec pdf_item(Key::objkey(), Objects::pdfobjects()) -> pdftype()
%% @doc Returns the PDFItem for the Object corresponding to Key.
%% If no object is found and exception is generated.
pdf_item(Key, Objects) ->
    {value, Object} = search_object(Key, Objects),
    pdf_item(Object).

%% @spec pdf_item(Object::pdfobject()) -> pdftype()
%% @doc Returns the PDFItem for the Object.
pdf_item({ Key, Value}) ->
    Value.


%% @spec find_in_dict(Key::string(), Dict::dict()) -> pdftype() | undefined
%% @doc Finds a item in a PDF dictionary with Key 
find_in_dict(Key, {dict,Dict})->
    case lists:keysearch(Key, 1, Dict) of
	{value, {_ , A }} -> 
	    A;
	false ->
	    undefined
    end.


%% @spec store_in_dict({Key, Value}, Dict::dict()) -> dict()
%% Key = string()
%% Value = pdftype()
%% @doc Stores a new item in a PDF dictionary, replacing the old
%% one if it exists.
store_in_dict({Key, Value} = A, {dict, D}) ->
    case lists:keymember(Key, 1, D) of 
	true ->
	    {dict, lists:keyreplace(Key,1,D,A) }; 
	false ->
	    {dict, D ++ [A]} %% Append to keep "Type" etc. first.
    end.

%% @spec get_ref(Object::pdfobject()) -> Ref
%% Ref = integer()
get_ref({{obj, Ref,_},_}) ->
	       Ref.

%% @spec get_next_ref(Objects::pdfobjects()) -> Ref
%% Ref = integer()
%% @doc returns next available Reference count 
get_next_ref(Objects) ->
    length(Objects) + 1.

%% @spec make_object(Ref::integer(), PDFItem::pdftype()) -> pdfobject()
%% @doc Returns a PDF Object with Reference number Ref and PDFItem 
make_object(Ref, PDFItem) ->
    { make_object_key(Ref) , PDFItem }.

%% @spec make_object_key(Ref::integer()) -> objkey()
%% @doc Returns an Object Key with Reference number Ref 
make_object_key(Ref) ->
    { obj, Ref, 0 }.


%% @spec serialise2bin(Objects) -> binary()
%% Objects = pdfobject() | [pdfobject()]
%% @doc Returns a serialised object as a binary
serialise2bin(L) when is_list(L) ->
    lists:map(fun(A) ->
		      list_to_binary(serialise(A))
	      end, L);
serialise2bin(A) ->
    list_to_binary(serialise(A)).


%% @spec serialise(Object::pdfobject()) -> deeplist()
%% deeplist() = deeplist() | string()
%% @doc Returns a serialised object as a deep list of strings
serialise({{obj,I,J},K}) ->
    [eg_pdf_op:i2s(I)," ",eg_pdf_op:i2s(J)," obj\n", serialise(K),"endobj\n"];
serialise({stream, S}) -> 
    serialise({stream,{dict,[]}, S});
serialise({stream,{dict,K},L}) when is_list(L)  ->
    B = list_to_binary([L]),
    serialise({stream,{dict,K},B});
serialise({stream,{dict,Dict},B}) when is_binary(B)  ->
    Len = size(B),
    NewDict = store_in_dict({"Length", Len},{dict,Dict}),
    [serialise(NewDict),
     "\nstream\n",B,"\nendstream\n"
    ];
serialise({dict,L}) ->
    ["<<\n", lists:map(fun({I,J}) ->
			       ["/",I," ",serialise(J),"\n"]
		       end, L),
     ">>\n"];
serialise({name, S}) ->
    [" /",S," "];
serialise({string, S}) ->
    [" (",S,") "];
serialise({hexstring, S}) ->
    [" <",s2hs(S),"> "];
serialise({ptr, I, J}) ->
    [" ",eg_pdf_op:i2s(I)," ",eg_pdf_op:i2s(J)," R "];
serialise({array, L}) ->
    [" [ ", lists:map(fun(I) -> serialise(I) end, L), " ] "];
serialise({date, Date}) ->
    ["(D:", date(Date),")"];
serialise({rect, {A,B,C,D}}) ->
    serialise({array,[A,B,C,D]});
serialise(true) ->
    " true ";
serialise(false) ->
    " false ";
serialise(null) ->
    " null ";
serialise(N) when is_integer(N) ->
    [" ",eg_pdf_op:i2s(N), " "];
serialise(F) when is_float(F)->
      [" ", eg_pdf_op:f2s(F), " "];
serialise(X) ->
    io:format("I cannot serialise:~p~n", [X]),
    exit(serialise).

date({Year,Month,Day}) when Year < 100 ->
    date({Year+2000,Month,Day});
date({Year,Month,Day}) ->
    d2s([Year,Month,Day]);
date({YMD,{H,MIN,SEC}})->
    date(YMD) ++ d2s([H,MIN,SEC]).

d2s([])->
    [];
d2s([H|T]) when H < 10->
    "0"++ eg_pdf_op:i2s(H) ++ d2s(T);
d2s([H|T]) ->
    eg_pdf_op:i2s(H) ++ d2s(T).

%% Erlang string to Hex string
s2hs([]) ->[];
s2hs([H|T]) ->
    A = H band 16#0F,
    B = (H band 16#F0) bsr 4,
    [nibble2c(B), nibble2c(A) | s2hs(T)].

nibble2c(N) when N < 10 -> N + $0;
nibble2c(N) -> N-10+$A.

%% =======================================================

%% @spec pdf_object_dict_item(Key::string(), Object::pdfobject()) -> ObjectType
%% ObjectType = pdftype() | undefined | not_dict
%% @doc Returns the value corresponding to Key from a PDF Objects dictionary. 
%% ("Pages", "Page", "Font", etc). 
%% If the Key is missing undefined is returned
%% If the Object is not a dictionary not_dict is returned
pdf_object_dict_item(Key, Object) ->
    case pdf_item(Object) of
	{dict,Dict} ->
	    find_in_dict(Key,{dict,Dict});
	_ ->
	    not_dict
    end.

%% @spec is_pdf_object_type(Type::string(), Object::pdfobject()) ->
%%          true | false
%% @doc Returns true if the object has the type name Type, otherwise false. 
%% PDF object types are ("Pages", "Page", "Font", etc).
is_pdf_object_type(Type, Object) ->
    case pdf_object_dict_item("Type", Object) of
	{name, Type} -> true;
	_  -> false
    end.

%% @spec get_objects_of_type(Type::string(), Objects::pdfobjects()) ->
%%           pdfobjects()
%% @doc Returns a list of objects with dictionary key "Type" = Type 
%% PDF object types are ("Pages", "Page", "Font", etc).
get_objects_of_type(Type, Objects) ->
        lists:filter(fun(Object) -> 
			     is_pdf_object_type(Type, Object) 
		     end,  
		     Objects).

%% ===============================================================
%% @spec export(InfoRef::integer(),Objects::pdfobjects()) -> binary()
%% @doc Make a pdf document ready for export to 
%% file or any other media (network etc.).
export(InfoRef, Objects) ->
    SortedObjects = lists:keysort(1,Objects),
    BObjects = serialise2bin(SortedObjects),
    b([
       startmark(),
       pdfbmagic(),
       BObjects,
       xref(BObjects),
       trailer(InfoRef, SortedObjects),
       startxref(BObjects),
       endmark()
      ]).

startmark() -> "%PDF-1.4".
endmark() ->   "%%EOF\r\n".
%% pdfbmagic() ->
%%   "zG_\\325\\371\\337J\\244\030\\267\\260#s6\\037\\246dR L\\204s\\037".
%% pdfbmagic() ->
%%   [8#015,$%,8#342,8#343,8#317,8#323, 8#015,8#012].
pdfbmagic()->
    ["\n"].

%% xref
%% 0 9
%% 0000000000 65535 f 
%% 0000000033 00000 n 
%% 0000000098 00000 n 
%% 0000000144 00000 n 
%% 0000000203 00000 n 
%% 0000000231 00000 n 
%% 0000000409 00000 n 
%% 0000000721 00000 n 
%% 0000000835 00000 n 
%% trailer
%% <<
%% /Size 9
%% /Root 1 0 R
%% /Info 8 0 R
%% >>
%% startxref
%% 1073
%% %%EOF
xref(Objects) ->
    {XRefs, _EndAccu} = 
	lists:mapfoldl( 
	  fun xref/2, length(startmark()) + length(pdfbmagic()), 
	  Objects ),
    ["xref\n",
     "0 ",i(nobjects(Objects) + 1), "\n",
     xref1(0,"65535 f"),
     XRefs
    ].

xref(Obj, Pos) ->
    {xref1(Pos, "00000 n"), Pos + objsize(Obj)}.

xref1(I, Str) ->
    lists:flatten(io_lib:format("~10.10.0w ~s \n", [I,Str])).

trailer(InfoRef, Objects) ->
    [Root] = get_objects_of_type("Catalog", Objects),
    RootRef = get_ref(Root),
    ["trailer\n",
     "<<\n",
     "/Size ",i(nobjects(Objects) + 1), "\n",
     "/Root ",i(RootRef), " 0 R\n",
     "/Info ", i(InfoRef), " 0 R\n",
     ">>\n"].


startxref(Objects) ->
    ["startxref\n",
     i(lists:foldl(fun(A, Accu) -> objsize(A) + Accu end, 
		   length(startmark()) + length(pdfbmagic()), 
			  Objects)), 
     "\n"].


%% ----------------------------------------------------

nobjects(Objects) ->
    length(Objects).

%% The length of the indirect object
objsize(Obj) when is_binary(Obj)
		  -> size(Obj);
objsize(Obj) when is_list(Obj) ->
    size(b(Obj)).

b(X) -> list_to_binary(X).
i(X) -> integer_to_list(X).

%% -------------------------------------
    
%% showGrid(PDF, a4 | usLetter) 
%%   adds a grid to the current page page

sizeOfPaper(a4) ->
    {595, 842};
sizeOfPaper(usLetter) ->
    {612, 792}.

showGrid(PDF, Paper) ->
    {PaperWidth, PaperHeight} = sizeOfPaper(Paper),
    %% Top = PaperHeight - 10,
    Top = 825, % hack
    Bottom = 10,
    Left = 10,
    %% Right = PaperWidth - 10,
    Right = 575,
    eg_pdf:set_font(PDF,"Helvetica", 8),
    vlines(PDF, Left, Right, Top, Bottom),
    hlines(PDF, Left, Right, Top, Bottom).

hlines(PDF, Left, Right, Top, Bottom) ->
    diter(Top,25,10,
	  fun(Y) ->
		  %% eg_pdf:set_fill_gray(PDF,1.0),
		  eg_pdf:line(PDF, Left, Y, Left+20, Y),
		  eg_pdf:line(PDF, Right, Y, Right-20, Y),
		  %% eg_pdf:set_fill_gray(PDF,0.8),
		  eg_pdf:line(PDF, Left+20,Y,Right-20,Y),
		  moveAndShow(PDF, Left, Y+2, eg_pdf_op:n2s(Y)),
		  moveAndShow(PDF, Right-20, Y+2, eg_pdf_op:n2s(Y)),
		  true
	  end).

vlines(PDF, Left, Right, Top, Bottom) ->
    diter(Right,25,10,
	  fun(X) ->
		  eg_pdf:line(PDF, X, Top, X, Top-20),
		  moveAndShow(PDF, X-5, Top-35,eg_pdf_op:n2s(X)),
		  eg_pdf:line(PDF, X, Bottom, X, Bottom+20),
		  eg_pdf:line(PDF, X, Top -40, X, Bottom + 35),
		  moveAndShow(PDF, X-5, Bottom+23,eg_pdf_op:n2s(X))
	  end).

moveAndShow(PDF, X, Y, Str) ->
    eg_pdf:begin_text(PDF),
    eg_pdf:set_text_pos(PDF, X, Y),
    eg_pdf:text(PDF, Str),
    eg_pdf:end_text(PDF).

moveAndShowRot(PDF, X, Y, Str, Rot) ->
    eg_pdf:save_state(PDF),
    eg_pdf:begin_text(PDF),
    eg_pdf:rotate(PDF, Rot),
    eg_pdf:set_text_pos(PDF, X, Y),
    eg_pdf:text(PDF, Str),
    eg_pdf:end_text(PDF),
    eg_pdf:restore_state(PDF).

moveAndShow(PDF, X, Y, Str, Scale) ->
    eg_pdf:begin_text(PDF),
    eg_pdf:set_text_pos(PDF, X, Y),
    eg_pdf:set_text_scale(PDF, Scale),
    eg_pdf:text(PDF, Str),
    eg_pdf:set_text_scale(PDF, 100),
    eg_pdf:end_text(PDF).

%% Str is drawn to the left of X (X is the _right_ alignment side of the Str
%% text box)
moveAndShowRight(PDF, {Font,Size}, X, Y, Str) when is_integer(X),
						   is_integer(Y) ->
    Width = eg_pdf:get_string_width(PDF, Font, Size, Str),
    moveAndShow(PDF, X-Width, Y, Str);
moveAndShowRight(_,_,_,_,_)  ->
    ok.

%% downwards iterator

diter(X, Inc, Stop, F) when X < Stop ->
    true;
diter(X, Inc, Stop, F) ->
    F(X), diter(X-Inc,Inc,Stop,F).

draw_box(PDF, X, Y, Measure, Lines, MaxRows) ->    
    eg_pdf:append_stream(PDF, draw_box1(X, Y, Measure, Lines, MaxRows)).

draw_box1(X1,Y1,Measure,Leading,MaxRows) ->
    %% X1,Y1,Leading are in points
    %% Measure        is in picas
    X2 = X1 + Measure,
    Y2 = Y1 - Leading*MaxRows, 
    [" q  0.4 g 0.4 G 0 w ",
     %% verticals
     line(X1,Y1,X1,Y2),
     line(X2,Y1,X2,Y2),
     for(0, MaxRows,
	 fun(I) ->
		 Y = Y1 - I*Leading,
		 line(X1,Y,X2,Y)
	 end),
     " Q "].

for(I, Max, F) when I > Max -> [];
for(I, Max, F)              -> [F(I)|for(I+1,Max,F)].

line(X1,Y1,X2,Y2) -> [eg_pdf_op:i2s(X1)," ",eg_pdf_op:i2s(Y1)," m ",
		      eg_pdf_op:i2s(X2)," ",eg_pdf_op:i2s(Y2)," l S "].

-define(B_SHIFT_C, 99).
-define(C_SHIFT_B, 100).
-define(START_A, 103).
-define(START_B, 104).
-define(START_C, 105).
-define(STOP,    106).

-define(DIGIT(X), X >= $0, X =< $9). 
-define(l2i(X), list_to_integer(X)).

code128(String0) ->
    {Start,Mode} = code128_start(String0),
    String1 = code128_conv(Mode, String0, []),
    CheckChar = code128_chk(String1, Start),
    Result = lists:flatten([Start|String1]++[CheckChar,?STOP]),
    [code128_trans(X) || X <- Result].

code128_trans(X) when X >= 95 -> 
    X + 97;
code128_trans(X) ->
    X + 32.

code128_chk(String, StartChar) ->
    F = fun(C, {N,Acc}) -> {N+1, Acc+(N*C)} end,
    {_,Sum} = lists:foldl(F, {1, StartChar}, String),
    (Sum rem 103).
				       
code128_conv(_, [], Acc) ->
    lists:reverse(Acc);

code128_conv(c, [A,B,C,D|R], Acc) when ?DIGIT(A),?DIGIT(B),
				       ?DIGIT(C),?DIGIT(D) ->
    code128_conv(c, R, [?l2i([C,D]),?l2i([A,B])|Acc]);
code128_conv(c, R, Acc) ->
    code128_conv(b, R, [?C_SHIFT_B|Acc]);

code128_conv(b, R=[A,B,C,D|_], Acc) when ?DIGIT(A),?DIGIT(B),
					 ?DIGIT(C),?DIGIT(D) ->
    code128_conv(c, R, [?B_SHIFT_C|Acc]);
code128_conv(b, [C|R], Acc) ->
    code128_conv(b, R, [C-32|Acc]).

code128_start([A,B,C,D|_]) when ?DIGIT(A),?DIGIT(B),?DIGIT(C),?DIGIT(D) ->
    {?START_C,c};
code128_start(String0) ->
    {?START_B,b}.
