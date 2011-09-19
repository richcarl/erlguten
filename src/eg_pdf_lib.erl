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
%% Purpose: Erlguten PDF encoding routines
%%==========================================================================

%% @doc ErlGuten PDF encoding routines
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
