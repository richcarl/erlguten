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
%% Author: Mikael Karlsson <mikael.karlsson@creado.com>
%% Purpose: PDF Annotations
%%==========================================================================

%% @doc Annotations.
%%
%% <p>Purpose: Create and serialise PDF Annotations </p>
%% <p>       Ref. Chapter 8.4 Annotations in PDF reference v1.4 </p>
%% 
%% <p>
%% <dl>
%%  <dt>/Type</dt>    <dd>/Annot</dd>
%%  <dt>/Subtype</dt> <dd>name, Required</dd>
%%  <dt>/Contents</dt>
%%    <dd>text string. Required or optional depending on subtype</dd>
%%  <dt>/P</dt>       
%%    <dd>dictionary. Optional indirect ref. to the page object</dd>
%%  <dt>/Rect</dt>    <dd>rectangle. Required</dd>
%%  <dt>/NM</dt>      <dd>text string. Optional, annotation name</dd>
%%  <dt>/M</dt>       
%%    <dd>Date or string. Optional, last modified date and time</dd>
%%  <dt>/F</dt>       
%%    <dd>Integer. optional, Flags specifying various characteristics</dd>
%%  <dt>/BS</dt>       <dd>Dictionary. Optional, Border style</dd>
%%  <dt>/Border</dt>  
%%    <dd>Array. Optional, annotation border characteristics. 
%%       Use bs inst.</dd>
%%  <dt>/AP</dt>      <dd>Dictionary. Appearance</dd>
%%  <dt>/AS</dt>      
%%     <dd>Name. Required if ap contains one or more subdirectories.
%%         Appearance state, selects applicable appearance stream.</dd>
%%  <dt>/C</dt>       
%%    <dd>Array of 3 numbers range 0-1. Optional. RGB color, for icon etc.</dd>
%%  <dt>/CA</dt>      
%%    <dd>Number 0-1, Optional. Constant opacity value 0=no contr. 1=full</dd>
%%  <dt>/T</dt>     <dd>Text string. Optional. Text label in title bar.</dd>
%%  <dt>/Popup</dt>   
%%   <dd>Dictionary. Optional. Indirect reference to popup annot for edit.</dd>
%%  <dt>/A</dt>       
%%     <dd>Dictionary. Optional. Action to be performed when activated.</dd>
%%  <dt>/AA</dt>      <dd>Dictionary. Optional. Additional actions</dd>
%%  <dt>/StructParent</dt>
%%    <dd>Integer. Required if annotation is a structural content item.</dd>
%% </dl>
%% </p>
%% @end

-module(eg_pdf_annot).

-include("eg.hrl").


-export([new_text/2, new_text/3, new_open_text/2, new_open_text/3, 
	 new_link/2, new_link/3, new_action/2, new_line/3, set_flags/2,
         andflags/2, orflags/2, contents/1, flag/1, isflagset/2, rect/1,
	 set_borderstyle/3, style/1, serialise/2]).

%% ============================================================================

%% Subtypes
subtype(text)             -> {name, "Text"};
subtype(link)             -> {name, "Link"};
subtype(freetext)         -> {name, "FreeText"};
subtype(line)             -> {name, "Line"};
subtype(square)           -> {name, "Square"};
subtype(circle)           -> {name, "Circle"};
subtype(highlight)        -> {name, "Highlight"};
subtype(underline)        -> {name, "Underline"};
subtype(squiggly)         -> {name, "Squiggly"};
subtype(strikeout)        -> {name, "StrikeOut"};
subtype(stamp)            -> {name, "Stamp"};
subtype(ink)              -> {name, "Ink"};
subtype(popup)            -> {name, "Popup"};
subtype(fileattachement)  -> {name, "FileAttacheMent"};
subtype(sound)            -> {name, "Sound"};
subtype(movie)            -> {name, "Movie"};
subtype(widget)           -> {name, "Widget"};
subtype(printermark)      -> {name, "PrinterMark"};
subtype(trapnet)          -> {name, "TrapNet"}.

contents(Contents)        -> {string, Contents}.

%% @type rect() = {X, Y, Width, Height}
rect({A,B,C,D})           -> {array, [A,B,C,D]}.
    
%% Flags
flag(invisible) -> 1;
flag(hidden)    -> 2;
flag(print)     -> 4;
flag(nozoom)    -> 8;
flag(norotate)  -> 16;
flag(noview)    -> 32;
flag(readonly)  -> 64.


%% @spec new(Subtype, Rect) -> dict()
%% Rect = {rect, {X,Y, Width, Height}}
new(SubType, {rect,{X,Y,W,H}} = Rect)->
    {dict,[{"Type",{name,"Annot"}},
	   {"Subtype", subtype(SubType)}, 
	   {"Rect", Rect }]}.

new(SubType, Rect, Contents)->
    A = new(SubType, Rect),
    set_contents(Contents,A).

%% @spec new_text(Rect::rect(), Contents::string()) ->
%%       text_annotation()
%% @doc Text annotations represents a sticky note

new_text(Rect, Contents)->     
    new(text,Rect,Contents).

%% Default when Name is not given is Note
%% @spec new_text(Rect::rect(), Contents::string(), name()) ->
%%       text_annotation()
new_text(Rect, Contents, Name)-> 
    A = new_text(Rect, Contents),
    store("Name", name(Name), A).

%% @spec new_open_text(Rect::rect(), Contents::string()) ->
%%       text_annotation()
%% @doc Text annotation that is open

new_open_text(Rect, Contents)-> 
    A = new_text(Rect, Contents),
    store( "Open", true, A ).

%% @spec new_open_text(Rect::rect(), Contents::string(), name()) ->
%%       text_annotation()
new_open_text(Rect, Contents, Name)-> 
    A = new_text(Rect, Contents),
    B = store("Name", name(Name), A),
    store( "Open", true, B ).

%% @type name() = comment|help|insert|key|note|paragraph

name(comment)     -> {name, "Comment"};
name(help)        -> {name, "Help"};
name(insert)      -> {name, "Insert"};
name(key)         -> {name, "Key"};
name(newparagraph)-> {name, "NewParagraph"};
name(note)        -> {name, "Note"};
name(paragraph)   -> {name, "Paragraph"}.


%% ------ Link annotations are either hyper text links
%%       or actions to performed
%% @spec new_link(Rect::rect(), Dest::destination()) -> dict()
new_link(Rect, Dest)->     
    A = new(link,Rect),
%%    [ PageNo | T ] = Dest, 
    store("Border",{array,[16,16,1]}, store("Dest", Dest, A )).



%% @spec new_link(Rect::rect(), Dest::destination(), Highlight) -> dict()
%% Highlight = none | invert | outline | push

new_link(Rect, Dest, Highlight)-> 
    A = new_link(Rect, Dest),
    store( "H", highlight(Highlight), A ).

    

highlight(none)    -> {name, "N"};
highlight(invert)  -> {name, "I"}; %% Default
highlight(outline) -> {name, "O"};
highlight(push)    -> {name, "P"}.


%% Action is a predefined action dictionary
new_action(Rect, {action, Action})->
    A = new(link, Rect),
    store("A", {action, Action}, A).

%% ------ Line annotations 
new_line(Rect, Contents, {Point1, Point2})->     
    A = new(line,Rect, Contents),
    {X1, Y1} = Point1, {X2, Y2} = Point2, 
    store("L", {array,[X1, Y1, X2, Y2]}, A ).


set_contents(Contents, Annot)->
    store("Contents", {string,Contents}, Annot).

store(Key, Value, Dict)->
    eg_pdf_lib:store_in_dict({Key, Value}, Dict).


set_borderstyle(Width, Style, Annot)->
    S = style(Style),
    A = {dict,[{"Type",{name,"Border"}},
	       {"W", Width},
	       {"S", S}]},
    store("BS", A, Annot).

style(solid)     -> {name, "S"};
style(dashed)    -> {name, "D"};
style(beveled)   -> {name, "B"};
style(inset)     -> {name, "I"};
style(underline) -> {name, "U"}.
    
%% Annotation flags

orflags(Flag, Flags)->
    flag(Flag) bor Flags.

andflags(Flag, Flags)->
    flag(Flag) band Flags.

isflagset(Flag, Flags)->
    case andflags(Flag, Flags) of
	0 ->
	    false;
	_ ->
	    true
    end.

set_flags(Flags, Annot)->
    store("F", Flags, Annot).



serialise(Annots, I)->
    serialise(Annots, I, [], []).

serialise([], I, Is, Os) -> 
    A = {{obj,I,0},{array,lists:map(fun(J) ->
					    {ptr,J,0}
				    end, 
				    lists:reverse(Is))}},
    {I+1, {ptr,I,0}, lists:reverse([A|Os])};
serialise([H|T], I, Fs, E) ->
    O = mk_annot(I, H),
    serialise(T, I+1, [I|Fs], [O|E]).

mk_annot(I, {annot,A}) ->
    {{obj,I,0},	  
     {dict,dict:to_list(A)}
    }.
