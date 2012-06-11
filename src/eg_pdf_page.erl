%%==========================================================================
%% Copyright (C) 2004 Mikael Karlsson
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
%% Purpose: PDF Pages
%%==========================================================================

%% @doc Page.
%%
%% <p>Purpose: Create PDF Pages </p>
%% <p>       Ref. Chapter 3.6.2 Page objects in PDF reference v1.4 </p>
%% 
%% <p>
%% <dl>
%%  <dt>/Type</dt>    <dd>/Page</dd>
%%  <dt>/Parent</dt> <dd>dictionary, Required; indirect reference. The parent
%%      page tree node.</dd>
%%  <dt>/LastModified</dt>
%%    <dd>date, Required if PieceInfo</dd>
%%  <dt>/Resources</dt>       
%%    <dd>dictionary. Required, inheritable. The resources required by the 
%%        page.</dd>
%%  <dt>/MediaBox</dt>    <dd>rectangle. Required, inheritable. Boundaries 
%%        of physical medium.</dd>
%%  <dt>/CropBox</dt>     <dd>rectangle. Optional, inheritable. Visible region
%%        of default user space. Defaults to value of MediaBox</dd>
%%  <dt>/BleedBox</dt>       
%%    <dd>rectangle, Optional. Clipping region for production environment.
%%        Defaults to value of CropBox.</dd>
%%  <dt>/TrimBox</dt>       
%%    <dd>rectangle, Optional. Intended dimensions of finished page after
%%        trimming.</dd>
%%  <dt>/ArtBox</dt><dd>rectangle, Optional. 
%%       Intended dimensions of the finished page after trimming.</dd>
%%  <dt>/BoxColorInfo</dt>  
%%    <dd>dictionary, Optional. Se PDF reference</dd>
%%  <dt>/Contents</dt>      <dd>stream or array. Content stream.</dd>
%%  <dt>/Rotate</dt>      
%%     <dd>integer, Optional, inheritable. The number of degrees the page
%%         is rotated cloclwise when displayed.</dd>
%%  <dt>/Group</dt>       
%%    <dd>dictionary, Optional. See PDF reference</dd>
%%  <dt>/Thumb</dt> <dd>stream, Optional. Stream object defining the page's
%%     thumbnail image.</dd>
%%  <dt>/B</dt>     <dd>array, Optional. Array of indirect references to 
%%       article beads appearing on the page.</dd>
%%  <dt>/Dur</dt>   <dd>number, Optional. Display duration in number of 
%%      seconds during presentations.</dd>
%%  <dt>/Trans</dt>     <dd>dictionary, Optional. Transition dictionary
%%       for presentations</dd>
%%  <dt>/Annots</dt>      <dd>array, Optional. Array of annotation 
%%   dictionaries.</dd>
%%  <dt>/AA</dt> <dd>dictionary, Optional. Additional actions dictionary</dd>
%%  <dt>/Metadata</dt> <dd>stream, Optional. Metadata for the page</dd>
%%  <dt>/PieceInfo</dt> <dd>dictionary, Optional. See PDF reference.</dd>
%%  <dt>/StructParents</dt> <dd>integer, Required if the page contains
%%      structural content items.</dd>
%%  <dt>/ID</dt> <dd>string, Optional. Digital id of the page parent Web 
%%     Capture content set.</dd>
%%  <dt>/PZ</dt> <dd>number, Optional. Preferred zoom factor.</dd>
%%  <dt>/SeparationInfo</dt> <dd>dictionary, Optional. Separation dictionary
%%     to generate color separations fopr the page.</dd>
%% </dl>
%% </p>
%% @end

-module(eg_pdf_page).

-export([page/3,
	 append_page/2, 
	 append_page2tree/2, 
	 append_to_page/3, 
	 get_page/2, 
	 get_page_contents/2, 
	 page_tree/5]).

%% ============================================================================


%% @spec page(ParenRef::integer(), ContentRef::integer(), Options) -> dict()
%% Options = [Option]
%% Option = {'"LastModified"', date()} | {'"Resources"', dict() } | 
%% {'"Annots"', array() } | {Key, pdftype()}
page(ParentRef, ContentsRef, Options) ->
    {dict, [{"Type", {name,"Page"}},
	    {"Parent", {ptr,ParentRef,0}},
	    {"Contents", {ptr, ContentsRef, 0}}
	    | lists:map( fun page_opt/1, Options ) ]}.

page_opt({"LastModified" = A, {date, Date}}) ->
    {A,{date, Date}};
page_opt({"Resources", {dict,_}} = A) ->
    A;
page_opt({"Resources", {ptr,_,_}} = A) ->
    A;
page_opt({"Annots", _}=A ) ->
    A;
page_opt({A,B}) ->
    {A,B}.

%% @spec page_tree(KidRefs, FontsPtr::ptr(), XObjectsPtr::ptr(), 
%%                 MediaBox::rect(), ProcSet) -> dict()
%% KidRefs = [ number() ]
%% ProcSet = imageb | imagec | imagebc
%% @doc Creates a Pages (Page Tree) dictionary
page_tree(KidRefs, FontsPtr, XObjectsPtr, MediaBox = {rect,{_A,_B,_C,_D}}, 
	  ProcSet ) ->
    ImProcSet = case ProcSet of
		    imagebc -> [{name, "ImageB"},{name, "ImageC"}];
		    imageb -> [{name, "ImageB"}];
		    imagec -> [{name, "ImageC"}];
		    _ -> []
		end,
    {dict,[{"Type",{name,"Pages"}},
	   {"Count",length(KidRefs)},
	   {"MediaBox", MediaBox },
	   {"Kids",{array, lists:map(fun(I) ->{ptr,I,0} end, KidRefs)}},
	   {"Resources",
	    {dict,[{"Font", FontsPtr },{"XObject", XObjectsPtr },
		   {"ProcSet", {array,[{name,"PDF"},{name,"Text"} | 
				       ImProcSet]}}]}}]}.


append_page(PageContents, Objects) ->
    [PageTreeObj] = eg_pdf_lib:get_objects_of_type("Pages", Objects),
    PTRef = eg_pdf_lib:get_ref(PageTreeObj),
    {ContentRef,Objects1} = eg_pdf_lib:add_object({stream, PageContents}, 
						  Objects),
    {PageRef, Objects2}   = eg_pdf_lib:add_object(page(PTRef,ContentRef,[]), 
						  Objects1),
    NewPTreeItem = append_page2tree(PageRef, eg_pdf_lib:pdf_item(PageTreeObj)),
    Objects3 = eg_pdf_lib:store_object(
		 eg_pdf_lib:make_object(PTRef, NewPTreeItem),  Objects2),
    Objects3.


append_page2tree(Ref, PageTreeItem) ->
    Key = "Kids",
    {array, Kids} = eg_pdf_lib:find_in_dict(Key, PageTreeItem),
    NewKids = Kids ++ [{ptr, Ref, 0}], 
    eg_pdf_lib:store_in_dict({Key, {array, NewKids}}, PageTreeItem).

get_page(PageNo, Objects) ->
    [PageTreeObj] = eg_pdf_lib:get_objects_of_type("Pages", Objects),
    PageTree = eg_pdf_lib:pdf_item(PageTreeObj), 
    {array, Kids} = eg_pdf_lib:find_in_dict("Kids", PageTree),
    PagePtr = lists:nth(PageNo, Kids),
    case eg_pdf_lib:search_object(PagePtr, Objects) of
	{value, Object} ->
	    Object;
	false ->
	    no_page_available
    end.

get_page_contents(PageObj, Objects) ->
    PageDict = eg_pdf_lib:pdf_item(PageObj),
    case eg_pdf_lib:find_in_dict("Contents", PageDict) of
	{ptr, _I, _J} = Ptr ->
	    {value, Object} = eg_pdf_lib:search_object(Ptr, Objects),
	    Object;
	PtrList ->
	    lists:map(fun(Ptr) ->
		      {value, Object} = eg_pdf_lib:search_object(Ptr, Objects),
		      Object 
		      end, PtrList)
    end.

append_to_page(S, PageObj, Objects) ->    
    {Key, {stream, Str}} = get_page_contents(PageObj, Objects),
    eg_pdf_lib:store_object({Key, {stream, Str ++ S}}, Objects).



