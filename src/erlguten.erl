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
%% Authors:   Joe Armstrong <joe@sics.se>
%% Purpose: Main program
%%==========================================================================

-module(erlguten).

-export([batch/1, get_tag_schema/2, parse_flow/1]).

-export([test/0,bug/0]).

-import(lists, [map/2]).

-include("eg.hrl").

test() -> format("test1.map").

bug() -> format("test2.map").
    

batch([X]) ->
    format(atom_to_list(X)).

%% A map file has the following structure
%% <data page="1">
%% <template name="template1">
%% <intro>
%%   <p>...</p>
%%   <q>...</q>
%% </intro>
%% <body>
%%  ...
%% </body>
%% </template>
%%
%%  Logic:
%%    in Env
%%    set template=template1
%%    set current page=1
%%    enter intro
%%      set currentBox = intro
%%      set dict entry {initialised, Page, one}
%%      set dict entry {free, Page, intro} = 1
%%      set tagMap for template1:tagMap(one, intro)
%%      call template1:handler(p, Args, Data, Env)  -> Env1
%%      call template2:handler(q, Args, Data, Env1) -> Env2
%%      etc.

format(File) ->
    V = eg_xml_lite:parse_file(File),
    io:format("read:~p~n",[V]),
    Out = filename:rootname(File) ++ ".pdf",
    case V of
	{error, _W} ->
	    io:format("Error in source(~s):~p~n",[File, V]),
	    exit(1);
	[{pi,_},{xml,{data, [{"page", N}], Templates}}] ->
	    Page = list_to_integer(N),
	    io:format("Data starts on page:~p~n",[Page]),
	    PDF  = eg_pdf:new(),
	    Env = #env{page=Page, pdf=PDF, dict=dict:new()},
	    loop(Templates, Env),
	    {Serialised, _PageNo} = eg_pdf:export(PDF),
	    file:write_file(Out,[Serialised]),
	    io:format("Created a file called:~p~n",[Out]),
	    eg_pdf:delete(PDF);
	_ ->
	    io:format("bad XML - must begin \"<?xml ...\n<flow \n"),
	    exit(1)
    end.

loop([{template,Args,Data}|T], Env) ->
    io:format("tempate Args=~p~n",[Args]),
    Template= get_template_name(Args),
    io:format("Template:~p~n",[Template]),
    Env1 = Env#env{template=Template},
    io:format("tempate data=~p~n",[Data]),
    Env2 = instanciate_template(Template, Env1),
    Env3 = format_boxes(Data, Env2),
    loop(T, Env3);
loop([], Env) ->
    Env.

format_boxes([{Box,_Args,Data}|T], Env) ->
    Env1 = initialise_box(Box, Env),
    %% loop over the paragraphs in the Box
    Env2 = format_paragraphs(Data, Box, Env1),
    format_boxes(T, Env2);
format_boxes([], E) ->
    E.

initialise_box(Box, E) ->
    #env{dict=Dict, page=Page, pdf=PDF, template=Template}=E,
    Dict1 = dict:store({free,Page,Box}, 1, Dict),
    B = Template:box(Box),
    #box{x=XX,y=YY,leading=Lead, width=Width, lines=Lines} = B,
    eg_pdf_lib:draw_box(PDF, XX, YY,Width, Lead,Lines),
    Env1 = E#env{dict=Dict1, currentBox=Box},
    %% initialse the tagMap
    initialise_tagMap(Template, Box, Env1).

format_paragraphs([{ParaTag,Args,Data}|T], Box, Env) ->
    Template = Env#env.template,
    case (catch Template:handler(Box, ParaTag, Args, Data, Env)) of
	{'EXIT', Why} ->
	    io:format("oops ~w: ~w ~w Args=~p Data=~p~n",[Template,Box, 
							 ParaTag,
						      Args,Data]),
	    io:format("Why=~p~n",[Why]),
	    exit(1);
	Env1 ->
	    format_paragraphs(T, Box, Env1)
    end;
format_paragraphs([], _Box, E) ->
    E.

initialise_tagMap(Template, Box, E) ->
    Ts = case (catch Template:tagMap(Box)) of
	    {'EXIT', _Why} ->
		 io:format("error in tagmap for ~p:~p~n",
			   [Template,Box]),
		 io:format("using defualt~n"),
		 default_tagmap();
	     L -> L
	 end,
    PDF = E#env.pdf,
    TagMap = map(fun(I) ->
			 io:format("Tagmap entry=~p~n",[I]),
			 eg_pdf:ensure_font_gets_loaded(PDF, I#tagMap.font),
			 #tagMap{font=F, size=Psize, color=Color, 
				 voff=V, break=Break, name=N} = I,
			 {N, eg_richText:mk_face(F, Psize, Break, Color, V)} 
		 end, Ts),
    E#env{tagMap=TagMap}.

    
default_tagmap() ->
    [#tagMap{name=defult,font="Times-Roman",size=11},
     #tagMap{name=em,font="Times-Italic", size=11},
     #tagMap{name=code,font="Courier",size=11,break=false}].

instanciate_template(Template, E) ->
    #env{dict=Dict,page=Page,pdf=PDF} = E,
    _E1 = case dict:find(Key={initialised, Page, Template}, Dict) of
	     error ->
		 io:format("calling first instanciation Page:~p "
			   "Template: ~p ~n", [Page, Template]),
		 Template:on_instanciation(Page, PDF),
		 Dict1 = dict:store(Key, true, Dict),
		 E#env{dict=Dict1};
	     _ ->
		 E
	 end.
    
get_tag_schema(Tag, [H|T]) ->
    case H of
	Tag -> H;
	_   -> get_tag_schema(Tag, T)
    end;
get_tag_schema(Tag, []) ->
    exit({missing,tag,Tag}).

parse_flow([{"galley",F},{"name",_Tag}]) ->
    case eg_xml_lite:parse_file(F) of
	{error, E} ->
	    io:format("Error in galley(~p):~p~n",[F, E]),
	    exit(1);
	_L ->
	    %G = parse_galley(F, L),
	    %get_box(Tag, G)
	    true
    end.

get_template_name([{"name", N}]) ->
    list_to_atom(N).








