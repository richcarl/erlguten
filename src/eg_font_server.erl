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
%%==========================================================================

-module(eg_font_server).

-include("eg.hrl").


-export([start/0, stop/0, char_width/2, data/1, info/1, kern/2]).

%% ============================================================================

start() ->
    case lists:member(fonts, ets:all()) of
	true ->
	    true;
	false ->
	    fonts = ets:new(fonts, [named_table,set,public]),
	    true
    end.

stop() ->
    ets:delete(fonts).

%% available_fonts() ->
%%     eg_afm:available_fonts().

%% ensure_loaded(Font, Index) ->
%%     %% io:format("Ensure_loaded font number=~p = ~s~n", [Index, Font]),
%%     case ets:lookup(fonts, {info, Index}) of
%% 	[_] ->
%% 	    true;
%% 	[] ->
%% 	    case eg_afm:read_font_info(Font) of
%% 		{ok, {afm_qdh1,Font,Widths,Kern,All}} ->
%% 		    ets:insert(fonts, {{info, Index}, Font}),
%% 		    ets:insert(fonts, {{allData, Font}, All}),
%% 		    lists:foreach(fun({Char,W}) ->
%% 				    ets:insert(fonts,
%% 					       {{width,Index,Char}, W})
%% 			    end, Widths),
%% 		    lists:foreach(fun({KP,W}) ->
%% 				    ets:insert(fonts,
%% 					       {{kern,Index,KP}, W})
%% 			    end, Kern),
%% 		    true;
%% 		{error, Why} ->
%% 		    exit({cannot_load_font, Why})
%% 	    end
%%     end.

info(Index) ->
    case ets:lookup(fonts, {info, Index}) of
	[{_,I}] ->
	    I;
	[] ->
	    exit({font_server_info,Index})
    end.

data(Fontname) ->
    case ets:lookup(fonts, {allData, Fontname}) of
	[{_,I}] ->
	    {ok, I};
	[] ->
	    error
    end.

char_width(N, Char) ->
    case ets:lookup(fonts, {width,N,Char}) of
	[{_,W}] ->
	    W;
	[] ->
	    io:format("Cannot figure out width of:~p ~p~n",[N, Char]),
	    io:format("Possible \n in code etc~n"),
	    1000
    end.

kern(N, KP) ->
    case ets:lookup(fonts, {kern,N,KP}) of
	[{_,W}] ->
	    W;
	[] ->
	    0
    end.









