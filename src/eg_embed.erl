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
%% Purpose: Font embedding code
%%==========================================================================

-module(eg_embed).

-export([embed/1, 
	 parse_pfb/1
	]).


%% The format of a PBF is specified in 5040.Download_Fonts.pdf
%% It is a sequence of chunks
%%   <<128, Type, N1, N2, N3, N4 >> <<Len bytes>>
%%   <<128, 3>>

%% Where
%%    Type = 1 -> ascii
%%    Type = 2 -> binary
%%    Len = N1 + N2*256 + N3*256*256 + N4*256*256*256,

% test() ->
%     embed("brush.pfb").

embed(F) ->
    P = parse_pfb(F),
    O = lists:map(fun({_,_,B}) -> B end, P),
    file:write_file(F ++ ".synth", O).

%% Parse_pfb -> [{Type,Len,Bin}]
%%  The lengths are the required lengths in the
%%  object descriptor ...

parse_pfb(F) ->
    {ok, Bin} = file:read_file(F),
    L = parse(Bin).

parse(<<128,3>>) ->
    [];
parse(B) ->
    {B1,B2} = split_binary(B, 6),
    [128,Type,N1,N2,N3,N4] = binary_to_list(B1),
    Len = N1 + N2*256 + N3*256*256 + N4*256*256*256,
    %% io:format("Chunk: ~p length=~p~n",[Type, Len]),
    case Len of
	0 -> [];
	_ ->
	    {B3,B4} = split_binary(B2, Len),
	    [{Type,Len,B3}|parse(B4)]
    end.




