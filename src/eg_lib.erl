%%==========================================================================
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
%%==========================================================================

-module(eg_lib).

-export([showGrid/2, moveAndShow/4, find_files/3, find_files/5,
         priv_dir/0, priv_src_dir/0]).

-include_lib("kernel/include/file.hrl").


%% showGrid(PDF, a4 | usLetter) 
%%   adds a grid to the current page page

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

sizeOfPaper(a4) ->
    {595, 842};
sizeOfPaper(usLetter) ->
    {612, 792}.

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

%% downwards iterator

diter(X, Inc, Stop, F) when X < Stop ->
    true;
diter(X, Inc, Stop, F) ->
    F(X), diter(X-Inc,Inc,Stop,F).
    

%%==========================================================================

%% Examples:
%%   find_files(".", "*.erl", false)
%%     finds all files in the current directory.
%%     Recursive scan of sub-directories is also allowed.
%%
%%   find_files(Dir, RegExp, Recursive, Fun/2, Acc0)
%%      applies Fun(File, Acc) -> Acc. to each file

find_files(Dir, Re, Flag) -> 
    Re1 = regexp:sh_to_awk(Re),
    lists:reverse(find_files(Dir, Re1, Flag,
                             fun(File, Acc) ->[File|Acc] end, [])).

find_files(Dir, Reg, Recursive, Fun, Acc) ->
    case file:list_dir(Dir) of
	{ok, Files} -> find_files(Files, Dir, Reg, Recursive, Fun, Acc);
	{error, _}  -> Acc
    end.

find_files([File|T], Dir, Reg, Recursive, Fun, Acc0) ->
    FullName = Dir ++  [$/|File],
    case file_type(FullName) of
	regular ->
	    case regexp:match(FullName, Reg) of
		{match, _, _}  -> 
		    Acc = Fun(FullName, Acc0),
		    find_files(T, Dir, Reg, Recursive, Fun, Acc);
		_ ->
		    find_files(T, Dir, Reg, Recursive, Fun, Acc0)
	    end;
	directory -> 
	    case Recursive of
		true ->
		    Acc1 = find_files(FullName, Reg, Recursive, Fun, Acc0),
		    find_files(T, Dir, Reg, Recursive, Fun, Acc1);
		false ->
		    find_files(T, Dir, Reg, Recursive, Fun, Acc0)
	    end;
	error -> 
	    find_files(T, Dir, Reg, Recursive, Fun, Acc0)
    end;
find_files([], _, _, _, _, A) ->
    A.

file_type(File) ->
    case file:read_file_info(File) of
	{ok, Facts} ->
	    case Facts#file_info.type of
		regular   -> regular;
		directory -> directory;
		_         -> error
	    end;
	_ ->
	    error
    end.

priv_src_dir() ->
    filename:join(priv_dir(), "src").

priv_dir() ->
    case code:priv_dir(erlguten) of
	{error, _} ->
	    filename:join(filename:dirname(code:which(?MODULE)),
                          "../priv");
	N ->
	    N
    end.
