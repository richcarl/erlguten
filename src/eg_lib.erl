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

%% shared internal functions

-module(eg_lib).

-export([find_files/3, find_files/5,
         priv_dir/0, priv_src_dir/0]).

-include_lib("kernel/include/file.hrl").


%% Examples:
%%   find_files(".", "*.erl", false)
%%     finds all files in the current directory.
%%     Recursive scan of sub-directories is also allowed.
%%
%%   find_files(Dir, RegExp, Recursive, Fun/2, Acc0)
%%      applies Fun(File, Acc) -> Acc. to each file

find_files(Dir, Re, Flag) -> 
    Re1 = sh_to_awk(Re),
    lists:reverse(find_files(Dir, Re1, Flag,
                             fun(File, Acc) ->[File|Acc] end, [])).

find_files(Dir, Reg, Recursive, Fun, Acc) ->
    case file:list_dir(Dir) of
	{ok, Files} ->
            %% sort files of directory to make result stable; otherwise it
            %% depends on the file system and can vary between machines
            find_files(lists:sort(Files), Dir, Reg, Recursive, Fun, Acc);
	{error, _}  -> Acc
    end.

find_files([File|T], Dir, Reg, Recursive, Fun, Acc0) ->
    FullName = Dir ++  [$/|File],
    case file_type(FullName) of
	regular ->
	    case re:run(FullName, Reg) of
		{match, _} ->
		    Acc = Fun(FullName, Acc0),
		    find_files(T, Dir, Reg, Recursive, Fun, Acc);
		nomatch ->
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

%% NOTE: This function is copied from the deprecated stdlib 'regexp' module
%% sh_to_awk(ShellRegExp)
%%  Convert a sh style regexp into a full AWK one. The main difficulty is
%%  getting character sets right as the conventions are different.

sh_to_awk(Sh) -> "^(" ++ sh_to_awk_1(Sh).	%Fix the beginning

sh_to_awk_1([$*|Sh]) ->				%This matches any string
    ".*" ++ sh_to_awk_1(Sh);
sh_to_awk_1([$?|Sh]) ->				%This matches any character
    [$.|sh_to_awk_1(Sh)];
sh_to_awk_1([$[,$^,$]|Sh]) ->			%This takes careful handling
    "\\^" ++ sh_to_awk_1(Sh);
sh_to_awk_1("[^" ++ Sh) -> [$[|sh_to_awk_2(Sh, true)];
sh_to_awk_1("[!" ++ Sh) -> "[^" ++ sh_to_awk_2(Sh, false);
sh_to_awk_1([$[|Sh]) -> [$[|sh_to_awk_2(Sh, false)];
sh_to_awk_1([C|Sh]) ->
    %% Unspecialise everything else which is not an escape character.
    case special_char(C) of
	true -> [$\\,C|sh_to_awk_1(Sh)];
	false -> [C|sh_to_awk_1(Sh)]
    end;
sh_to_awk_1([]) -> ")$".			%Fix the end

sh_to_awk_2([$]|Sh], UpArrow) -> [$]|sh_to_awk_3(Sh, UpArrow)];
sh_to_awk_2(Sh, UpArrow) -> sh_to_awk_3(Sh, UpArrow).

sh_to_awk_3([$]|Sh], true) -> "^]" ++ sh_to_awk_1(Sh);
sh_to_awk_3([$]|Sh], false) -> [$]|sh_to_awk_1(Sh)];
sh_to_awk_3([C|Sh], UpArrow) -> [C|sh_to_awk_3(Sh, UpArrow)];
sh_to_awk_3([], true) -> [$^|sh_to_awk_1([])];
sh_to_awk_3([], false) -> sh_to_awk_1([]).

%%  Test if a character is a special character.
special_char($|) -> true;
special_char($*) -> true;
special_char($+) -> true;
special_char($?) -> true;
special_char($() -> true;
special_char($)) -> true;
special_char($\\) -> true;
special_char($^) -> true;
special_char($$) -> true;
special_char($.) -> true;
special_char($[) -> true;
special_char($]) -> true;
special_char($") -> true;
special_char(_C) -> false.
