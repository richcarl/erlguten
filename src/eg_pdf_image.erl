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
%% Author:   Mikael Karlsson <mikael.karlsson@creado.com>
%% Purpose: Import images to PDF
%%==========================================================================

-module(eg_pdf_image).

%% Purpose: Import images from file, currently jpg only 
%%          Pack into XObjects
%% 

-export([mk_images/4, get_head_info/1, process_header/1]).

-include("eg.hrl").

%% ============================================================================


mk_images([], I, Is, Os) -> 
    A = {{obj,I,0},{dict,lists:map(fun({Alias, ImageIndex}) ->
					   {Alias, {ptr,ImageIndex,0}}
				   end, 
				   lists:reverse(Is))}},
    {I+1, {ptr,I,0}, lists:reverse([A|Os])};
mk_images([{ImageURI, #image{alias=Alias}=Im}|T], I, Fs, E) ->
    O = mk_image(I, ImageURI, Im),
    mk_images(T, I+1, [{Alias,I}|Fs], [O|E]).

mk_image(I, File, #image{alias=_Alias, width=W, height=H}) ->
    Image = read_image(File),
    case process_header(Image) of
	{jpeg_head,{Width, Height, Ncomponents, Data_precision}} ->
	    Extras = [{"Filter", {name,"DCTDecode"}},
		      {"ColorSpace",{name, colorspace(Ncomponents)}},
		      {"BitsPerComponent", Data_precision}];
	_ ->
	    {Width, Height} = {W,H},
	    Extras = []
    end,
    {{obj,I,0}, 
     {stream,	  
      {dict,[{"Type",{name,"XObject"}},
	     {"Subtype",{name,"Image"}},
	     {"Width",Width},
	     {"Height", Height}| Extras ]},
      Image}
    }.


colorspace(0)-> "DeviceGray";
colorspace(1)-> "DeviceGray";
colorspace(2)-> "DeviceGray";
colorspace(3)-> "DeviceRGB";
colorspace(4)-> "DeviceCMYK".

read_image(File) ->
    {ok, Image} = file:read_file(File),
    Image.

get_head_info(File) ->
    process_header(read_image(File)).

%% JPEG support
-define(SOF0,  16#C0).     %% Start Of Frame
-define(SOF1,  16#C1).     %% Digit indicates compression type
-define(SOF2,  16#C2).     %% Only SOF0-SOF2 are now in common use
-define(SOF3,  16#C3).
-define(SOF5,  16#C5).
-define(SOF6,  16#C6).
-define(SOF7,  16#C7).
-define(SOF9,  16#C9).
-define(SOF10, 16#CA).
-define(SOF11, 16#CB).
-define(SOF13, 16#CD).
-define(SOF14, 16#CE).
-define(SOF15, 16#CF).
-define(SOI,   16#D8).     %% Start Of Image (beginning of file) 
-define(EOI,   16#D9).     %% End Of Image 
-define(SOS,   16#DA).     %% Start Of Scan (begin of compressed data) 
%% -define(APP0,  16#E0).  %% Application-specific markers, don't bother
%% -define(APP12, 16#EC).     
%% -define(COM,   16#FE).  %% Comment 


%% JPEG file
process_header( << 16#FF:8, ?SOI:8, Rest/binary >> )->
    process_jpeg( Rest );
process_header(_Any) ->
    image_format_not_yet_implemented_or_unknown.


%% JPEG file header processing

%% Skip any leading 16#FF
process_jpeg( << 16#FF:8,Rest/binary >> ) -> process_jpeg(Rest);

%% Codes 0xC4, 0xC8, 0xCC shall not be treated as SOFn.
process_jpeg( << ?SOF0:8,Rest/binary >> ) -> jpeg_sof( Rest);
process_jpeg( << ?SOF1:8,Rest/binary >> ) -> jpeg_sof( Rest);
process_jpeg( << ?SOF2:8,Rest/binary >> ) -> jpeg_sof( Rest);
process_jpeg( << ?SOF3:8,Rest/binary >> ) -> jpeg_sof( Rest);
process_jpeg( << ?SOF5:8,Rest/binary >> ) -> jpeg_sof( Rest);
process_jpeg( << ?SOF6:8,Rest/binary >> ) -> jpeg_sof( Rest);
process_jpeg( << ?SOF7:8,Rest/binary >> ) -> jpeg_sof( Rest);
process_jpeg( << ?SOF9:8,Rest/binary >> ) -> jpeg_sof( Rest);
process_jpeg( << ?SOF10:8,Rest/binary >> ) -> jpeg_sof(Rest);
process_jpeg( << ?SOF11:8,Rest/binary >> ) -> jpeg_sof(Rest);
process_jpeg( << ?SOF13:8,Rest/binary >> ) -> jpeg_sof(Rest);
process_jpeg( << ?SOF14:8,Rest/binary >> ) -> jpeg_sof(Rest);
process_jpeg( << ?SOF15:8,Rest/binary >> ) -> jpeg_sof(Rest);
process_jpeg( << ?SOS:8,_Rest/binary >> ) ->[];
process_jpeg( << ?EOI:8,_Rest/binary >> ) -> jeoi; %% Tables only
process_jpeg( << _Any:8, Rest/binary >> )->process_jpeg(skip_marker(Rest));
process_jpeg( << >> ) -> [].

jpeg_sof ( Rest )->
    << Length:16, Data_precision:8, Height:16, Width:16, 
     Ncomponents:8, Rest2/binary >> = Rest, Complen = Ncomponents * 3,
    Length = Complen + 8, %% This is a guard that will cause an error exception
    << _SkipComponents:Complen/binary, _Rest3/binary >> = Rest2,
    {jpeg_head,{Width, Height, Ncomponents, Data_precision}}.
%%     process_jpeg(Rest3)].

skip_marker(Image)->
    << Length:16, Rest/binary >> = Image, 
    AdjLen = Length-2,
    << _Skip:AdjLen/binary, Rest2/binary >> = Rest,
    Rest2.
