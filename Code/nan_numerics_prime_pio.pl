%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*	Nan.Numerics.Prime
	A simple prime number library
	Copyright 2016 Julio P. Di Egidio
	<mailto:julio@diegidio.name>
	<http://julio.diegidio.name/Projects/Nan.Numerics.Prime/>
	
	This file is part of Nan.Numerics.Prime.
	
	Nan.Numerics.Prime is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	Nan.Numerics.Prime is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with Nan.Numerics.Prime.  If not, see <http://www.gnu.org/licenses/>.
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (SWI-Prolog 7.3.25)

% TODO: Improve leveraging of library(pio)?

:- module(prime_pio, []).

:- public
	open_/3,		% +File:file, +Mode:oneof(read,write), :Goal:callable
	read_/2,		% +Stream:stream, :GAdd:callable
	write_/2.		% +Stream:stream, :GGen:callable

/** <module> A simple prime number library :: pure I/O

Module =prime_pio= provides low-level predicates to read/write from/to a
file or stream all consecutive prime numbers starting from =2= and up to a
certain limit that is determined by the caller.

The accepted file format is a comma-separated list of the consecutive
prime numbers starting from =2= and terminated by a period.

*NOTE*: Predicates in this module are not meant for public use.

@author		Julio P. Di Egidio
@version	1.2.5-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file__opts(read,
	[	lock(read),
		eof_action(error)
	|	BaseOpts
	]) :-
	file__opts(BaseOpts).
file__opts(write,
	[	lock(write),
		buffer(full)
	|	BaseOpts
	]) :-
	file__opts(BaseOpts).

stream__opts(read,
	[	eof_action(error)
	|	BaseOpts
	]) :-
	stream__opts(BaseOpts).
stream__opts(write,
	[	buffer(full),
		representation_errors(error)
	|	BaseOpts
	]) :-
	stream__opts(BaseOpts).

file__opts(
	[	bom(false),
		encoding(ascii),
		type(text)
	]).

stream__opts(
	[	encoding(ascii),
		type(text),
		record_position(true),	% NOTE: Must be true for pio:stream_to_lazy_list/2.
		buffer_size(1024)		% TODO: Review this. #####
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	open_(+File:file, +Mode:oneof(read,write), :Goal:callable) is det.
%
%	Opens File in Mode and calls back =|Goal(Stream)|= where _Stream_ is
%	the opened I/O stream.  Mode can be one of =read= or =write=.  File is
%	automatically closed after Goal is finished.
%
%	Encoding of file is =ascii=, type is =text=, buffering is =full=.
%
%	@error	Errors from system:open/4.

:- meta_predicate
	open_(+, +, 1).

open_(File, Mode, Goal) :-
	file__opts(Mode, Opts),
	setup_call_cleanup(
		open(File, Mode, Stream, [wait(false)| Opts]),
		call(Goal, Stream),
		close(Stream, [force(false)])
	).

%!	read_(+Stream:stream, :GAdd:callable) is det.
%
%	Reads consecutive prime numbers from Stream.  The file must not be
%	empty and numbers must start at =2=.  For every read number greater
%	than =2= calls =|GAdd(+P0:prime +P:prime)|=, where _P0_ is the
%	previously read number and _P_ is the current number.  Stops reading
%	either at end-of-stream or when the call to GAdd fails.
%
%	Encoding of stream is =ascii=, type is =text=, buffer size is =1024=.
%
%	*NOTE*: Does not check that the numbers read from the stream are
%	consecutive prime numbers starting from =2=.
%
%	@error	syntax_error(invalid_format)	Input format is invalid.
%	@error	syntax_error(invalid_start)		Input values must start at =2=.
%	@error	syntax_error(invalid_value)		Input values must be =posint=.
%	@error	Errors from system:read/2.
%	@tbd	Improve parse errors?

:- meta_predicate
	read_(+, 2).

read_(Stream, GAdd) :-
	set_stream_(Stream, read),
	stream_to_lazy_list(Stream, List),
	phrase(parse_(GAdd), List, _).

:- meta_predicate
	parse_(2, +, -),
	parse__p(2, +, +, -),
	parse__add(2, +, +, +, -),
	parse__sel(2, +, +, -).

parse_(GAdd) --> "2", !,
	parse__sel(GAdd, 2).
parse_(_) -->
	syntax_error(invalid_start).

parse__p(GAdd, P0) --> integer(P), { P > 0 }, !,
	parse__add(GAdd, P0, P).
parse__p(_, _) -->
	syntax_error(invalid_value).

parse__add(GAdd, P0, P) -->
	{ call(GAdd, P0, P) }, !,
	parse__sel(GAdd, P).
parse__add(_, _, _) --> [].

parse__sel(GAdd, P0) --> ",", !,
	parse__p(GAdd, P0).
parse__sel(_, _) --> ".", eos, !.
parse__sel(_, _) -->
	syntax_error(invalid_format).

%!	write_(+Stream:stream, :GGen:callable) is det.
%
%	Writes consecutive prime numbers to Stream.  Always writes the number
%	=2=, then writes all numbers generated by calling =|GGen(-P:prime)|=,
%	where _P_ shall be greater than =2=.  Stops writing when bactracking on
%	GGen terminates.
%
%	Encoding of stream is =ascii=, type is =text=, buffering is =full=,
%	buffer size is =1024=.
%
%	*NOTE*: Does not check that the numbers generated by GGen are
%	consecutive prime numbers starting from =3=.
%
%	@error	Errors from system:write/2.

:- meta_predicate
	write_(+, 1).

write_(Stream, GGen) :-
	set_stream_(Stream, write),
	write(Stream, 2),
	ignore(forall(
		call(GGen, P),
		(	write(Stream, ','),
			write(Stream, P)
		)
	)),
	write(Stream, '.').

set_stream_(Stream, Mode) :-
	stream__opts(Mode, Opts),
	set_stream__do(Stream, Opts).

set_stream__do(_, []) :- !.
set_stream__do(Stream, [Opt| Opts]) :-
	set_stream(Stream, Opt),
	set_stream__do(Stream, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
