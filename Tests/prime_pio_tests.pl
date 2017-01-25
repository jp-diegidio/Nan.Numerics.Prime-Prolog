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

:- module(prime_pio_test, []).

/** <module> A simple prime number library :: Pure I/O tests

Tests for module =prime_pio=.

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

:- use_module(library(plunit)).

:- use_module(loader).
:- loader:load_module('nan_numerics_prime_pio.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t__parse(In) :-
	context_module(Mod),
	atom_codes(In, Codes),
	phrase(prime_pio:parse_(Mod:t__gadd), Codes, _).	% NOTE: phrase/3!

t__gadd(_, P) :- P < 5.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_pio:parse_').

test(parse__0,
[	true
]) :-
	t__parse('').

test(parse__0_b,
[	true
]) :-
	t__parse(' ').

test(parse__1,
[	true
]) :-
	t__parse('2').

test(parse__1_b,
[	true
]) :-
	t__parse(' 2 ').

test(parse__2,
[	true
]) :-
	t__parse('2,3').

test(parse__2_b,
[	true
]) :-
	t__parse(' 2 , 3 ').

test(parse__max,
[	true
]) :-
	t__parse('2,3,5,7').

test(parse__1_f_a,
[	error(syntax_error(invalid_format), end_of_file - 1)
]) :-
	t__parse('a').

test(parse__1_f_c,
[	error(syntax_error(invalid_format), end_of_file - 1)
]) :-
	t__parse(',').

test(parse__2_f_a,
[	error(syntax_error(invalid_format), end_of_file - 1)
]) :-
	t__parse('2a').

test(parse__2_f_c,
[	error(syntax_error(invalid_format), end_of_file - 0)
]) :-
	t__parse('2,').

test(parse__3_f_a,
[	error(syntax_error(invalid_format), end_of_file - 1)
]) :-
	t__parse('2,a').

test(parse__3_f_c,
[	error(syntax_error(invalid_format), end_of_file - 1)
]) :-
	t__parse('2,,').

test(parse__3_f_a1,
[	error(syntax_error(invalid_format), end_of_file - 5)
]) :-
	t__parse('a,2,3').

test(parse__3_f_a2,
[	error(syntax_error(invalid_format), end_of_file - 3)
]) :-
	t__parse('2,a,3').

test(parse__3_f_a3,
[	error(syntax_error(invalid_format), end_of_file - 1)
]) :-
	t__parse('2,3,a').

test(parse__3_f_c1,
[	error(syntax_error(invalid_format), end_of_file - 4)
]) :-
	t__parse(',2,3').

test(parse__3_f_c2,
[	error(syntax_error(invalid_format), end_of_file - 2)
]) :-
	t__parse('2,,3').

test(parse__3_f_c3,
[	error(syntax_error(invalid_format), end_of_file - 0)
]) :-
	t__parse('2,3,').

:- end_tests('prime_pio:parse_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
