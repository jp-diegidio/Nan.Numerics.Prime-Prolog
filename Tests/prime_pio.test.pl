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

/** <test> A simple prime number library :: pure I/O

@author		Julio P. Di Egidio
@version	1.2.1-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

% (SWI-Prolog 7.3.24)

:- use_module(library(plunit)).

:- ensure_loaded(test_inc).
:- test_module('nan_numerics_prime_pio.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_pio:parse_').

test(parse__p0_f_e,
[	error(syntax_error(invalid_start))
]) :-
	t__parse('').

test(parse__p0_f_a,
[	error(syntax_error(invalid_start))
]) :-
	t__parse('a').

test(parse__p0_f_1,
[	error(syntax_error(invalid_start))
]) :-
	t__parse('1').

test(parse__p0_f_2,
[	error(syntax_error(invalid_format))
]) :-
	t__parse('2').

test(parse__p0_f_2a,
[	error(syntax_error(invalid_format))
]) :-
	t__parse('2-').

test(parse__p0_f_2c,
[	error(syntax_error(invalid_value))
]) :-
	t__parse('2,').

test(parse__p_f_a,
[	error(syntax_error(invalid_value))
]) :-
	t__parse('2,a').

test(parse__p_f_2,
[	error(syntax_error(invalid_format))
]) :-
	t__parse('2,3').

test(parse__p_f_2a,
[	error(syntax_error(invalid_format))
]) :-
	t__parse('2,3-').

test(parse__p_f_2c,
[	error(syntax_error(invalid_value))
]) :-
	t__parse('2,3,').

test(parse__p_f_2b_1,
[	error(syntax_error(invalid_start))
]) :-
	t__parse(' 2,3.').

test(parse__p_f_2b_2,
[	error(syntax_error(invalid_format))
]) :-
	t__parse('2 ,3.').

test(parse__p_f_2b_3,
[	error(syntax_error(invalid_value))
]) :-
	t__parse('2, 3.').

test(parse__p_f_2b_4,
[	error(syntax_error(invalid_format))
]) :-
	t__parse('2,3 .').

test(parse__p_f_2b_5,
[	error(syntax_error(invalid_format))
]) :-
	t__parse('2,3. ').

test(parse__p0,
[	true
]) :-
	t__parse('2.').

test(parse__p1,
[	true
]) :-
	t__parse('2,3.').

test(parse__p2,
[	true
]) :-
	t__parse('2,3,5.').

test(parse__p2_Max,
[	true
]) :-
	t__parse('2,3,5.', 3).

t__parse(A) :-
	context_module(Mod),
	atom_codes(A, Codes),
	phrase(prime_pio:parse_(Mod:t__gadd), Codes, _).

t__parse(A, Max) :-
	context_module(Mod),
	atom_codes(A, Codes),
	phrase(prime_pio:parse_(Mod:t__gadd(Max)), Codes, _).

t__gadd(_, _).

t__gadd(Max, _, P) :- P =< Max.

:- end_tests('prime_pio:parse_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
