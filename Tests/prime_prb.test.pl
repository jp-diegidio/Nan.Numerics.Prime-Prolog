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

/*	A simple prime number library :: probabilistic

@author		Julio P. Di Egidio
@version	1.2.3-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

:- use_module(library(plunit)).

:- ensure_loaded(module_inc).
:- module_inc('nan_numerics_prime_prb.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_prb:test_').

test(test__1,
[	fail
]) :-
	prime_prb:test_(1, _).

test(test__2,
[	true(Cert == true)
]) :-
	prime_prb:test_(2, Cert).

test(test__3,
[	true(Cert == true)
]) :-
	prime_prb:test_(3, Cert).

test(test__4,
[	fail
]) :-
	prime_prb:test_(4, _).

% Woodall primes := { i*2^i-1 | i in {2,3,6,30,75,81,...} }
% http://www.prothsearch.net/woodall.html
t__woodall(I, N) :- N is I * (1 << I) - 1.

test(test__w_75,
[	true(Cert == true)
]) :-
	t__woodall(75, N),
	prime_prb:test_(N, Cert).

test(test__w_81,
[	true(Cert == false)
]) :-
	t__woodall(81, N),
	prime_prb:test_(N, Cert).

test(test__w_81_2,
[	fail
]) :-
	t__woodall(81, N),
	N2 is N + 2,
	prime_prb:test_(N2, _).

:- end_tests('prime_prb:test_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
