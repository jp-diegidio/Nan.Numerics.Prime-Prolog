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

:- module(prime_prb_test, []).

/** <module> A simple prime number library :: Probabilistic tests

Tests for module =prime_prb=.

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

:- use_module(library(plunit)).
:- use_module(library(lists)).

:- use_module(loader).
:- loader:load_module('nan_numerics_prime_prb.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Woodall primes: http://www.prothsearch.net/woodall.html

t__ws([2, 3, 6, 30, 75, 81, 115, 123, 249, 362, 384, 462, 512, 751, 822]).

t__w(W, N) :- N is W * (1 << W) - 1.

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

test(test__w_75,
[	true(Cert == true)
]) :-
	t__w(75, N),
	prime_prb:test_(N, Cert).

test(test__w_75p2,
[	fail
]) :-
	t__w(75, N),
	N2 is N + 2,
	prime_prb:test_(N2, _).

test(test__w_81,
[	true(Cert == false)
]) :-
	t__w(81, N),
	prime_prb:test_(N, Cert).

test(test__w_81p2,
[	fail
]) :-
	t__w(81, N),
	N2 is N + 2,
	prime_prb:test_(N2, _).

test(test__w_all,
[	forall((t__ws(Ws), member(W, Ws))),
	true
]) :-
	t__w(W, N),
	prime_prb:test_(N, _).

:- end_tests('prime_prb:test_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
