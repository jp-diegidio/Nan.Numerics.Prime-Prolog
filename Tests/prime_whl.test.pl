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

/** <test> A simple prime number library :: wheel

@author		Julio P. Di Egidio
@version	1.2.1-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

% (SWI-Prolog 7.3.24)

:- use_module(library(plunit)).

:- ensure_loaded(test_inc).
:- test_module('nan_numerics_prime_whl.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_whl:test_').

test(test__1,
[	fail
]) :-
	prime_whl:test_(1, _).

test(test__2,
[	true(Cert == true)
]) :-
	prime_whl:test_(2, Cert).

test(test__3,
[	true(Cert == true)
]) :-
	prime_whl:test_(3, Cert).

test(test__4,
[	fail
]) :-
	prime_whl:test_(4, _).

test(test__5,
[	true(Cert == true)
]) :-
	prime_whl:test_(5, Cert).

test(test__6,
[	fail
]) :-
	prime_whl:test_(6, _).

test(test__10,
[	fail
]) :-
	prime_whl:test_(10, _).

test(test__11,
[	true(Cert == true)
]) :-
	prime_whl:test_(11, Cert).

test(test__12,
[	fail
]) :-
	prime_whl:test_(12, _).

test(test__120,
[	fail
]) :-
	prime_whl:test_(120, _).

test(test__121,
[	true(Cert == false)
]) :-
	prime_whl:test_(121, Cert).

test(test__122,
[	fail
]) :-
	prime_whl:test_(122, _).

test(test__220,
[	fail
]) :-
	prime_whl:test_(220, _).

test(test__221,
[	true(Cert == false)
]) :-
	prime_whl:test_(221, Cert).

test(test__222,
[	fail
]) :-
	prime_whl:test_(222, _).

:- end_tests('prime_whl:test_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_whl:right_').

test(right__1,
[	true((W, Cert) == (2, true))
]) :-
	prime_whl:right_(1, W, Cert).

test(right__2,
[	true((W, Cert) == (2, true))
]) :-
	prime_whl:right_(2, W, Cert).

test(right__3,
[	true((W, Cert) == (3, true))
]) :-
	prime_whl:right_(3, W, Cert).

test(right__4,
[	true((W, Cert) == (5, true))
]) :-
	prime_whl:right_(4, W, Cert).

test(right__10,
[	true((W, Cert) == (11, true))
]) :-
	prime_whl:right_(10, W, Cert).

test(right__11,
[	true((W, Cert) == (11, true))
]) :-
	prime_whl:right_(11, W, Cert).

test(right__12,
[	true((W, Cert) == (13, true))
]) :-
	prime_whl:right_(12, W, Cert).

test(right__112,
[	true((W, Cert) == (113, true))
]) :-
	prime_whl:right_(112, W, Cert).

test(right__113,
[	true((W, Cert) == (113, true))
]) :-
	prime_whl:right_(113, W, Cert).

test(right__114,
[	true((W, Cert) == (121, false))
]) :-
	prime_whl:right_(114, W, Cert).

test(right__120,
[	true((W, Cert) == (121, false))
]) :-
	prime_whl:right_(120, W, Cert).

test(right__121,
[	true((W, Cert) == (121, false))
]) :-
	prime_whl:right_(121, W, Cert).

test(right__122,
[	true((W, Cert) == (127, false))
]) :-
	prime_whl:right_(122, W, Cert).

test(right__220,
[	true((W, Cert) == (221, false))
]) :-
	prime_whl:right_(220, W, Cert).

test(right__221,
[	true((W, Cert) == (221, false))
]) :-
	prime_whl:right_(221, W, Cert).

test(right__222,
[	true((W, Cert) == (223, false))
]) :-
	prime_whl:right_(222, W, Cert).

:- end_tests('prime_whl:right_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_whl:left_').

test(left__1,
[	fail
]) :-
	prime_whl:left_(1, _, _).

test(left__2,
[	true((W, Cert) == (2, true))
]) :-
	prime_whl:left_(2, W, Cert).

test(left__3,
[	true((W, Cert) == (3, true))
]) :-
	prime_whl:left_(3, W, Cert).

test(left__4,
[	true((W, Cert) == (3, true))
]) :-
	prime_whl:left_(4, W, Cert).

test(left__5,
[	true((W, Cert) == (5, true))
]) :-
	prime_whl:left_(5, W, Cert).

test(left__6,
[	true((W, Cert) == (5, true))
]) :-
	prime_whl:left_(6, W, Cert).

test(left__10,
[	true((W, Cert) == (7, true))
]) :-
	prime_whl:left_(10, W, Cert).

test(left__11,
[	true((W, Cert) == (11, true))
]) :-
	prime_whl:left_(11, W, Cert).

test(left__12,
[	true((W, Cert) == (11, true))
]) :-
	prime_whl:left_(12, W, Cert).

test(left__120,
[	true((W, Cert) == (113, true))
]) :-
	prime_whl:left_(120, W, Cert).

test(left__121,
[	true((W, Cert) == (121, false))
]) :-
	prime_whl:left_(121, W, Cert).

test(left__122,
[	true((W, Cert) == (121, false))
]) :-
	prime_whl:left_(122, W, Cert).

test(left__220,
[	true((W, Cert) == (211, false))
]) :-
	prime_whl:left_(220, W, Cert).

test(left__221,
[	true((W, Cert) == (221, false))
]) :-
	prime_whl:left_(221, W, Cert).

test(left__222,
[	true((W, Cert) == (221, false))
]) :-
	prime_whl:left_(222, W, Cert).

:- end_tests('prime_whl:left_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
