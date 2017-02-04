%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*	Nan.Numerics.Primes
	Nan.Numerics.Primes/Prolog 1.3.0-beta
	A Simple Prime Number Library in Prolog
	Copyright 2016-2017 Julio P. Di Egidio
	<mailto:julio@diegidio.name>
	<http://julio.diegidio.name/Projects/Nan.Numerics.Primes/>
	
	This file is part of Nan.Numerics.Primes.
	
	Nan.Numerics.Primes is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	Nan.Numerics.Primes is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with Nan.Numerics.Primes.  If not, see <http://www.gnu.org/licenses/>.
*/

% (SWI-Prolog 7.3.25)

:- module(primes_logic_tests, []).

/** <module> A Simple Prime Number Library :: Logic tests

Part of *|Nan.Numerics.Primes|* (nan/numerics/primes.pl)

Tests for module =primes_logic= (nan/numerics/primes_logic.pl).

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016-2017 Julio P. Di Egidio
@license	GNU GPLv3
*/

:- initialization
	use_module(locator),
	file_path('primes_logic.pl', Path),
	use_module(Path).

:- use_module(library(plunit)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_lgc_test).

t__c(fail, 1).
t__c(true, 2).
t__c(true, 3).
t__c(fail, 4).
t__c(true, 5).
t__c(fail, 6).
t__c(fail, 10).
t__c(true, 11).
t__c(fail, 12).
t__c(fail, 112).
t__c(true, 113).
t__c(fail, 114).
t__c(fail, 120).
t__c(fail, 121).
t__c(fail, 122).
t__c(fail, 220).
t__c(fail, 221).
t__c(fail, 222).
t__c(true, 223).
t__c(fail, 12300).
t__c(true, 12301).
t__c(fail, 12345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789011111).

test(prime_lgc_test__f,
[	forall(t__c(fail, N)),
	fail
]) :-
	prime_lgc_test(N).

test(prime_lgc_test__t,
[	forall(t__c(true, N)),
	true
]) :-
	prime_lgc_test(N).

:- end_tests(prime_lgc_test).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_lgc_right).

t__c(1, 2).
t__c(2, 2).
t__c(3, 3).
t__c(4, 5).
t__c(5, 5).
t__c(6, 7).
t__c(10, 11).
t__c(11, 11).
t__c(12, 13).
t__c(112, 113).
t__c(113, 113).
t__c(114, 127).
t__c(211, 211).
t__c(212, 223).
t__c(12288, 12289).
t__c(12289, 12289).
t__c(12290, 12301).

t__c(123456789012345678901234567890123456789012345678839,
	123456789012345678901234567890123456789012345678839).
t__c(123456789012345678901234567890123456789012345679033,
	123456789012345678901234567890123456789012345679141).
t__c(123456789012345678901234567890123456789012345679141,
	123456789012345678901234567890123456789012345679141).

test(prime_lgc_right__t,
[	forall(t__c(N, P0)),
	true(P == P0)
]) :-
	prime_lgc_right(N, P).

:- end_tests(prime_lgc_right).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_lgc_left).

t__c(2, 2).
t__c(3, 3).
t__c(4, 3).
t__c(10, 7).
t__c(11, 11).
t__c(12, 11).
t__c(113, 113).
t__c(114, 113).
t__c(211, 211).
t__c(212, 211).
t__c(12289, 12289).
t__c(12290, 12289).
t__c(12291, 12289).

t__c(123456789012345678901234567890123456789012345678839,
	123456789012345678901234567890123456789012345678839).
t__c(123456789012345678901234567890123456789012345679033,
	123456789012345678901234567890123456789012345678839).
t__c(123456789012345678901234567890123456789012345679141,
	123456789012345678901234567890123456789012345679141).

test(prime_lgc_left__f,
[	fail
]) :-
	prime_lgc_left(1, _).

test(prime_lgc_left__t,
[	forall(t__c(N, P0)),
	true(P == P0)
]) :-
	prime_lgc_left(N, P).

:- end_tests(prime_lgc_left).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_lgc_next).

t__c(1, 2).
t__c(2, 3).
t__c(3, 5).
t__c(4, 5).
t__c(7, 11).
t__c(8, 11).
t__c(10, 11).
t__c(11, 13).
t__c(12, 13).
t__c(13, 17).
t__c(112, 113).
t__c(113, 127).
t__c(210, 211).
t__c(211, 223).
t__c(12287, 12289).
t__c(12288, 12289).
t__c(12289, 12301).

test(prime_lgc_next__t,
[	forall(t__c(N, P0)),
	true(P == P0)
]) :-
	prime_lgc_next(N, P).

:- end_tests(prime_lgc_next).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_lgc_next_p).

t__c(2, 3).
t__c(3, 5).
t__c(7, 11).
t__c(11, 13).
t__c(13, 17).
t__c(113, 127).
t__c(211, 223).
t__c(12289, 12301).

test(prime_lgc_next_p__t,
[	forall(t__c(P, P0)),
	true(P1 == P0)
]) :-
	prime_lgc_next_p(P, P1).

:- end_tests(prime_lgc_next_p).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_lgc_prev).

t__c(3, 2).
t__c(4, 3).
t__c(7, 5).
t__c(8, 7).
t__c(10, 7).
t__c(11, 7).
t__c(12, 11).
t__c(13, 11).
t__c(127, 113).
t__c(128, 127).
t__c(223, 211).
t__c(224, 223).
t__c(12301, 12289).
t__c(12302, 12301).

test(prime_lgc_prev__f,
[	forall((N = 1; N = 2)),
	fail
]) :-
	prime_lgc_prev(N, _).

test(prime_lgc_prev__t,
[	forall(t__c(N, P0)),
	true(P == P0)
]) :-
	prime_lgc_prev(N, P).

:- end_tests(prime_lgc_prev).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_lgc_prev_p).

t__c(3, 2).
t__c(7, 5).
t__c(11, 7).
t__c(13, 11).
t__c(127, 113).
t__c(223, 211).
t__c(12301, 12289).

test(prime_lgc_prev_p__f,
[	forall((N = 1; N = 2)),
	fail
]) :-
	prime_lgc_prev_p(N, _).

test(prime_lgc_prev_p__t,
[	forall(t__c(P, P0)),
	true(P1 == P0)
]) :-
	prime_lgc_prev_p(P, P1).

:- end_tests(prime_lgc_prev_p).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_lgc_gen).

t__c(   10,     2, []).
t__c(    3,     3, [3]).
t__c(    4,     5, [5]).
t__c(    1,    13, [2, 3, 5, 7, 11, 13]).
t__c(  109,   149, [109, 113, 127, 131, 137, 139, 149]).
t__c(  210,   231, [211, 223, 227, 229]).
t__c(12288, 12330, [12289, 12301, 12323, 12329]).

t__gen_2(Inf, Sup, P) :-
	prime_lgc_gen(Inf, P),
	(P > Sup -> !, fail; true).

test(prime_lgc_gen_2__t,
[	forall(t__c(Inf, Sup, Ps0)),
	all(P == Ps0)
]) :-
	t__gen_2(Inf, Sup, P).

test(prime_lgc_gen_3__t,
[	forall(t__c(Inf, Sup, Ps0)),
	all(P == Ps0)
]) :-
	prime_lgc_gen(Inf, Sup, P).

:- end_tests(prime_lgc_gen).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_lgc_gen_p).

t__c(   10,     2, []).
t__c(    3,     3, [3]).
t__c(    3,     5, [3, 5]).
t__c(    2,    13, [2, 3, 5, 7, 11, 13]).
t__c(  109,   150, [109, 113, 127, 131, 137, 139, 149]).
t__c(  211,   231, [211, 223, 227, 229]).
t__c(12289, 12330, [12289, 12301, 12323, 12329]).

t__gen_p_2(L, Sup, P) :-
	prime_lgc_gen_p(L, P),
	(P > Sup -> !, fail; true).

test(prime_lgc_gen_p_2__t,
[	forall(t__c(L, Sup, Ps0)),
	all(P == Ps0)
]) :-
	t__gen_p_2(L, Sup, P).

test(prime_lgc_gen_p_3__t,
[	forall(t__c(L, Sup, Ps0)),
	all(P == Ps0)
]) :-
	prime_lgc_gen_p(L, Sup, P).

:- end_tests(prime_lgc_gen_p).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_lgc_gen_rev).

t__c(   10,     2, []).
t__c(    3,     3, [3]).
t__c(    4,     5, [5]).
t__c(    1,    13, [13, 11, 7, 5, 3, 2]).
t__c(  109,   149, [149, 139, 137, 131, 127, 113, 109]).
t__c(  210,   231, [229, 227, 223, 211]).
t__c(12288, 12330, [12329, 12323, 12301, 12289]).

t__gen_rev_2(Inf, Sup, P) :-
	prime_lgc_gen_rev(Sup, P),
	(P < Inf -> !, fail; true).

test(prime_lgc_gen_rev_2__t,
[	forall(t__c(Inf, Sup, Ps0)),
	all(P == Ps0)
]) :-
	t__gen_rev_2(Inf, Sup, P).

test(prime_lgc_gen_rev_3__t,
[	forall(t__c(Inf, Sup, Ps0)),
	all(P == Ps0)
]) :-
	prime_lgc_gen_rev(Inf, Sup, P).

:- end_tests(prime_lgc_gen_rev).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_lgc_gen_rev_p).

t__c(   10,     2, []).
t__c(    3,     3, [3]).
t__c(    4,     5, [5]).
t__c(    1,    13, [13, 11, 7, 5, 3, 2]).
t__c(  109,   149, [149, 139, 137, 131, 127, 113, 109]).
t__c(  210,   229, [229, 227, 223, 211]).
t__c(12288, 12329, [12329, 12323, 12301, 12289]).

t__gen_rev_p_2(Inf, H, P) :-
	prime_lgc_gen_rev_p(H, P),
	(P < Inf -> !, fail; true).

test(prime_lgc_gen_rev_p_2__t,
[	forall(t__c(Inf, H, Ps0)),
	all(P == Ps0)
]) :-
	t__gen_rev_p_2(Inf, H, P).

test(prime_lgc_gen_rev_p_3__t,
[	forall(t__c(Inf, H, Ps0)),
	all(P == Ps0)
]) :-
	prime_lgc_gen_rev_p(Inf, H, P).

:- end_tests(prime_lgc_gen_rev_p).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_lgc_div).

t__p(N) :- (N = 1; N = 2; N = 3; N = 211; N = 12301).

t__c_2(4, 2).
t__c_2(210, 2).
t__c_2(213, 3).
t__c_2(221, 13).
t__c_2(12300, 2).
t__c_2(12345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789011111, 41).

t__c_3(10, 4, 5).
t__c_3(210, 6, 7).
t__c_3(213, 4, 71).
t__c_3(221, 15, 17).
t__c_3(12300, 1, 2).

t__f_3(210, 10).
t__f_3(213, 72).
t__f_3(221, 18).

test(prime_lgc_div_2__p,
[	forall(t__p(N)),
	fail
]) :-
	prime_lgc_div(N, _).

test(prime_lgc_div_3__p,
[	forall(t__p(N)),
	fail
]) :-
	prime_lgc_div(N, 1, _).

test(prime_lgc_div_2__t,
[	forall(t__c_2(N, P0)),
	true(P == P0)
]) :-
	prime_lgc_div(N, P).

test(prime_lgc_div_3__t,
[	forall(t__c_3(N, Inf, P0)),
	true(P == P0)
]) :-
	prime_lgc_div(N, Inf, P).

test(prime_lgc_div_3__f,
[	forall(t__f_3(N, Inf)),
	fail
]) :-
	prime_lgc_div(N, Inf, _).

:- end_tests(prime_lgc_div).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_lgc_div_p).

t__p(N) :- (N = 1; N = 2; N = 3; N = 211; N = 12301).

t__c_3(10, 3, 5).
t__c_3(210, 5, 5).
t__c_3(213, 5, 71).
t__c_3(221, 11, 13).
t__c_3(12300, 11, 41).

t__f_3(210, 11).
t__f_3(213, 73).
t__f_3(221, 19).

test(prime_lgc_div_p_3__p,
[	forall(t__p(N)),
	fail
]) :-
	prime_lgc_div_p(N, 2, _).

test(prime_lgc_div_p_3__t,
[	forall(t__c_3(N, L, P0)),
	true(P == P0)
]) :-
	prime_lgc_div_p(N, L, P).

test(prime_lgc_div_p_3__f,
[	forall(t__f_3(N, L)),
	fail
]) :-
	prime_lgc_div_p(N, L, _).

:- end_tests(prime_lgc_div_p).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_lgc_div_rev).

t__p(N) :- (N = 1; N = 2; N = 3; N = 211; N = 12301).

t__c_2(4, 2).
t__c_2(210, 7).
t__c_2(213, 71).
t__c_2(221, 17).
t__c_2(12300, 41).

t__c_3(10, 4, 2).
t__c_3(210, 6, 5).
t__c_3(213, 4, 3).
t__c_3(221, 15, 13).
t__c_3(12300, 7, 5).

t__f_3(210, 1).
t__f_3(213, 2).
t__f_3(221, 12).

test(prime_lgc_div_rev_2__p,
[	forall(t__p(N)),
	fail
]) :-
	prime_lgc_div_rev(N, _).

test(prime_lgc_div_rev_3__p,
[	forall(t__p(N)),
	fail
]) :-
	prime_lgc_div_rev(N, N, _).

test(prime_lgc_div_rev_2__t,
[	forall(t__c_2(N, P0)),
	true(P == P0)
]) :-
	prime_lgc_div_rev(N, P).

test(prime_lgc_div_rev_3__t,
[	forall(t__c_3(N, Sup, P0)),
	true(P == P0)
]) :-
	prime_lgc_div_rev(N, Sup, P).

test(prime_lgc_div_rev_3__f,
[	forall(t__f_3(N, Sup)),
	fail
]) :-
	prime_lgc_div_rev(N, Sup, _).

:- end_tests(prime_lgc_div_rev).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_lgc_div_rev_p).

t__p(N) :- (N = 1; N = 2; N = 3; N = 211; N = 12301).

t__c_3(10, 7, 5).
t__c_3(210, 5, 5).
t__c_3(213, 5, 3).
t__c_3(221, 19, 17).
t__c_3(12300, 11, 5).

t__f_3(210, 1).
t__f_3(213, 2).
t__f_3(221, 11).

test(prime_lgc_div_rev_p_3__p,
[	forall(t__p(N)),
	fail
]) :-
	prime_lgc_div_rev_p(N, N, _).

test(prime_lgc_div_rev_p_3__t,
[	forall(t__c_3(N, H, P0)),
	true(P == P0)
]) :-
	prime_lgc_div_rev_p(N, H, P).

test(prime_lgc_div_rev_p_3__f,
[	forall(t__f_3(N, H)),
	fail
]) :-
	prime_lgc_div_rev_p(N, H, _).

:- end_tests(prime_lgc_div_rev_p).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_lgc_fact).

t__c(1, [1^1]).
t__c(2, [2^1]).
t__c(3, [3^1]).
t__c(4, [2^2]).
t__c(210, [2^1, 3^1, 5^1, 7^1]).
t__c(211, [211^1]).
t__c(213, [3^1, 71^1]).
t__c(221, [13^1, 17^1]).
t__c(12300, [2^2, 3^1, 5^2, 41^1]).
t__c(12301, [12301^1]).
t__c(123456789012345678910, [2^1, 5^1, 12345678901234567891^1]).
t__c(30, [2^1, 3^1, 5^1]).
t__c(45000, [2^3, 3^2, 5^4]).

test(prime_lgc_fact__t,
[	forall(t__c(N, Fs0)),
	true(Fs == Fs0)
]) :-
	prime_lgc_fact(N, Fs).

:- end_tests(prime_lgc_fact).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
