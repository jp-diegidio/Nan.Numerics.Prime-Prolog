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

:- module(primes_tests, []).

/*	A Simple Prime Number Library :: Tests

Part of *|Nan.Numerics.Primes|* (nan/numerics/primes.pl)

Tests for module =primes= (nan/numerics/primes.pl).

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016-2017 Julio P. Di Egidio
@license	GNU GPLv3
*/

:- initialization
	use_module(loader),
	module_path('primes.pl', Path),
	use_module(Path).

:- use_module(library(plunit)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_test).

t__1e3(
	[	2, 3, 5, 7, 11, 13, 17, 19, 23, 29,
		31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
		73, 79, 83, 89, 97, 101, 103, 107, 109, 113,
		127, 131, 137, 139, 149, 151, 157, 163, 167, 173,
		179, 181, 191, 193, 197, 199, 211, 223, 227, 229,
		233, 239, 241, 251, 257, 263, 269, 271, 277, 281,
		283, 293, 307, 311, 313, 317, 331, 337, 347, 349,
		353, 359, 367, 373, 379, 383, 389, 397, 401, 409,
		419, 421, 431, 433, 439, 443, 449, 457, 461, 463,
		467, 479, 487, 491, 499, 503, 509, 521, 523, 541,
		547, 557, 563, 569, 571, 577, 587, 593, 599, 601,
		607, 613, 617, 619, 631, 641, 643, 647, 653, 659,
		661, 673, 677, 683, 691, 701, 709, 719, 727, 733,
		739, 743, 751, 757, 761, 769, 773, 787, 797, 809,
		811, 821, 823, 827, 829, 839, 853, 857, 859, 863,
		877, 881, 883, 887, 907, 911, 919, 929, 937, 941,
		947, 953, 967, 971, 977, 983, 991, 997
	]).

t__t(N) :-
	t__1e3(Ps),
	member(N, Ps).

t__f(N) :-
	t__1e3(Ps),
	between(1, 1000, N),
	\+ memberchk(N, Ps).

test(prime_test__f,
[	true
]) :-
	forall(
		t__f(N),
		\+ prime_test(N)
	).

test(prime_test__t,
[	true
]) :-
	forall(
		t__t(N),
		prime_test(N)
	).

:- end_tests(prime_test).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_right).

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

test(prime_right__t,
[	forall(t__c(N, P0)),
	true(P == P0)
]) :-
	prime_right(N, P).

:- end_tests(prime_right).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_left).

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

test(prime_left__f,
[	fail
]) :-
	prime_left(1, _).

test(prime_left__t,
[	forall(t__c(N, P0)),
	true(P == P0)
]) :-
	prime_left(N, P).

:- end_tests(prime_left).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_next).

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

test(prime_next__t,
[	forall(t__c(N, P0)),
	true(P == P0)
]) :-
	prime_next(N, P).

:- end_tests(prime_next).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_prev).

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

test(prime_prev__f,
[	forall((N = 1; N = 2)),
	fail
]) :-
	prime_prev(N, _).

test(prime_prev__t,
[	forall(t__c(N, P0)),
	true(P == P0)
]) :-
	prime_prev(N, P).

:- end_tests(prime_prev).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_gen).

t__c(   10,     2, []).
t__c(    3,     3, [3]).
t__c(    4,     5, [5]).
t__c(    1,    13, [2, 3, 5, 7, 11, 13]).
t__c(  109,   149, [109, 113, 127, 131, 137, 139, 149]).
t__c(  210,   231, [211, 223, 227, 229]).
t__c(12288, 12330, [12289, 12301, 12323, 12329]).

t__gen_2(Inf, Sup, P) :-
	prime_gen(Inf, P),
	(P > Sup -> !, fail; true).

test(prime_gen_2__t,
[	forall(t__c(Inf, Sup, Ps0)),
	all(P == Ps0)
]) :-
	t__gen_2(Inf, Sup, P).

test(prime_gen_3__t,
[	forall(t__c(Inf, Sup, Ps0)),
	all(P == Ps0)
]) :-
	prime_gen(Inf, Sup, P).

:- end_tests(prime_gen).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_gen_rev).

t__c(   10,     2, []).
t__c(    3,     3, [3]).
t__c(    4,     5, [5]).
t__c(    1,    13, [13, 11, 7, 5, 3, 2]).
t__c(  109,   149, [149, 139, 137, 131, 127, 113, 109]).
t__c(  210,   231, [229, 227, 223, 211]).
t__c(12288, 12330, [12329, 12323, 12301, 12289]).

t__gen_rev_2(Inf, Sup, P) :-
	prime_gen_rev(Sup, P),
	(P < Inf -> !, fail; true).

test(prime_gen_rev_2__t,
[	forall(t__c(Inf, Sup, Ps0)),
	all(P == Ps0)
]) :-
	t__gen_rev_2(Inf, Sup, P).

test(prime_gen_rev_3__t,
[	forall(t__c(Inf, Sup, Ps0)),
	all(P == Ps0)
]) :-
	prime_gen_rev(Inf, Sup, P).

:- end_tests(prime_gen_rev).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_div).

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

test(prime_div_2__p,
[	forall(t__p(N)),
	fail
]) :-
	prime_div(N, _).

test(prime_div_3__p,
[	forall(t__p(N)),
	fail
]) :-
	prime_div(N, 1, _).

test(prime_div_2__t,
[	forall(t__c_2(N, P0)),
	true(P == P0)
]) :-
	prime_div(N, P).

test(prime_div_3__t,
[	forall(t__c_3(N, Inf, P0)),
	true(P == P0)
]) :-
	prime_div(N, Inf, P).

test(prime_div_3__f,
[	forall(t__f_3(N, Inf)),
	fail
]) :-
	prime_div(N, Inf, _).

:- end_tests(prime_div).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_div_rev).

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

test(prime_div_rev_2__p,
[	forall(t__p(N)),
	fail
]) :-
	prime_div_rev(N, _).

test(prime_div_rev_3__p,
[	forall(t__p(N)),
	fail
]) :-
	prime_div_rev(N, N, _).

test(prime_div_rev_2__t,
[	forall(t__c_2(N, P0)),
	true(P == P0)
]) :-
	prime_div_rev(N, P).

test(prime_div_rev_3__t,
[	forall(t__c_3(N, Sup, P0)),
	true(P == P0)
]) :-
	prime_div_rev(N, Sup, P).

test(prime_div_rev_3__f,
[	forall(t__f_3(N, Sup)),
	fail
]) :-
	prime_div_rev(N, Sup, _).

:- end_tests(prime_div_rev).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_fact).

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
t__c(52821404422431584600618278760865721, [1103^2, 2711^2, 3137^2, 4523^2, 5417^2]).
t__c(12345602200999012330678801133, [6197^1,300667^1,6625902158916276667^1]).

test(prime_fact__t,
[	forall(t__c(N, Fs0)),
	true(Fs == Fs0)
]) :-
	prime_fact(N, Fs).

:- end_tests(prime_fact).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(primes_state).

test(prime_prb_acc__t,
[	true(Acc == 80)
]) :-
	prime_prb_acc(Acc).

test(prime_prb_det_max__t,
[	true(Max == 3317044064679887385961980)
]) :-
	prime_prb_det_max(Max).

test(prime_whl_lev__t,
[	true(Lev == 5)
]) :-
	prime_whl_lev(Lev).

test(prime_whl_det_max__t,
[	true(Max == 168)
]) :-
	prime_whl_det_max(Max).

:- end_tests(primes_state).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
