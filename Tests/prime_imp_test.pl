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

:- module(prime_lgc_test, []).

/** <module> A simple prime number library :: Logic tests

Tests for module =prime_lgc=.

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

:- use_module(library(plunit)).
:- use_module(library(lists)).

:- use_module(loader).
:- loader:load_module('nan_numerics_prime_lgc.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t__mode(Mode) :-
	member(Mode, [direct, mem_read, mem_write, mem_sync]).

t__cnt_max(Mode, _, _, 0, 2) :-
	memberchk(Mode, [direct, mem_read]), !.
t__cnt_max(_, TC, TM, TC, TM).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:test_', [setup(prime_mem:clear_)]).

test(test__1,
[	forall(t__mode(Mode)),
	fail
]) :-
	prime_lgc:test_(Mode, 1).

test(test__2,
[	forall(t__mode(Mode)),
	true
]) :-
	prime_lgc:test_(Mode, 2).

test(test__3,
[	forall(t__mode(Mode)),
	true
]) :-
	prime_lgc:test_(Mode, 3).

test(test__4,
[	forall(t__mode(Mode)),
	fail
]) :-
	prime_lgc:test_(Mode, 4).

test(test__10,
[	forall(t__mode(Mode)),
	fail
]) :-
	prime_lgc:test_(Mode, 10).

test(test__11,
[	forall(t__mode(Mode)),
	true
]) :-
	prime_lgc:test_(Mode, 11).

test(test__12,
[	forall(t__mode(Mode)),
	fail
]) :-
	prime_lgc:test_(Mode, 12).

test(test__112,
[	forall(t__mode(Mode)),
	fail
]) :-
	prime_lgc:test_(Mode, 112).

test(test__113,
[	forall(t__mode(Mode)),
	true
]) :-
	prime_lgc:test_(Mode, 113).

test(test__114,
[	forall(t__mode(Mode)),
	fail
]) :-
	prime_lgc:test_(Mode, 114).

test(test__220,
[	forall(t__mode(Mode)),
	fail
]) :-
	prime_lgc:test_(Mode, 220).

test(test__221,
[	forall(t__mode(Mode)),
	fail
]) :-
	prime_lgc:test_(Mode, 221).

test(test__222,
[	forall(t__mode(Mode)),
	fail
]) :-
	prime_lgc:test_(Mode, 222).

test(test__12300,
[	forall(t__mode(Mode)),
	fail
]) :-
	prime_lgc:test_(Mode, 12300).

test(test__12301,
[	forall(t__mode(Mode)),
	true
]) :-
	prime_lgc:test_(Mode, 12301).

:- end_tests('prime_lgc:test_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:right_', [setup(prime_mem:clear_)]).

test(right__1,
[	forall(t__mode(Mode)),
	true(P == 2)
]) :-
	prime_lgc:right_(Mode, 1, P).

test(right__2,
[	forall(t__mode(Mode)),
	true(P == 2)
]) :-
	prime_lgc:right_(Mode, 2, P).

test(right__3,
[	forall(t__mode(Mode)),
	true(P == 3)
]) :-
	prime_lgc:right_(Mode, 3, P).

test(right__4,
[	forall(t__mode(Mode)),
	true(P == 5)
]) :-
	prime_lgc:right_(Mode, 4, P).

test(right__10,
[	forall(t__mode(Mode)),
	true(P == 11)
]) :-
	prime_lgc:right_(Mode, 10, P).

test(right__11,
[	forall(t__mode(Mode)),
	true(P == 11)
]) :-
	prime_lgc:right_(Mode, 11, P).

test(right__12,
[	forall(t__mode(Mode)),
	true(P == 13)
]) :-
	prime_lgc:right_(Mode, 12, P).

test(right__113,
[	forall(t__mode(Mode)),
	true(P == 113)
]) :-
	prime_lgc:right_(Mode, 113, P).

test(right__114,
[	forall(t__mode(Mode)),
	true(P == 127)
]) :-
	prime_lgc:right_(Mode, 114, P).

test(right__211,
[	forall(t__mode(Mode)),
	true(P == 211)
]) :-
	prime_lgc:right_(Mode, 211, P).

test(right__212,
[	forall(t__mode(Mode)),
	true(P == 223)
]) :-
	prime_lgc:right_(Mode, 212, P).

test(right__12288,
[	forall(t__mode(Mode)),
	true(P == 12289)
]) :-
	prime_lgc:right_(Mode, 12288, P).

test(right__12289,
[	forall(t__mode(Mode)),
	true(P == 12289)
]) :-
	prime_lgc:right_(Mode, 12289, P).

test(right__12290,
[	forall(t__mode(Mode)),
	true(P == 12301)
]) :-
	prime_lgc:right_(Mode, 12290, P).

:- end_tests('prime_lgc:right_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:left_', [setup(prime_mem:clear_)]).

test(left__1,
[	forall(t__mode(Mode)),
	fail
]) :-
	prime_lgc:left_(Mode, 1, _).

test(left__2,
[	forall(t__mode(Mode)),
	true(P == 2)
]) :-
	prime_lgc:left_(Mode, 2, P).

test(left__3,
[	forall(t__mode(Mode)),
	true(P == 3)
]) :-
	prime_lgc:left_(Mode, 3, P).

test(left__4,
[	forall(t__mode(Mode)),
	true(P == 3)
]) :-
	prime_lgc:left_(Mode, 4, P).

test(left__10,
[	forall(t__mode(Mode)),
	true(P == 7)
]) :-
	prime_lgc:left_(Mode, 10, P).

test(left__11,
[	forall(t__mode(Mode)),
	true(P == 11)
]) :-
	prime_lgc:left_(Mode, 11, P).

test(left__12,
[	forall(t__mode(Mode)),
	true(P == 11)
]) :-
	prime_lgc:left_(Mode, 12, P).

test(left__113,
[	forall(t__mode(Mode)),
	true(P == 113)
]) :-
	prime_lgc:left_(Mode, 113, P).

test(left__114,
[	forall(t__mode(Mode)),
	true(P == 113)
]) :-
	prime_lgc:left_(Mode, 114, P).

test(left__211,
[	forall(t__mode(Mode)),
	true(P == 211)
]) :-
	prime_lgc:left_(Mode, 211, P).

test(left__212,
[	forall(t__mode(Mode)),
	true(P == 211)
]) :-
	prime_lgc:left_(Mode, 212, P).

test(left__12289,
[	forall(t__mode(Mode)),
	true(P == 12289)
]) :-
	prime_lgc:left_(Mode, 12289, P).

test(left__12290,
[	forall(t__mode(Mode)),
	true(P == 12289)
]) :-
	prime_lgc:left_(Mode, 12290, P).

test(left__12291,
[	forall(t__mode(Mode)),
	true(P == 12289)
]) :-
	prime_lgc:left_(Mode, 12291, P).

:- end_tests('prime_lgc:left_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:next_').

t__next(Mode, N, P, C, M) :-
	prime_mem:clear_,
	prime_lgc:next_(Mode, N, P),
	prime_mem:count_(C),
	prime_mem:max_(M).

test(next__1,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (2, TC, TM))
]) :-
	t__next(Mode, 1, P, C, M).

test(next__2,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 3, TC, TM))),
	true((P, C, M) == (3, TC, TM))
]) :-
	t__next(Mode, 2, P, C, M).

test(next__3,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 5, TC, TM))),
	true((P, C, M) == (5, TC, TM))
]) :-
	t__next(Mode, 3, P, C, M).

test(next__4,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (5, TC, TM))
]) :-
	t__next(Mode, 4, P, C, M).

test(next__7,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 11, TC, TM))),
	true((P, C, M) == (11, TC, TM))
]) :-
	t__next(Mode, 7, P, C, M).

test(next__8,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (11, TC, TM))
]) :-
	t__next(Mode, 8, P, C, M).

test(next__10,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (11, TC, TM))
]) :-
	t__next(Mode, 10, P, C, M).

test(next__11,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 13, TC, TM))),
	true((P, C, M) == (13, TC, TM))
]) :-
	t__next(Mode, 11, P, C, M).

test(next__12,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (13, TC, TM))
]) :-
	t__next(Mode, 12, P, C, M).

test(next__13,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 17, TC, TM))),
	true((P, C, M) == (17, TC, TM))
]) :-
	t__next(Mode, 13, P, C, M).

test(next__112,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (113, TC, TM))
]) :-
	t__next(Mode, 112, P, C, M).

test(next__113,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 127, TC, TM))),
	true((P, C, M) == (127, TC, TM))
]) :-
	t__next(Mode, 113, P, C, M).

test(next__210,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (211, TC, TM))
]) :-
	t__next(Mode, 210, P, C, M).

test(next__211,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 223, TC, TM))),
	true((P, C, M) == (223, TC, TM))
]) :-
	t__next(Mode, 211, P, C, M).

test(next__12287,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (12289, TC, TM))
]) :-
	t__next(Mode, 12287, P, C, M).

test(next__12288,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (12289, TC, TM))
]) :-
	t__next(Mode, 12288, P, C, M).

test(next__12289,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 12301, TC, TM))),
	true((P, C, M) == (12301, TC, TM))
]) :-
	t__next(Mode, 12289, P, C, M).

:- end_tests('prime_lgc:next_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:prev_').

t__prev(Mode, N, P, C, M) :-
	prime_mem:clear_,
	prime_lgc:prev_(Mode, N, P),
	prime_mem:count_(C),
	prime_mem:max_(M).

test(prev__1,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__prev(Mode, 1, _, _, _).

test(prev__2,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__prev(Mode, 2, _, _, _).

test(prev__3,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 3, TC, TM))),
	true((P, C, M) == (2, TC, TM))
]) :-
	t__prev(Mode, 3, P, C, M).

test(prev__4,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (3, TC, TM))
]) :-
	t__prev(Mode, 4, P, C, M).

test(prev__7,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 7, TC, TM))),
	true((P, C, M) == (5, TC, TM))
]) :-
	t__prev(Mode, 7, P, C, M).

test(prev__8,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (7, TC, TM))
]) :-
	t__prev(Mode, 8, P, C, M).

test(prev__10,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (7, TC, TM))
]) :-
	t__prev(Mode, 10, P, C, M).

test(prev__11,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 11, TC, TM))),
	true((P, C, M) == (7, TC, TM))
]) :-
	t__prev(Mode, 11, P, C, M).

test(prev__12,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (11, TC, TM))
]) :-
	t__prev(Mode, 12, P, C, M).

test(prev__13,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 13, TC, TM))),
	true((P, C, M) == (11, TC, TM))
]) :-
	t__prev(Mode, 13, P, C, M).

test(prev__127,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 127, TC, TM))),
	true((P, C, M) == (113, TC, TM))
]) :-
	t__prev(Mode, 127, P, C, M).

test(prev__128,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (127, TC, TM))
]) :-
	t__prev(Mode, 128, P, C, M).

test(prev__223,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 223, TC, TM))),
	true((P, C, M) == (211, TC, TM))
]) :-
	t__prev(Mode, 223, P, C, M).

test(prev__224,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (223, TC, TM))
]) :-
	t__prev(Mode, 224, P, C, M).

test(prev__12301,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 12301, TC, TM))),
	true((P, C, M) == (12289, TC, TM))
]) :-
	t__prev(Mode, 12301, P, C, M).

test(prev__12302,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (12301, TC, TM))
]) :-
	t__prev(Mode, 12302, P, C, M).

:- end_tests('prime_lgc:prev_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:gen_3_').

t__gen_3(Mode, Inf, Max, Ps, C, M) :-
	prime_mem:clear_,
	findall(P,
	(	prime_lgc:gen_(Mode, Inf, P),
		(P >= Max, !; true)
	), Ps),
	prime_mem:count_(C),
	prime_mem:max_(M).

test(gen_3__3_3,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((Ps, C, M) == ([3], TC, TM))
]) :-
	t__gen_3(Mode, 3, 3, Ps, C, M).

test(gen_3__2_13,
[	forall((t__mode(Mode), t__cnt_max(Mode, 5, 13, TC, TM))),
	true((Ps, C, M) == ([2, 3, 5, 7, 11, 13], TC, TM))
]) :-
	t__gen_3(Mode, 1, 13, Ps, C, M).

test(gen_3__109_149,
[	forall((t__mode(Mode), t__cnt_max(Mode, 6, 149, TC, TM))),
	true((Ps, C, M) == ([109, 113, 127, 131, 137, 139, 149], TC, TM))
]) :-
	t__gen_3(Mode, 108, 149, Ps, C, M).

test(gen_3__211_229,
[	forall((t__mode(Mode), t__cnt_max(Mode, 3, 229, TC, TM))),
	true((Ps, C, M) == ([211, 223, 227, 229], TC, TM))
]) :-
	t__gen_3(Mode, 210, 229, Ps, C, M).

test(gen_3__12289_12329,
[	forall((t__mode(Mode), t__cnt_max(Mode, 3, 12329, TC, TM))),
	true((Ps, C, M) == ([12289, 12301, 12323, 12329], TC, TM))
]) :-
	t__gen_3(Mode, 12288, 12329, Ps, C, M).

:- end_tests('prime_lgc:gen_3_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:gen_4_').

t__gen_4(Mode, Inf, Sup, Ps, C, M) :-
	prime_mem:clear_,
	findall(P, prime_lgc:gen_(Mode, Inf, Sup, P), Ps),
	prime_mem:count_(C),
	prime_mem:max_(M).

test(gen_4__3_2,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((Ps, C, M) == ([], TC, TM))
]) :-
	t__gen_4(Mode, 3, 2, Ps, C, M).

test(gen_4__3_3,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((Ps, C, M) == ([3], TC, TM))
]) :-
	t__gen_4(Mode, 3, 3, Ps, C, M).

test(gen_4__2_13,
[	forall((t__mode(Mode), t__cnt_max(Mode, 6, 17, TC, TM))),
	true((Ps, C, M) == ([2, 3, 5, 7, 11, 13], TC, TM))
]) :-
	t__gen_4(Mode, 1, 14, Ps, C, M).

test(gen_4__109_149,
[	forall((t__mode(Mode), t__cnt_max(Mode, 7, 151, TC, TM))),
	true((Ps, C, M) == ([109, 113, 127, 131, 137, 139, 149], TC, TM))
]) :-
	t__gen_4(Mode, 108, 150, Ps, C, M).

test(gen_4__211_229,
[	forall((t__mode(Mode), t__cnt_max(Mode, 4, 233, TC, TM))),
	true((Ps, C, M) == ([211, 223, 227, 229], TC, TM))
]) :-
	t__gen_4(Mode, 210, 230, Ps, C, M).

test(gen_4__12289_12329,
[	forall((t__mode(Mode), t__cnt_max(Mode, 4, 12343, TC, TM))),
	true((Ps, C, M) == ([12289, 12301, 12323, 12329], TC, TM))
]) :-
	t__gen_4(Mode, 12288, 12330, Ps, C, M).

:- end_tests('prime_lgc:gen_4_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:gen_rev_4_').

t__gen_rev_4(Mode, Inf, Sup, Ps, C, M) :-
	prime_mem:clear_,
	findall(P, prime_lgc:gen_rev_(Mode, Inf, Sup, P), Ps),
	prime_mem:count_(C),
	prime_mem:max_(M).

test(gen_rev_4__3_2,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((Ps, C, M) == ([], TC, TM))
]) :-
	t__gen_rev_4(Mode, 3, 2, Ps, C, M).

test(gen_rev_4__3_3,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((Ps, C, M) == ([3], TC, TM))
]) :-
	t__gen_rev_4(Mode, 3, 3, Ps, C, M).

test(gen_rev_4__2_13,
[	forall((t__mode(Mode), t__cnt_max(Mode, 5, 13, TC, TM))),
	true((Ps, C, M) == ([13, 11, 7, 5, 3, 2], TC, TM))
]) :-
	t__gen_rev_4(Mode, 1, 14, Ps, C, M).

test(gen_rev_4__109_149,
[	forall((t__mode(Mode), t__cnt_max(Mode, 7, 149, TC, TM))),
	true((Ps, C, M) == ([149, 139, 137, 131, 127, 113, 109], TC, TM))
]) :-
	t__gen_rev_4(Mode, 108, 150, Ps, C, M).

test(gen_rev_4__211_229,
[	forall((t__mode(Mode), t__cnt_max(Mode, 4, 229, TC, TM))),
	true((Ps, C, M) == ([229, 227, 223, 211], TC, TM))
]) :-
	t__gen_rev_4(Mode, 210, 230, Ps, C, M).

test(gen_rev_4__12289_12329,
[	forall((t__mode(Mode), t__cnt_max(Mode, 4, 12329, TC, TM))),
	true((Ps, C, M) == ([12329, 12323, 12301, 12289], TC, TM))
]) :-
	t__gen_rev_4(Mode, 12288, 12330, Ps, C, M).

:- end_tests('prime_lgc:gen_rev_4_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:div_3_').

t__div_3(Mode, N, P, C, M) :-
	prime_mem:clear_,
	prime_lgc:div_(Mode, N, P),
	prime_mem:count_(C),
	prime_mem:max_(M).

test(div_3__1,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_3(Mode, 1, _, _, _).

test(div_3__2,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_3(Mode, 2, _, _, _).

test(div_3__3,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_3(Mode, 3, _, _, _).

test(div_3__4,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (2, TC, TM))
]) :-
	t__div_3(Mode, 4, P, C, M).

test(div_3__210,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (2, TC, TM))
]) :-
	t__div_3(Mode, 210, P, C, M).

test(div_3__211,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_3(Mode, 211, _, _, _).

test(div_3__213,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 3, TC, TM))),
	true((P, C, M) == (3, TC, TM))
]) :-
	t__div_3(Mode, 213, P, C, M).

test(div_3__221,
[	forall((t__mode(Mode), t__cnt_max(Mode, 5, 13, TC, TM))),
	true((P, C, M) == (13, TC, TM))
]) :-
	t__div_3(Mode, 221, P, C, M).

test(div_3__12300,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (2, TC, TM))
]) :-
	t__div_3(Mode, 12300, P, C, M).

test(div_3__12301,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_3(Mode, 12301, _, _, _).

test(div_3__isqrt,
[	true((P, C, M) == (41, 0, 2))
]) :-
	N = 12345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789011111,
	t__div_3(direct, N, P, C, M).

:- end_tests('prime_lgc:div_3_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:div_4_').

t__div_4(Mode, N, Inf, P, C, M) :-
	prime_mem:clear_,
	prime_lgc:div_(Mode, N, Inf, P),
	prime_mem:count_(C),
	prime_mem:max_(M).

test(div_4__1,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_4(Mode, 1, 3, _, _, _).

test(div_4__2,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_4(Mode, 2, 3, _, _, _).

test(div_4__3,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_4(Mode, 3, 3, _, _, _).

test(div_4__4,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_4(Mode, 4, 3, _, _, _).

test(div_4__210,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (3, TC, TM))
]) :-
	t__div_4(Mode, 210, 3, P, C, M).

test(div_4__211,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_4(Mode, 211, 3, _, _, _).

test(div_4__213,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (3, TC, TM))
]) :-
	t__div_4(Mode, 213, 3, P, C, M).

test(div_4__221,
[	forall((t__mode(Mode), t__cnt_max(Mode, 4, 13, TC, TM))),
	true((P, C, M) == (13, TC, TM))
]) :-
	t__div_4(Mode, 221, 3, P, C, M).

test(div_4__12300,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (3, TC, TM))
]) :-
	t__div_4(Mode, 12300, 3, P, C, M).

test(div_4__12301,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_4(Mode, 12301, 3, _, _, _).

:- end_tests('prime_lgc:div_4_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:div_rev_3_').

t__div_rev_3(Mode, N, P, C, M) :-
	prime_mem:clear_,
	prime_lgc:div_rev_(Mode, N, P),
	prime_mem:count_(C),
	prime_mem:max_(M).

test(div_rev_3__1,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_rev_3(Mode, 1, _, _, _).

test(div_rev_3__2,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_rev_3(Mode, 2, _, _, _).

test(div_rev_3__3,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_rev_3(Mode, 3, _, _, _).

test(div_rev_3__4,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (2, TC, TM))
]) :-
	t__div_rev_3(Mode, 4, P, C, M).

test(div_rev_3__210,
[	forall((t__mode(Mode), t__cnt_max(Mode, 23, 103, TC, TM))),
	true((P, C, M) == (7, TC, TM))
]) :-
	t__div_rev_3(Mode, 210, P, C, M).

test(div_rev_3__211,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_rev_3(Mode, 211, _, _, _).

test(div_rev_3__213,
[	forall((t__mode(Mode), t__cnt_max(Mode, 7, 103, TC, TM))),
	true((P, C, M) == (71, TC, TM))
]) :-
	t__div_rev_3(Mode, 213, P, C, M).

test(div_rev_3__221,
[	forall((t__mode(Mode), t__cnt_max(Mode, 22, 109, TC, TM))),
	true((P, C, M) == (17, TC, TM))
]) :-
	t__div_rev_3(Mode, 221, P, C, M).

test(div_rev_3__12300,
[	forall((t__mode(Mode), t__cnt_max(Mode, 788, 6143, TC, TM))),
	true((P, C, M) == (41, TC, TM))
]) :-
	t__div_rev_3(Mode, 12300, P, C, M).

test(div_rev_3__12301,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_rev_3(Mode, 12301, _, _, _).

:- end_tests('prime_lgc:div_rev_3_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:div_rev_4_').

t__div_rev_4(Mode, N, Sup, P, C, M) :-
	prime_mem:clear_,
	prime_lgc:div_rev_(Mode, N, Sup, P),
	prime_mem:count_(C),
	prime_mem:max_(M).

test(div_rev_4__1,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_rev_4(Mode, 1, 7, _, _, _).

test(div_rev_4__2,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_rev_4(Mode, 2, 7, _, _, _).

test(div_rev_4__3,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_rev_4(Mode, 3, 7, _, _, _).

test(div_rev_4__4,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (2, TC, TM))
]) :-
	t__div_rev_4(Mode, 4, 7, P, C, M).

test(div_rev_4__210,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((P, C, M) == (7, TC, TM))
]) :-
	t__div_rev_4(Mode, 210, 7, P, C, M).

test(div_rev_4__211,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_rev_4(Mode, 211, 7, _, _, _).

test(div_rev_4__213,
[	forall((t__mode(Mode), t__cnt_max(Mode, 2, 7, TC, TM))),
	true((P, C, M) == (3, TC, TM))
]) :-
	t__div_rev_4(Mode, 213, 7, P, C, M).

test(div_rev_4__221,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_rev_4(Mode, 221, 7, _, _, _).

test(div_rev_4__12300,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 7, TC, TM))),
	true((P, C, M) == (5, TC, TM))
]) :-
	t__div_rev_4(Mode, 12300, 7, P, C, M).

test(div_rev_4__12301,
[	forall(t__mode(Mode)),
	fail
]) :-
	t__div_rev_4(Mode, 12301, 7, _, _, _).

:- end_tests('prime_lgc:div_rev_4_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:fact_').

t__fact(Mode, N, PFs, C, M) :-
	prime_mem:clear_,
	prime_lgc:fact_(Mode, N, PFs),
	prime_mem:count_(C),
	prime_mem:max_(M).

test(fact__1,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((PFs, C, M) == ([1^1], TC, TM))
]) :-
	t__fact(Mode, 1, PFs, C, M).

test(fact__2,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((PFs, C, M) == ([2^1], TC, TM))
]) :-
	t__fact(Mode, 2, PFs, C, M).

test(fact__3,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((PFs, C, M) == ([3^1], TC, TM))
]) :-
	t__fact(Mode, 3, PFs, C, M).

test(fact__4,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((PFs, C, M) == ([2^2], TC, TM))
]) :-
	t__fact(Mode, 4, PFs, C, M).

test(fact__210,
[	forall((t__mode(Mode), t__cnt_max(Mode, 2, 5, TC, TM))),
	true((PFs, C, M) == ([2^1, 3^1, 5^1, 7^1], TC, TM))
]) :-
	t__fact(Mode, 210, PFs, C, M).

test(fact__211,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((PFs, C, M) == ([211^1], TC, TM))
]) :-
	t__fact(Mode, 211, PFs, C, M).

test(fact__213,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 3, TC, TM))),
	true((PFs, C, M) == ([3^1, 71^1], TC, TM))
]) :-
	t__fact(Mode, 213, PFs, C, M).

test(fact__221,
[	forall((t__mode(Mode), t__cnt_max(Mode, 5, 13, TC, TM))),
	true((PFs, C, M) == ([13^1, 17^1], TC, TM))
]) :-
	t__fact(Mode, 221, PFs, C, M).

test(fact__12300,
[	forall((t__mode(Mode), t__cnt_max(Mode, 2, 5, TC, TM))),
	true((PFs, C, M) == ([2^2, 3^1, 5^2, 41^1], TC, TM))
]) :-
	t__fact(Mode, 12300, PFs, C, M).

test(fact__12301,
[	forall((t__mode(Mode), t__cnt_max(Mode, 0, 2, TC, TM))),
	true((PFs, C, M) == ([12301^1], TC, TM))
]) :-
	t__fact(Mode, 12301, PFs, C, M).

test(fact__sup,
[	forall((t__mode(Mode), t__cnt_max(Mode, 2, 5, TC, TM))),
	true((PFs, C, M) == ([2^1, 5^1, 12345678901234567891^1], TC, TM))
]) :-
	t__fact(Mode, 123456789012345678910, PFs, C, M).

test(fact__2_3_5,
[	forall((t__mode(Mode), t__cnt_max(Mode, 1, 3, TC, TM))),
	setup(N is 2 * 3 * 5),
	true((PFs, C, M) == ([2^1, 3^1, 5^1], TC, TM))
]) :-
	t__fact(Mode, N, PFs, C, M).

test(fact__222_33_5555,
[	forall((t__mode(Mode), t__cnt_max(Mode, 2, 5, TC, TM))),
	setup(N is 2^3 * 3^2 * 5^4),
	true((PFs, C, M) == ([2^3, 3^2, 5^4], TC, TM))
]) :-
	t__fact(Mode, N, PFs, C, M).

:- end_tests('prime_lgc:fact_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
