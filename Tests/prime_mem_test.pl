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

:- module(prime_mem_test, []).

/** <module> A simple prime number library :: Memoization tests

Tests for module =prime_mem=.

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

:- use_module(library(plunit)).

:- use_module(loader).
:- loader:load_module('nan_numerics_prime_mem.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t__gnext(2, 3).
t__gnext(3, 5).
t__gnext(5, 7).
t__gnext(7, 11).

t__gnext__L(2).
t__gnext__H(7).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_mem:get_1_').

test(get_1__0,
[	true(P == 2)
]) :-
	prime_mem:clear_,
	prime_mem:get_(P).

test(get_1__1,
[	true(P == 2)
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:get_(P).

test(get_1__1_s_2,
[	true
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:get_(2).

test(get_1__1_s_3,
[	true
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:get_(3).

test(get_1__1_f_1,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:get_(1).

test(get_1__1_f_5,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:get_(5).

test(get_1__3,
[	true(P == 2)
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:get_(P).

test(get_1__3_s_3,
[	true
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:get_(3).

test(get_1__3_s_5,
[	true
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:get_(5).

test(get_1__3_s_7,
[	true
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:get_(7).

test(get_1__3_f_1,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:get_(1).

test(get_1__3_f_11,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:get_(11).

test(get_1__3_ord,
[	true(P == 2)
]) :-
	prime_mem:clear_,
	prime_mem:add_(5, 7),
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:get_(P).

test(get_1__3_ord_s_3,
[	true
]) :-
	prime_mem:clear_,
	prime_mem:add_(5, 7),
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:get_(3).

test(get_1__3_ord_s_5,
[	true
]) :-
	prime_mem:clear_,
	prime_mem:add_(5, 7),
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:get_(5).

test(get_1__3_ord_s_7,
[	true
]) :-
	prime_mem:clear_,
	prime_mem:add_(5, 7),
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:get_(7).

test(get_1__3_ord_f_1,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:add_(5, 7),
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:get_(1).

test(get_1__3_ord_f_11,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:add_(5, 7),
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:get_(11).

test(get_1__fill,
[	setup((t__gnext__L(L), t__gnext__H(H))),
	true(P == 2)
]) :-
	prime_mem:clear_,
	prime_mem:fill_(prime_mem_test:t__gnext, L, H),
	prime_mem:get_(P).

:- end_tests('prime_mem:get_1_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_mem:get_2_').

test(get_2__0,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:get_(_, _).

test(get_2__1,
[	true((P1, P2) == (2, 3))
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:get_(P1, P2).

test(get_2__1_s_2v,
[	true(P2 == 3)
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:get_(2, P2).

test(get_2__1_s_v3,
[	true(P1 == 2)
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:get_(P1, 3).

test(get_2__1_f_1v,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:get_(1, _).

test(get_2__1_f_v2,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:get_(_, 2).

test(get_2__1_f_3v,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:get_(3, _).

test(get_2__1_f_v5,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:get_(_, 5).

test(get_2__3,
[	true((P1, P2) == (2, 3))
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:get_(P1, P2).

test(get_2__3_s_3v,
[	true(P2 == 5)
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:get_(3, P2).

test(get_2__3_s_v7,
[	true(P1 == 5)
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:get_(P1, 7).

test(get_2__3_f_1v,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:get_(1, _).

test(get_2__3_f_v11,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:get_(_, 11).

test(get_2__3_ord,
[	true((P1, P2) == (5, 7))
]) :-
	prime_mem:clear_,
	prime_mem:add_(5, 7),
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:get_(P1, P2).

test(get_2__3_ord_s_3v,
[	true(P2 == 5)
]) :-
	prime_mem:clear_,
	prime_mem:add_(5, 7),
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:get_(3, P2).

test(get_2__3_ord_s_v7,
[	true(P1 == 5)
]) :-
	prime_mem:clear_,
	prime_mem:add_(5, 7),
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:get_(P1, 7).

test(get_2__3_ord_f_1v,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:add_(5, 7),
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:get_(1, _).

test(get_2__3_ord_f_v11,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:add_(5, 7),
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:get_(_, 11).

test(get_2__fill,
[	setup((t__gnext__L(L), t__gnext__H(H))),
	true((P1, P2) == (2, 3))
]) :-
	prime_mem:clear_,
	prime_mem:fill_(prime_mem_test:t__gnext, L, H),
	prime_mem:get_(P1, P2).

:- end_tests('prime_mem:get_2_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_mem:cnt_max_').

test(cnt_max__0,
[	true((Cnt, Max) == (0, 2))
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:clear_,
	prime_mem:count_(Cnt),
	prime_mem:max_(Max).

test(cnt_max__1,
[	true((Cnt, Max) == (1, 3))
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:count_(Cnt),
	prime_mem:max_(Max).

test(cnt_max__2,
[	true((Cnt, Max) == (2, 5))
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:count_(Cnt),
	prime_mem:max_(Max).

test(cnt_max__3,
[	true((Cnt, Max) == (3, 7))
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:count_(Cnt),
	prime_mem:max_(Max).

test(cnt_max__3_ord,
[	true((Cnt, Max) == (3, 7))
]) :-
	prime_mem:clear_,
	prime_mem:add_(5, 7),
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:count_(Cnt),
	prime_mem:max_(Max).

test(get_2__fill,
[	setup((t__gnext__L(L), t__gnext__H(H))),
	true((Cnt, Max) == (3, 7))
]) :-
	prime_mem:clear_,
	prime_mem:fill_(prime_mem_test:t__gnext, L, H),
	prime_mem:count_(Cnt),
	prime_mem:max_(Max).

:- end_tests('prime_mem:cnt_max_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
