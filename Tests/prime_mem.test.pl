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

/*	A simple prime number library :: memoization

@author		Julio P. Di Egidio
@version	1.2.5-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

:- use_module(library(plunit)).

:- ensure_loaded(module_inc).
:- module_inc('nan_numerics_prime_mem.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_mem:table_').

test(table_get__0,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:table_get_(_, _).

test(table_get__1,
[	true((T1, T2) == (2, 3))
]) :-
	prime_mem:clear_,
	prime_mem:table_add_(2, 3),
	prime_mem:table_get_(T1, T2).

test(table_get__1_s,
[	true((2, T2) == (2, 3))
]) :-
	prime_mem:clear_,
	prime_mem:table_add_(2, 3),
	prime_mem:table_get_(2, T2).

test(table_get__1_f,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:table_add_(2, 3),
	prime_mem:table_get_(3, _).

test(table_get__1_ss,
[	true
]) :-
	prime_mem:clear_,
	prime_mem:table_add_(2, 3),
	prime_mem:table_get_(2, 3).

test(table_get__1_sf,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:table_add_(2, 3),
	prime_mem:table_get_(2, 5).

test(table_get__1_fs,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:table_add_(2, 3),
	prime_mem:table_get_(3, 3).

test(table_get__1_ff,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:table_add_(2, 3),
	prime_mem:table_get_(3, 5).

test(table_get__3,
[	true((T1, T2) == (2, 3))
]) :-
	prime_mem:clear_,
	prime_mem:table_add_(2, 3),
	prime_mem:table_add_(3, 5),
	prime_mem:table_add_(5, 7),
	prime_mem:table_get_(T1, T2).

test(table_get__3_ord,
[	true((T1, T2) == (5, 7))
]) :-
	prime_mem:clear_,
	prime_mem:table_add_(5, 7),
	prime_mem:table_add_(2, 3),
	prime_mem:table_add_(3, 5),
	prime_mem:table_get_(T1, T2).

test(table_get__3_1,
[	true((2, T2) == (2, 3))
]) :-
	prime_mem:clear_,
	prime_mem:table_add_(2, 3),
	prime_mem:table_add_(3, 5),
	prime_mem:table_add_(5, 7),
	prime_mem:table_get_(2, T2).

test(table_get__3_2,
[	true((3, T2) == (3, 5))
]) :-
	prime_mem:clear_,
	prime_mem:table_add_(2, 3),
	prime_mem:table_add_(3, 5),
	prime_mem:table_add_(5, 7),
	prime_mem:table_get_(3, T2).

test(table_get__3_f,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:table_add_(2, 3),
	prime_mem:table_add_(3, 5),
	prime_mem:table_add_(5, 7),
	prime_mem:table_get_(7, _).

:- end_tests('prime_mem:table_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_mem:flags_').

t__flags__clear :-
	prime_mem:flag_clear_(f_a),
	prime_mem:flag_clear_(f_b),
	prime_mem:flag_clear_(f_c).

test(flag_get__0,
[	true(Val == 0)
]) :-
	t__flags__clear,
	prime_mem:flag_get_(f_a, Val).

test(flag_get__10,
[	true((Old, Val) == (0, 0))
]) :-
	t__flags__clear,
	prime_mem:flag_set_(f_a, Old, Old),
	prime_mem:flag_get_(f_a, Val).

test(flag_get__10s,
[	true(Val == 0)
]) :-
	t__flags__clear,
	prime_mem:flag_set_(f_a, 0),
	prime_mem:flag_get_(f_a, Val).

test(flag_get__10s_s,
[	true
]) :-
	t__flags__clear,
	prime_mem:flag_set_(f_a, 0),
	prime_mem:flag_get_(f_a, 0).

test(flag_get__10s_f,
[	fail
]) :-
	t__flags__clear,
	prime_mem:flag_set_(f_a, 0),
	prime_mem:flag_get_(f_a, 1).

test(flag_get__11,
[	true(Val == 1)
]) :-
	t__flags__clear,
	prime_mem:flag_set_(f_a, Old, Old+1),
	prime_mem:flag_get_(f_a, Val).

test(flag_get__11s,
[	true(Val == 1)
]) :-
	t__flags__clear,
	prime_mem:flag_set_(f_a, 1),
	prime_mem:flag_get_(f_a, Val).

test(flag_get__3_a,
[	true(Val == 1)
]) :-
	t__flags__clear,
	prime_mem:flag_set_(f_a, Old, Old+1),
	prime_mem:flag_set_(f_b, _, Old+2),
	prime_mem:flag_set_(f_c, _, Old+3),
	prime_mem:flag_get_(f_a, Val).

test(flag_get__3_b,
[	true(Val == 2)
]) :-
	t__flags__clear,
	prime_mem:flag_set_(f_a, Old, Old+1),
	prime_mem:flag_set_(f_b, _, Old+2),
	prime_mem:flag_set_(f_c, _, Old+3),
	prime_mem:flag_get_(f_b, Val).

test(flag_get__3_b_ord,
[	true(Val == 2)
]) :-
	t__flags__clear,
	prime_mem:flag_set_(f_c, Old, Old+3),
	prime_mem:flag_set_(f_a, _, Old+1),
	prime_mem:flag_set_(f_b, _, Old+2),
	prime_mem:flag_get_(f_b, Val).

:- end_tests('prime_mem:flags_').

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

test(get_1__1_s,
[	true
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:get_(3).

test(get_1__1_f,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:get_(5).

test(get_1__2,
[	true(P == 2)
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:get_(P).

test(get_1__3,
[	true(P == 2)
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:get_(P).

test(get_1__3_ord,
[	true(P == 2)
]) :-
	prime_mem:clear_,
	prime_mem:add_(5, 7),
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
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

test(get_2__1_s,
[	true
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:get_(2, 3).

test(get_2__1_f,
[	fail
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:get_(3, 5).

test(get_2__2,
[	true((P1, P2) == (2, 3))
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:get_(P1, P2).

test(get_2__3,
[	true((P1, P2) == (2, 3))
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:get_(P1, P2).

test(get_2__3_ord,
[	true((P1, P2) == (5, 7))
]) :-
	prime_mem:clear_,
	prime_mem:add_(5, 7),
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:get_(P1, P2).

:- end_tests('prime_mem:get_2_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_mem:count_').

test(count__0,
[	true(C == 0)
]) :-
	prime_mem:clear_,
	prime_mem:count_(C).

test(count__1,
[	true(C == 1)
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:count_(C).

test(count__2,
[	true(C == 2)
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:count_(C).

test(count__3,
[	true(C == 3)
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:count_(C).

test(count__3_ord,
[	true(C == 3)
]) :-
	prime_mem:clear_,
	prime_mem:add_(5, 7),
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:count_(C).

:- end_tests('prime_mem:count_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_mem:max_').

test(max__0,
[	true(P == 2)
]) :-
	prime_mem:clear_,
	prime_mem:max_(P).

test(max__1,
[	true(P == 3)
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:max_(P).

test(max__2,
[	true(P == 5)
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:max_(P).

test(max__3,
[	true(P == 7)
]) :-
	prime_mem:clear_,
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:add_(5, 7),
	prime_mem:max_(P).

test(max__3_ord,
[	true(P == 7)
]) :-
	prime_mem:clear_,
	prime_mem:add_(5, 7),
	prime_mem:add_(2, 3),
	prime_mem:add_(3, 5),
	prime_mem:max_(P).

:- end_tests('prime_mem:max_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
