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

/*	A simple prime number library :: logic

@author		Julio P. Di Egidio
@version	1.2.3-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

:- use_module(library(plunit)).

:- ensure_loaded(module_inc).
:- module_inc('nan_numerics_prime_lgc.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:test_', [setup(prime_mem:clear_)]).

test(test__1,
[	fail
]) :-
	prime_lgc:test_(1).

test(test__2,
[	true
]) :-
	prime_lgc:test_(2).

test(test__3,
[	true
]) :-
	prime_lgc:test_(3).

test(test__4,
[	fail
]) :-
	prime_lgc:test_(4).

test(test__10,
[	fail
]) :-
	prime_lgc:test_(10).

test(test__11,
[	true
]) :-
	prime_lgc:test_(11).

test(test__12,
[	fail
]) :-
	prime_lgc:test_(12).

test(test__112,
[	fail
]) :-
	prime_lgc:test_(112).

test(test__113,
[	true
]) :-
	prime_lgc:test_(113).

test(test__114,
[	fail
]) :-
	prime_lgc:test_(114).

test(test__220,
[	fail
]) :-
	prime_lgc:test_(220).

test(test__221,
[	fail
]) :-
	prime_lgc:test_(221).

test(test__222,
[	fail
]) :-
	prime_lgc:test_(222).

test(test__12300,
[	fail
]) :-
	prime_lgc:test_(12300).

test(test__12301,
[	true
]) :-
	prime_lgc:test_(12301).

:- end_tests('prime_lgc:test_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:div_', [setup(prime_mem:clear_)]).

test(div__1,
[	fail
]) :-
	prime_lgc:div_(1, _).

test(div__2,
[	fail
]) :-
	prime_lgc:div_(2, _).

test(div__3,
[	fail
]) :-
	prime_lgc:div_(3, _).

test(div__4,
[	true(P == 2)
]) :-
	prime_lgc:div_(4, P).

test(div__210,
[	true(P == 2)
]) :-
	prime_lgc:div_(210, P).

test(div__211,
[	fail
]) :-
	prime_lgc:div_(211, _).

test(div__213,
[	true(P == 3)
]) :-
	prime_lgc:div_(213, P).

test(div__221,
[	true(P == 13)
]) :-
	prime_lgc:div_(221, P).

test(div__12300,
[	true(P == 2)
]) :-
	prime_lgc:div_(12300, P).

test(div__12301,
[	fail
]) :-
	prime_lgc:div_(12301, _).

test(div__sup,
[	true(P == 3)
]) :-
	prime_lgc:div_(1234567891012345678901234567890123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789012345678910123456789011111, P).

:- end_tests('prime_lgc:div_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:div_rev_', [setup(prime_mem:clear_)]).

test(div_rev__1,
[	fail
]) :-
	prime_lgc:div_rev_(1, _).

test(div_rev__2,
[	fail
]) :-
	prime_lgc:div_rev_(2, _).

test(div_rev__3,
[	fail
]) :-
	prime_lgc:div_rev_(3, _).

test(div_rev__4,
[	true(P == 2)
]) :-
	prime_lgc:div_rev_(4, P).

test(div_rev__210,
[	true(P == 7)
]) :-
	prime_lgc:div_rev_(210, P).

test(div_rev__211,
[	fail
]) :-
	prime_lgc:div_rev_(211, _).

test(div_rev__213,
[	true(P == 71)
]) :-
	prime_lgc:div_rev_(213, P).

test(div_rev__221,
[	true(P == 17)
]) :-
	prime_lgc:div_rev_(221, P).

test(div_rev__12300,
[	true(P == 41)
]) :-
	prime_lgc:div_rev_(12300, P).

test(div_rev__12301,
[	fail
]) :-
	prime_lgc:div_rev_(12301, _).

:- end_tests('prime_lgc:div_rev_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:fact_', [setup(prime_mem:clear_)]).

test(fact__1,
[	true(PFs == [1^1])
]) :-
	prime_lgc:fact_(1, PFs).

test(fact__2,
[	true(PFs == [2^1])
]) :-
	prime_lgc:fact_(2, PFs).

test(fact__3,
[	true(PFs == [3^1])
]) :-
	prime_lgc:fact_(3, PFs).

test(fact__4,
[	true(PFs == [2^2])
]) :-
	prime_lgc:fact_(4, PFs).

test(fact__210,
[	true(PFs == [2^1, 3^1, 5^1, 7^1])
]) :-
	prime_lgc:fact_(210, PFs).

test(fact__211,
[	true(PFs == [211^1])
]) :-
	prime_lgc:fact_(211, PFs).

test(fact__213,
[	true(PFs == [3^1, 71^1])
]) :-
	prime_lgc:fact_(213, PFs).

test(fact__221,
[	true(PFs == [13^1, 17^1])
]) :-
	prime_lgc:fact_(221, PFs).

test(fact__12300,
[	true(PFs == [2^2, 3^1, 5^2, 41^1])
]) :-
	prime_lgc:fact_(12300, PFs).

test(fact__12301,
[	true(PFs == [12301^1])
]) :-
	prime_lgc:fact_(12301, PFs).

test(fact__sup,
[	true(PFs == [2^1, 5^1, 12345678901234567891^1])
]) :-
	prime_lgc:fact_(123456789012345678910, PFs).

:- end_tests('prime_lgc:fact_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:gen_2_', [setup(prime_mem:clear_)]).

test(gen_2__2_13,
[	all(P == [2, 3, 5, 7, 11, 13])
]) :-
	prime_lgc:gen_(1, P),
	(P >= 13, !; true).

test(gen_2__109_149,
[	all(P == [109, 113, 127, 131, 137, 139, 149])
]) :-
	prime_lgc:gen_(108, P),
	(P >= 149, !; true).

test(gen_2__211_229,
[	all(P == [211, 223, 227, 229])
]) :-
	prime_lgc:gen_(210, P),
	(P >= 229, !; true).

test(gen_2__12289_12329,
[	all(P == [12289, 12301, 12323, 12329])
]) :-
	prime_lgc:gen_(12288, P),
	(P >= 12329, !; true).

:- end_tests('prime_lgc:gen_2_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:gen_3_', [setup(prime_mem:clear_)]).

test(gen_3__2_13,
[	all(P == [2, 3, 5, 7, 11, 13])
]) :-
	prime_lgc:gen_(1, 14, P).

test(gen_3__109_149,
[	all(P == [109, 113, 127, 131, 137, 139, 149])
]) :-
	prime_lgc:gen_(108, 150, P).

test(gen_3__211_229,
[	all(P == [211, 223, 227, 229])
]) :-
	prime_lgc:gen_(210, 230, P).

test(gen_3__12289_12329,
[	all(P == [12289, 12301, 12323, 12329])
]) :-
	prime_lgc:gen_(12288, 12330, P).

:- end_tests('prime_lgc:gen_3_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:gen_p_2_', [setup(prime_mem:clear_)]).

test(gen_p_2__2_13,
[	all(P == [2, 3, 5, 7, 11, 13])
]) :-
	prime_lgc:gen_p_(2, P),
	(P >= 13, !; true).

test(gen_p_2__109_149,
[	all(P == [109, 113, 127, 131, 137, 139, 149])
]) :-
	prime_lgc:gen_p_(109, P),
	(P >= 149, !; true).

test(gen_p_2__211_229,
[	all(P == [211, 223, 227, 229])
]) :-
	prime_lgc:gen_p_(211, P),
	(P >= 229, !; true).

test(gen_p_2__12289_12329,
[	all(P == [12289, 12301, 12323, 12329])
]) :-
	prime_lgc:gen_p_(12289, P),
	(P >= 12329, !; true).

:- end_tests('prime_lgc:gen_p_2_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:gen_p_3_', [setup(prime_mem:clear_)]).

test(gen_p_3__2_13,
[	all(P == [2, 3, 5, 7, 11, 13])
]) :-
	prime_lgc:gen_p_(2, 13, P).

test(gen_p_3__109_149,
[	all(P == [109, 113, 127, 131, 137, 139, 149])
]) :-
	prime_lgc:gen_p_(109, 149, P).

test(gen_p_3__211_229,
[	all(P == [211, 223, 227, 229])
]) :-
	prime_lgc:gen_p_(211, 229, P).

test(gen_p_3__12289_12329,
[	all(P == [12289, 12301, 12323, 12329])
]) :-
	prime_lgc:gen_p_(12289, 12329, P).

:- end_tests('prime_lgc:gen_p_3_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:gen_rev_2_', [setup(prime_mem:clear_)]).

test(gen_rev_2__2_13,
[	all(P == [13, 11, 7, 5, 3, 2])
]) :-
	prime_lgc:gen_rev_(14, P).

test(gen_rev_2__109_149,
[	all(P == [149, 139, 137, 131, 127, 113, 109])
]) :-
	prime_lgc:gen_rev_(150, P),
	(P =< 109, !; true).

test(gen_rev_2__211_229,
[	all(P == [229, 227, 223, 211])
]) :-
	prime_lgc:gen_rev_(230, P),
	(P =< 211, !; true).

test(gen_rev_2__12289_12329,
[	all(P == [12329, 12323, 12301, 12289])
]) :-
	prime_lgc:gen_rev_(12330, P),
	(P =< 12289, !; true).

:- end_tests('prime_lgc:gen_rev_2_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:gen_rev_3_', [setup(prime_mem:clear_)]).

test(gen_rev_3__2_13,
[	all(P == [13, 11, 7, 5, 3, 2])
]) :-
	prime_lgc:gen_rev_(1, 14, P).

test(gen_rev_3__109_149,
[	all(P == [149, 139, 137, 131, 127, 113, 109])
]) :-
	prime_lgc:gen_rev_(108, 150, P).

test(gen_rev_3__211_229,
[	all(P == [229, 227, 223, 211])
]) :-
	prime_lgc:gen_rev_(210, 230, P).

test(gen_rev_3__12289_12329,
[	all(P == [12329, 12323, 12301, 12289])
]) :-
	prime_lgc:gen_rev_(12288, 12330, P).

:- end_tests('prime_lgc:gen_rev_3_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:gen_rev_p_2_', [setup(prime_mem:clear_)]).

test(gen_rev_p_2__2_13,
[	all(P == [13, 11, 7, 5, 3, 2])
]) :-
	prime_lgc:gen_rev_p_(13, P).

test(gen_rev_p_2__109_149,
[	all(P == [149, 139, 137, 131, 127, 113, 109])
]) :-
	prime_lgc:gen_rev_p_(149, P),
	(P =< 109, !; true).

test(gen_rev_p_2__211_229,
[	all(P == [229, 227, 223, 211])
]) :-
	prime_lgc:gen_rev_p_(229, P),
	(P =< 211, !; true).

test(gen_rev_p_2__12289_12329,
[	all(P == [12329, 12323, 12301, 12289])
]) :-
	prime_lgc:gen_rev_p_(12329, P),
	(P =< 12289, !; true).

:- end_tests('prime_lgc:gen_rev_p_2_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:gen_rev_p_3_', [setup(prime_mem:clear_)]).

test(gen_rev_p_3__2_13,
[	all(P == [13, 11, 7, 5, 3, 2])
]) :-
	prime_lgc:gen_rev_p_(2, 13, P).

test(gen_rev_p_3__109_149,
[	all(P == [149, 139, 137, 131, 127, 113, 109])
]) :-
	prime_lgc:gen_rev_p_(109, 149, P).

test(gen_rev_p_3__211_229,
[	all(P == [229, 227, 223, 211])
]) :-
	prime_lgc:gen_rev_p_(211, 229, P).

test(gen_rev_p_3__12289_12329,
[	all(P == [12329, 12323, 12301, 12289])
]) :-
	prime_lgc:gen_rev_p_(12289, 12329, P).

:- end_tests('prime_lgc:gen_rev_p_3_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:next_', [setup(prime_mem:clear_)]).

test(next__1,
[	true(P == 2)
]) :-
	prime_lgc:next_(1, P).

test(next__2,
[	true(P == 3)
]) :-
	prime_lgc:next_(2, P).

test(next__3,
[	true(P == 5)
]) :-
	prime_lgc:next_(3, P).

test(next__4,
[	true(P == 5)
]) :-
	prime_lgc:next_(4, P).

test(next__7,
[	true(P == 11)
]) :-
	prime_lgc:next_(7, P).

test(next__8,
[	true(P == 11)
]) :-
	prime_lgc:next_(8, P).

test(next__10,
[	true(P == 11)
]) :-
	prime_lgc:next_(10, P).

test(next__11,
[	true(P == 13)
]) :-
	prime_lgc:next_(11, P).

test(next__12,
[	true(P == 13)
]) :-
	prime_lgc:next_(12, P).

test(next__13,
[	true(P == 17)
]) :-
	prime_lgc:next_(13, P).

test(next__112,
[	true(P == 113)
]) :-
	prime_lgc:next_(112, P).

test(next__113,
[	true(P == 127)
]) :-
	prime_lgc:next_(113, P).

test(next__210,
[	true(P == 223)
]) :-
	prime_lgc:next_(211, P).

test(next__211,
[	true(P == 223)
]) :-
	prime_lgc:next_(211, P).

test(next__12288,
[	true(P == 12289)
]) :-
	prime_lgc:next_(12288, P).

test(next__12289,
[	true(P == 12301)
]) :-
	prime_lgc:next_(12289, P).

:- end_tests('prime_lgc:next_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:next_p_', [setup(prime_mem:clear_)]).

test(next_p__2,
[	true(P == 3)
]) :-
	prime_lgc:next_p_(2, P).

test(next_p__3,
[	true(P == 5)
]) :-
	prime_lgc:next_p_(3, P).

test(next_p__7,
[	true(P == 11)
]) :-
	prime_lgc:next_p_(7, P).

test(next_p__11,
[	true(P == 13)
]) :-
	prime_lgc:next_p_(11, P).

test(next_p__13,
[	true(P == 17)
]) :-
	prime_lgc:next_p_(13, P).

test(next_p__113,
[	true(P == 127)
]) :-
	prime_lgc:next_p_(113, P).

test(next_p__211,
[	true(P == 223)
]) :-
	prime_lgc:next_p_(211, P).

test(next_p__12289,
[	true(P == 12301)
]) :-
	prime_lgc:next_p_(12289, P).

:- end_tests('prime_lgc:next_p_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:prev_', [setup(prime_mem:clear_)]).

test(prev__1,
[	fail
]) :-
	prime_lgc:prev_(1, _).

test(prev__2,
[	fail
]) :-
	prime_lgc:prev_(2, _).

test(prev__3,
[	true(P == 2)
]) :-
	prime_lgc:prev_(3, P).

test(prev__4,
[	true(P == 3)
]) :-
	prime_lgc:prev_(4, P).

test(prev__7,
[	true(P == 5)
]) :-
	prime_lgc:prev_(7, P).

test(prev__8,
[	true(P == 7)
]) :-
	prime_lgc:prev_(8, P).

test(prev__10,
[	true(P == 7)
]) :-
	prime_lgc:prev_(10, P).

test(prev__11,
[	true(P == 7)
]) :-
	prime_lgc:prev_(11, P).

test(prev__12,
[	true(P == 11)
]) :-
	prime_lgc:prev_(12, P).

test(prev__13,
[	true(P == 11)
]) :-
	prime_lgc:prev_(13, P).

test(prev__127,
[	true(P == 113)
]) :-
	prime_lgc:prev_(127, P).

test(prev__128,
[	true(P == 127)
]) :-
	prime_lgc:prev_(128, P).

test(prev__223,
[	true(P == 211)
]) :-
	prime_lgc:prev_(223, P).

test(prev__224,
[	true(P == 223)
]) :-
	prime_lgc:prev_(224, P).

test(prev__12301,
[	true(P == 12289)
]) :-
	prime_lgc:prev_(12301, P).

test(prev__12302,
[	true(P == 12301)
]) :-
	prime_lgc:prev_(12302, P).

:- end_tests('prime_lgc:prev_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:prev_p_', [setup(prime_mem:clear_)]).

test(prev_p__2,
[	fail
]) :-
	prime_lgc:prev_p_(2, _).

test(prev_p__3,
[	true(P == 2)
]) :-
	prime_lgc:prev_p_(3, P).

test(prev_p__7,
[	true(P == 5)
]) :-
	prime_lgc:prev_p_(7, P).

test(prev_p__11,
[	true(P == 7)
]) :-
	prime_lgc:prev_p_(11, P).

test(prev_p__13,
[	true(P == 11)
]) :-
	prime_lgc:prev_p_(13, P).

test(prev_p__127,
[	true(P == 113)
]) :-
	prime_lgc:prev_p_(127, P).

test(prev_p__223,
[	true(P == 211)
]) :-
	prime_lgc:prev_p_(223, P).

test(prev_p__12301,
[	true(P == 12289)
]) :-
	prime_lgc:prev_p_(12301, P).

:- end_tests('prime_lgc:prev_p_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:right_', [setup(prime_mem:clear_)]).

test(right__1,
[	true(P == 2)
]) :-
	prime_lgc:right_(1, P).

test(right__2,
[	true(P == 2)
]) :-
	prime_lgc:right_(2, P).

test(right__3,
[	true(P == 3)
]) :-
	prime_lgc:right_(3, P).

test(right__4,
[	true(P == 5)
]) :-
	prime_lgc:right_(4, P).

test(right__10,
[	true(P == 11)
]) :-
	prime_lgc:right_(10, P).

test(right__11,
[	true(P == 11)
]) :-
	prime_lgc:right_(11, P).

test(right__12,
[	true(P == 13)
]) :-
	prime_lgc:right_(12, P).

test(right__113,
[	true(P == 113)
]) :-
	prime_lgc:right_(113, P).

test(right__114,
[	true(P == 127)
]) :-
	prime_lgc:right_(114, P).

test(right__211,
[	true(P == 211)
]) :-
	prime_lgc:right_(211, P).

test(right__212,
[	true(P == 223)
]) :-
	prime_lgc:right_(212, P).

test(right__12289,
[	true(P == 12289)
]) :-
	prime_lgc:right_(12289, P).

test(right__12290,
[	true(P == 12301)
]) :-
	prime_lgc:right_(12290, P).

:- end_tests('prime_lgc:right_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('prime_lgc:left_', [setup(prime_mem:clear_)]).

test(left__1,
[	fail
]) :-
	prime_lgc:left_(1, _).

test(left__2,
[	true(P == 2)
]) :-
	prime_lgc:left_(2, P).

test(left__3,
[	true(P == 3)
]) :-
	prime_lgc:left_(3, P).

test(left__4,
[	true(P == 3)
]) :-
	prime_lgc:left_(4, P).

test(left__10,
[	true(P == 7)
]) :-
	prime_lgc:left_(10, P).

test(left__11,
[	true(P == 11)
]) :-
	prime_lgc:left_(11, P).

test(left__12,
[	true(P == 11)
]) :-
	prime_lgc:left_(12, P).

test(left__113,
[	true(P == 113)
]) :-
	prime_lgc:left_(113, P).

test(left__114,
[	true(P == 113)
]) :-
	prime_lgc:left_(114, P).

test(left__211,
[	true(P == 211)
]) :-
	prime_lgc:left_(211, P).

test(left__212,
[	true(P == 211)
]) :-
	prime_lgc:left_(212, P).

test(left__12289,
[	true(P == 12289)
]) :-
	prime_lgc:left_(12289, P).

test(left__12290,
[	true(P == 12289)
]) :-
	prime_lgc:left_(12290, P).

:- end_tests('prime_lgc:left_').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
