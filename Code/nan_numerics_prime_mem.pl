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

% NOTE: Implements thread-local memoization.

:- module(prime_mem, []).

:- public
	gen_/2,			% ?P1:prime, ?P2:prime
	get_/1,			% ?P:prime
	get_/2,			% ?P1:prime, ?P2:prime
	add_/2,			% +P1:prime, +P2:prime
	count_/1,		% -Cnt:nonneg
	max_/1,			% -Max:prime
	clear_/0.		% 

/** <module> A simple prime number library :: memoization

Module =prime_mem= provides low-level predicates for the memoization of
pairs of consecutive prime numbers.

Implements thread-local memoization.

*NOTE*: Predicates in this module are not meant for public use.

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	gen_(?P1:prime, ?P2:prime) is nondet.
%
%	Generates all memoized pairs (for program inspection).

gen_(P1, P2) :-
	table_(P1, P2).

%!	get_(+P0:prime) is semidet.
%!	get_(+P0:prime, -P:prime) is semidet.
%!	get_(-P:prime, +P0:prime) is semidet.
%
%	True if P0 exists in some memoized pair with P its consecutive (resp.
%	for the two variants with arity =2=) successor/predecessor prime.

get_(2) :- !.
get_(P) :-
	table_get_(P, _), !.
get_(P) :-
	table_get_(_, P).

get_(P1, P2) :-
	table_get_(P1, P2).

%!	add_(+P1:prime, +P2:prime) is det.
%
%	Memoizes the pair P1, P2 of consecutive prime numbers.
%
%	*NOTE*: Does not check that P1, P2 is a pair of consecutive prime
%	numbers and that the pair has not already been memoized.

add_(P1, P2) :-
	table_add_(P1, P2),
	flag_set_('Nan.Numerics.Prime::mem_cnt', C, C + 1),
	flag_set_('Nan.Numerics.Prime::mem_max', P, max(P, P2)).

%!	count_(-Cnt:nonneg) is det.
%
%	Cnt is the total number of memoized pairs.

count_(Cnt) :-
	flag_get_('Nan.Numerics.Prime::mem_cnt', Cnt).

%!	max_(-Max:prime) is det.
%
%	Max is the greatest prime number that exists in any memoized pair.
%
%	Max is =2= if no pair has been memoized.

max_(Max) :-
	flag_get_('Nan.Numerics.Prime::mem_max', Max).

%!	clear_ is det.
%
%	Clears all memoization.

clear_ :-
	table_clear_,
	flag_set_('Nan.Numerics.Prime::mem_cnt', 0),
	flag_set_('Nan.Numerics.Prime::mem_max', 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	table_(?T1:nonneg, ?T2:nonneg) is nondet.

:- thread_local
	table_/2.

%	table_get_(?T1:nonneg, ?T2:nonneg) is semidet.

table_get_(T1, T2) :-
	table_(T1, T2), !.

%	table_add_(+T1:nonneg, +T2:nonneg) is det.

table_add_(T1, T2) :-
	assertz(table_(T1, T2)).

%	table_clear_ is det.

table_clear_ :-
	retractall(table_(_, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	flag_get_(+Key:atom, -Val:nonneg) is det.

flag_get_(Key, Val) :-
	(nb_current(Key, Val); Val = 0), !.

%	flag_set_(+Key:atom, +Val:nonneg) is det.
%	flag_set_(+Key:atom, -Old:nonneg, +New:arith(nonneg)) is det.

flag_set_(Key, Val) :-
	nb_setval(Key, Val).

flag_set_(Key, Old, New) :-
	flag_get_(Key, Old),
	Val is New,
	nb_setval(Key, Val).

%	flag_clear_(+Key:atom) is det.

flag_clear_(Key) :-
	nb_delete(Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- thread_initialization(clear_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
