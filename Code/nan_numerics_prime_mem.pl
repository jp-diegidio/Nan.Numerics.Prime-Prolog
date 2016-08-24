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

% (SWI-Prolog 7.3.24)

% TODO: Implement size limits?

:- module(prime_mem, []).

:- public
	gen_/2,			% ?P1:prime, ?P2:prime
	get_/1,			% ?P:prime
	get_/2,			% ?P1:prime, ?P2:prime
	add_/2,			% +P1:prime, +P2:prime
	count_/1,		% -Count:nonneg
	max_/1,			% -Max:prime
	clear_/0.		% 

/** <module> A simple prime number library :: memoization

The module =prime_mem= provides low-level predicates for the memoization of
pairs of consecutive prime numbers.

*NOTE*: Predicates in this module are not meant for public use.

@author		Julio P. Di Egidio
@version	1.0 (beta)
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
@tbd		Implement size limits?
*/

:- initialization(clear_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	gen_(?P1:prime, ?P2:prime) is nondet.
%
%	Generates the memoized pairs (for program inspection).

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
%	Memoizes the pair P1, P2.
%
%	*NOTE*: Does not check that P1, P2 is a pair of consecutive prime
%	numbers and that the pair has not already been memoized.

add_(P1, P2) :-
	table_add_(P1, P2),
	flag_set_(count, C, C + 1),
	flag_set_(max, P, max(P, P2)).

%!	count_(-Count:nonneg) is det.
%
%	Count is the number of memoized pairs.

count_(Count) :-
	flag_get_(count, Count).

%!	max_(-Max:prime) is det.
%
%	Max is the greatest prime number that exists in any memoized pair.
%
%	Max is =2= if no memoized pair exists.

max_(Max) :-
	flag_get_(max, Max).

%!	clear_ is det.
%
%	Clears all memoization.

clear_ :-
	clearall_,
	flag_set_(count, 0),
	flag_set_(max, 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	table_(?T1:nonneg, ?T2:nonneg) is nondet.

:- dynamic
	table_/2.

%	table_get_(?T1:nonneg, ?T2:nonneg) is semidet.

table_get_(T1, T2) :-
	table_(T1, T2), !.

%	table_add_(+T1:nonneg, +T2:nonneg) is det.

table_add_(T1, T2) :-
	assertz(table_(T1, T2)).

%	flags_(?Key:atom, ?Val:nonneg) is nondet.

:- dynamic
	flags_/2.

%	flag_get_(+Key:atom, -Val:nonneg) is det.

flag_get_(Key, Val) :-
	(flags_(Key, Val); Val = 0), !.

%	flag_set_(+Key:atom, +New:arith(nonneg)) is det.

flag_set_(Key, New) :-
	flag_set_(Key, _, New).

%	flag_set_(+Key:atom, -Old:nonneg, +New:arith(nonneg)) is det.

flag_set_(Key, Old, New) :-
	flag_get_(Key, Old), Val is New,
	ignore((
		Val \== Old,
		ignore(retract(flags_(Key, _))),
		assertz(flags_(Key, Val))
	)).

%	clearall_ is det.

clearall_ :-
	retractall(table_(_, _)),
	retractall(flags_(_, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
