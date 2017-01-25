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

:- module(prime_mem, []).

:- public
	gen_/2,				% ?P1:prime, ?P2:prime
	get_/1,				% ?P:prime
	get_/2,				% ?P1:prime, ?P2:prime
	add_/2,				% +P1:prime, +P2:prime
	fill_/3,			% :GNext:callable, +L:prime, +H:prime
	clear_/0,			% 
	count_/1,			% -Cnt:nonneg
	max_/1.				% -Max:prime

/** <module> A simple prime number library :: Memoization

*|Nan.Numerics.Prime (nan_numerics_prime.pl)|*

Module =prime_mem= (nan_numerics_prime_mem.pl)
provides low-level predicates for the memoization of pairs of consecutive
prime numbers.

Predicates that read the state can be safely called concurrently. Calls to
predicates that write the state, i.e. add_/2, fill_/3, clear_/0, are _not_
synchronized.

*NOTE*: Predicates in this module are _unsafe_, i.e. do not validate input
arguments and are not steadfast.

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	gen_(?P1:prime, ?P2:prime) is nondet.
%
%	Generates all memoized pairs of prime numbers (for program inspection).
%
%	This predicate can be safely called concurrently.

gen_(P1, P2) :-
	table_(P1, P2).

%!	get_(?P:prime) is semidet.
%
%	True if P is equal to =2= or P is an element of some memoized pair of
%	prime numbers.
%
%	This predicate can be safely called concurrently.

get_(2) :- !.
get_(P) :-
	table_(P, _), !.
get_(P) :-
	table_(_, P), !.

%!	get_(?P1:prime, ?P2:prime) is semidet.
%
%	True if P1, P2 is a memoized pair of consecutive prime numbers.
%
%	This predicate can be safely called concurrently.

get_(P1, P2) :-
	table_(P1, P2), !.

%!	add_(+P1:prime, +P2:prime) is det.
%
%	Memoizes the pair of consecutive prime numbers P1, P2 if the pair has
%	not yet been memoized, otherwise does nothing.
%
%	Calls to this predicate are _not_ synchronized.
%
%	*NOTE*: Does not check that P1, P2 is a pair of consecutive prime
%	numbers.

add_(P1, P2) :-
	table_(P1, P2), !.
add_(P1, P2) :-
	table_add_(P1, P2),
	flag_set_('Nan.Numerics.Prime::mem_cnt', C, C + 1),
	flag_set_('Nan.Numerics.Prime::mem_max', M, max(M, P2)).

%!	fill_(:GNext:callable, +L:prime, +H:prime) is det.
%
%	Ensures that all pairs of consecutive prime numbers whose elements are
%	greater than or equal to L and less than or equal to H have been
%	memoized.
%
%	Calls =|GNext(+P0:prime, -P:prime)|= to successively get consecutive
%	prime numbers, where GNext must be a predicate that, given any prime
%	number _P0_, returns the next prime number _P_.
%
%	Calls to this predicate are _not_ synchronized.
%
%	*NOTE*: Does not check that that L and P are prime numbers, and does
%	not check that GNext returns _P_ that is prime and consecutive to _P0_.

:- meta_predicate
	fill_(2, +, +).

fill_(GNext, L, H) :-
	print_message(silent, prime_mem:fill_begin(L, H)),
	ignore(fill__do(GNext, L, H)),
	print_message(silent, prime_mem:fill_end).

:- meta_predicate
	fill__do(2, +, +).

fill__do(GNext, P0, H) :-
	call(GNext, P0, P),
	P =< H,
	add_(P0, P),
	fill__do(GNext, P, H).

%!	clear_ is det.
%
%	Clears all memoization.
%
%	Calls to this predicate are _not_ synchronized.

clear_ :-
	table_clear_,
	flag_set_('Nan.Numerics.Prime::mem_cnt', 0),
	flag_set_('Nan.Numerics.Prime::mem_max', 2).

%!	count_(-Cnt:nonneg) is det.
%
%	Cnt is the total number of memoized pairs of numbers.
%
%	This predicate can be safely called concurrently.

count_(Cnt) :-
	flag_get_('Nan.Numerics.Prime::mem_cnt', Cnt).

%!	max_(-Max:prime) is det.
%
%	Max is the greatest number that is an element of any memoized pair. Max
%	is =2= if no pair has been memoized.
%
%	This predicate can be safely called concurrently.

max_(Max) :-
	flag_get_('Nan.Numerics.Prime::mem_max', Max).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	table_(?P1:prime, ?P2:prime) is nondet.

:- dynamic
	table_/2.

%	table_add_(+P1:prime, +P2:prime) is det.

table_add_(P1, P2) :-
	assertz(table_(P1, P2)).

%	table_clear_ is det.

table_clear_ :-
	retractall(table_(_, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	flags_(+Key:atom, ?Val:nonneg) is semidet.

:- dynamic
	flags_/2.

%	flag_get_(+Key:atom, -Val:nonneg) is det.

flag_get_(Key, Val) :-
	(flags_(Key, Val); Val = 0), !.

%	flag_set_(+Key:atom, +Val:nonneg) is det.
%	flag_set_(+Key:atom, -Old:nonneg, +New:arith(nonneg)) is det.

flag_set_(Key, Val) :-
	flag_del_(Key),
	assertz(flags_(Key, Val)).

flag_set_(Key, Old, New) :-
	flag_get_(Key, Old), Val is New,
	flag_set_(Key, Val).

%	flag_del_(+Key:atom) is det.

flag_del_(Key) :-
	ignore(retract(flags_(Key, _))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% prime_mem:m__flags.
% prime_mem:m__list.

%	m__flags is det.

m__flags :-
	ignore(forall(flags_(Key, Val), format('F(~a = ~d)~n', [Key, Val]))).

%	m__list is det.

m__list :-
	m__flags,
	ignore(forall(table_(P1, P2), format('P(~d, ~d)~n', [P1, P2]))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile
	prolog:message//1.

prolog:message(prime_mem:fill_begin(L, H)) -->
	[ 'Nan.Numerics.Prime: Filling memoization in [~d; ~d]: .'-[L, H], flush ].
prolog:message(prime_mem:fill_end) -->
	[ at_same_line, 'done.'-[] ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization(clear_, now).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
