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

:- module(prime_lgc, []).

:- public
	test_/2,			% +Mode:pmode, +N:posint
	right_/3,			% +Mode:pmode, +N:posint, -P:prime
	left_/3,			% +Mode:pmode, +N:posint, -P:prime
	next_p_/3,			% +Mode:pmode, +P0:prime, -P:prime
	prev_p_/3,			% +Mode:pmode, +P0:prime, -P:prime
	gen_p_/3,			% +Mode:pmode, +L:prime, -P:prime
	gen_p_/4,			% +Mode:pmode, +L:prime, +Sup:posint, -P:prime
	gen_rev_p_/4,		% +Mode:pmode, +Inf:posint, +H:prime, -P:prime
	next_/3,			% +Mode:pmode, +N:posint, -P:prime
	prev_/3,			% +Mode:pmode, +N:posint, -P:prime
	gen_/3,				% +Mode:pmode, +Inf:posint, -P:prime
	gen_/4,				% +Mode:pmode, +Inf:posint, +Sup:posint, -P:prime
	gen_rev_/4,			% +Mode:pmode, +Inf:posint, +Sup:posint, -P:prime
	div_/3,				% +Mode:pmode, +N:posint, -P:prime
	div_/4,				% +Mode:pmode, +N:posint, +Inf:posint, -P:prime
	div_rev_/3,			% +Mode:pmode, +N:posint, -P:prime
	div_rev_/4,			% +Mode:pmode, +N:posint, +Sup:posint, -P:prime
	fact_/3.			% +Mode:pmode, +N:posint, -PFs:list(pfact)

/** <module> A simple prime number library :: Logic

*|Library Nan.Numerics.Prime (nan_numerics_prime.pl)|*

Module =prime_lgc= (nan_numerics_prime_lgc.pl)
provides low-level predicates to test positive integer numbers for
primality, find consecutive prime numbers, generate prime numbers in some
interval, find divisors and factor numbers.

Predicates that _|only read|_ the state take a _Mode_ argument that selects
one of four variants:

	* =direct= : does not use memoization
	* =mem_read= : only reads memoization
	* =mem_write= : equivalent to =mem_read= (for uniformity)
	* =mem_sync= : equivalent to =mem_read= (for uniformity)

Predicates that _|can write|_ the state take a _Mode_ argument that selects
one of four variants:

	* =direct= : does not use memoization
	* =mem_read= : only reads memoization
	* =mem_write= : reads and writes memoization
	* =mem_sync= : reads and writes memoization with synchronization

Predicates that only read the state can be safely called concurrently.
Calls to predicates that can write the state are _not_ synchronized if
_Mode_ is =mem_write=.

*NOTE*: Predicates in this module are _unsafe_, i.e. do not validate input
arguments and are not steadfast.

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
@tbd		Integrate =isqrt= function from GMP?
*/

:- use_module(nan_numerics_prime_mem).
:- use_module(nan_numerics_prime_whl).
:- use_module(nan_numerics_prime_prb).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	test_(+Mode:pmode, +N:posint) is semidet.
%
%	True if N is a prime number.
%
%	Mode selects one of four variants:
%
%		* =direct= : does not use memoization
%		* =mem_read= : only reads memoization
%		* =mem_write= : equivalent to =mem_read= (for uniformity)
%		* =mem_sync= : equivalent to =mem_read= (for uniformity)
%
%	This predicate can be safely called concurrently.

test_(direct, N) :- !,
	prime_whl:test_(N, Cert),
	test__do(Cert, N).

test_(_, N) :-
	prime_mem:get_(N), !.
test_(_, N) :-
	test_(direct, N).

test__do(true, _) :- !.
test__do(_, W) :-
	test__do(W).

%!	right_(+Mode:pmode, +N:posint, -P:prime) is det.
%
%	P is the smallest prime number greater than or equal to N.
%
%	Mode selects one of four variants:
%
%		* =direct= : does not use memoization
%		* =mem_read= : only reads memoization
%		* =mem_write= : equivalent to =mem_read= (for uniformity)
%		* =mem_sync= : equivalent to =mem_read= (for uniformity)
%
%	This predicate can be safely called concurrently.

right_(direct, N, P) :- !,
	prime_whl:right_(N, W, Cert),
	right__do(direct, Cert, W, P).

right_(_, N, N) :-
	prime_mem:get_(N), !.
right_(Mode, N, P) :-
	prime_whl:right_(N, W, Cert),
	right__do(Mode, Cert, W, P).

right__do(_, Cert, W, W) :-
	test__do(Cert, W), !.
right__do(Mode, _, W0, P) :-
	W1 is W0 + 1,
	right_(Mode, W1, P).

%!	left_(+Mode:pmode, +N:posint, -P:prime) is semidet.
%
%	P is the greatest prime number less than or equal to N.  Fails if N is
%	equal to =1=.
%
%	Mode selects one of four variants:
%
%		* =direct= : does not use memoization
%		* =mem_read= : only reads memoization
%		* =mem_write= : equivalent to =mem_read= (for uniformity)
%		* =mem_sync= : equivalent to =mem_read= (for uniformity)
%
%	This predicate can be safely called concurrently.

left_(direct, N, P) :- !,
	prime_whl:left_(N, W, Cert),
	left__do(direct, Cert, W, P).

left_(_, N, N) :-
	prime_mem:get_(N), !.
left_(Mode, N, P) :-
	prime_whl:left_(N, W, Cert),
	left__do(Mode, Cert, W, P).

left__do(_, Cert, W, W) :-
	test__do(Cert, W), !.
left__do(Mode, _, W0, P) :-
	W1 is W0 - 1,
	left_(Mode, W1, P).

%!	next_p_(+Mode:pmode, +P0:prime, -P:prime) is det.
%
%	P is the smallest prime number greater than P0.
%
%	Mode selects one of four variants:
%
%		* =direct= : does not use memoization
%		* =mem_read= : only reads memoization
%		* =mem_write= : reads and writes memoization
%		* =mem_sync= : reads and writes memoization with synchronization
%
%	Calls to this predicate are _not_ synchronized if Mode is =mem_write=.

next_p_(direct, P0, P) :-
	N is P0 + 1,
	right_(direct, N, P).

next_p_(mem_read, P0, P) :-
	prime_mem:get_(P0, P), !.
next_p_(mem_read, P0, P) :-
	next_p_(direct, P0, P).

next_p_(mem_write, P0, P) :-
	prime_mem:get_(P0, P), !.
next_p_(mem_write, P0, P) :-
	next_p_(direct, P0, P),
	prime_mem:add_(P0, P).

next_p_(mem_sync, P0, P) :-
	prime_mem:get_(P0, P), !.
next_p_(mem_sync, P0, P) :-
	next_p_(direct, P0, P),
	with_mutex('Nan.Numerics.Prime::mem_mutex', prime_mem:add_(P0, P)).

%!	prev_p_(+Mode:pmode, +P0:prime, -P:prime) is semidet.
%
%	P is the greatest prime number less than P0.  Fails if P0 is less than
%	or equal to =2=.
%
%	Mode selects one of four variants:
%
%		* =direct= : does not use memoization
%		* =mem_read= : only reads memoization
%		* =mem_write= : reads and writes memoization
%		* =mem_sync= : reads and writes memoization with synchronization
%
%	Calls to this predicate are _not_ synchronized if Mode is =mem_write=.

prev_p_(direct, P0, P) :-
	N is P0 - 1,
	left_(direct, N, P).

prev_p_(mem_read, P0, P) :-
	prime_mem:get_(P, P0), !.
prev_p_(mem_read, P0, P) :-
	prev_p_(direct, P0, P).

prev_p_(mem_write, P0, P) :-
	prime_mem:get_(P, P0), !.
prev_p_(mem_write, P0, P) :-
	prev_p_(direct, P0, P),
	prime_mem:add_(P, P0).

prev_p_(mem_sync, P0, P) :-
	prime_mem:get_(P, P0), !.
prev_p_(mem_sync, P0, P) :-
	prev_p_(direct, P0, P),
	with_mutex('Nan.Numerics.Prime::mem_mutex', prime_mem:add_(P, P0)).

%!	gen_p_(+Mode:pmode, +L:prime, -P:prime) is multi.
%!	gen_p_(+Mode:pmode, +L:prime, +Sup:posint, -P:prime) is nondet.
%
%	Generates in _ascending_ order all prime numbers P greater than or
%	equal to L, and less than or equal to Sup in the variant with arity
%	=4=. Fails if L is greater than Sup.
%
%	Mode selects one of four variants:
%
%		* =direct= : does not use memoization
%		* =mem_read= : only reads memoization
%		* =mem_write= : reads and writes memoization
%		* =mem_sync= : reads and writes memoization with synchronization
%
%	Calls to this predicate are _not_ synchronized if Mode is =mem_write=.

gen_p_(_, L, L).
gen_p_(Mode, L, P) :-
	next_p_(Mode, L, L1),
	gen_p_(Mode, L1, P).

gen_p_(Mode, L, Sup, P) :-
	L =< Sup,
	gen_p__do(Mode, L, Sup, P).

gen_p__do(_, H, H, H) :- !.
gen_p__do(_, L, _, L).
gen_p__do(Mode, L, Sup, P) :-
	next_p_(Mode, L, L1),
	gen_p_(Mode, L1, Sup, P).

%!	gen_rev_p_(+Mode:pmode, +Inf:posint, +H:prime, -P:prime) is nondet.
%
%	Generates in _descending_ order all prime numbers P greater than or
%	equal to L and less than or equal to H.  Fails if Inf is greater than
%	H.
%
%	Mode selects one of four variants:
%
%		* =direct= : does not use memoization
%		* =mem_read= : only reads memoization
%		* =mem_write= : reads and writes memoization
%		* =mem_sync= : reads and writes memoization with synchronization
%
%	Calls to this predicate are _not_ synchronized if Mode is =mem_write=.

gen_rev_p_(Mode, Inf, H, P) :-
	Inf =< H,
	gen_rev_p__do(Mode, Inf, H, P).

gen_rev_p__do(_, L, L, L) :- !.
gen_rev_p__do(_, _, H, H).
gen_rev_p__do(Mode, Inf, H, P) :-
	prev_p_(Mode, H, H1),
	gen_rev_p_(Mode, Inf, H1, P).

%!	next_(+Mode:pmode, +N:posint, -P:prime) is det.
%
%	P is the smallest prime number greater than N.
%
%	Mode selects one of four variants:
%
%		* =direct= : does not use memoization
%		* =mem_read= : only reads memoization
%		* =mem_write= : reads and writes memoization
%		* =mem_sync= : reads and writes memoization with synchronization
%
%	Calls to this predicate are _not_ synchronized if Mode is =mem_write=.

next_(Mode, N, P) :-
	right_(Mode, N, P0),
	next__do(Mode, N, P0, P).

next__do(Mode, P0, P0, P) :- !,
	next_p_(Mode, P0, P).
next__do(_, _, P0, P0).

%!	prev_(+Mode:pmode, +N:posint, -P:prime) is semidet.
%
%	P is the greatest prime number less than N.  Fails if N is less than or
%	equal to =2=.
%
%	Mode selects one of four variants:
%
%		* =direct= : does not use memoization
%		* =mem_read= : only reads memoization
%		* =mem_write= : reads and writes memoization
%		* =mem_sync= : reads and writes memoization with synchronization
%
%	Calls to this predicate are _not_ synchronized if Mode is =mem_write=.

prev_(Mode, N, P) :-
	left_(Mode, N, P0),
	prev__do(Mode, N, P0, P).

prev__do(Mode, P0, P0, P) :- !,
	prev_p_(Mode, P0, P).
prev__do(_, _, P0, P0).

%!	gen_(+Mode:pmode, +Inf:posint, -P:prime) is multi.
%!	gen_(+Mode:pmode, +Inf:posint, +Sup:posint, -P:prime) is nondet.
%
%	Generates in _ascending_ order all prime numbers P greater than or
%	equal to Inf, and less than or equal to Sup in the variant with arity
%	=4=.  Fails if Inf is greater than Sup.
%
%	Mode selects one of four variants:
%
%		* =direct= : does not use memoization
%		* =mem_read= : only reads memoization
%		* =mem_write= : reads and writes memoization
%		* =mem_sync= : reads and writes memoization with synchronization
%
%	Calls to this predicate are _not_ synchronized if Mode is =mem_write=.

gen_(Mode, Inf, P) :-
	right_(Mode, Inf, L),
	gen_p_(Mode, L, P).

gen_(Mode, Inf, Sup, P) :-
	right_(Mode, Inf, L),
	gen_p_(Mode, L, Sup, P).

%!	gen_rev_(+Mode:pmode, +Inf:posint, +Sup:posint, -P:prime) is nondet.
%
%	Generates in _descending_ order all prime numbers P greater than or
%	equal to Inf and less than or equal to Sup.  Fails if Inf is greater
%	than Sup.
%
%	Mode selects one of four variants:
%
%		* =direct= : does not use memoization
%		* =mem_read= : only reads memoization
%		* =mem_write= : reads and writes memoization
%		* =mem_sync= : reads and writes memoization with synchronization
%
%	Calls to this predicate are _not_ synchronized if Mode is =mem_write=.

gen_rev_(Mode, Inf, Sup, P) :-
	left_(Mode, Sup, H),
	gen_rev_p_(Mode, Inf, H, P).

%!	div_(+Mode:pmode, +N:posint, -P:prime) is semidet.
%!	div_(+Mode:pmode, +N:posint, +Inf:posint, -P:prime) is semidet.
%
%	True if N is a composite number with smallest prime factor P.
%	Tests only prime factors greater than or equal to Inf in the variant
%	with arity =4=.
%
%	Mode selects one of four variants:
%
%		* =direct= : does not use memoization
%		* =mem_read= : only reads memoization
%		* =mem_write= : reads and writes memoization
%		* =mem_sync= : reads and writes memoization with synchronization
%
%	Calls to this predicate are _not_ synchronized if Mode is =mem_write=.

div_(Mode, N, P) :-
	div_(Mode, N, 2, P).

div_(Mode, N, Inf, P) :-
	\+ test_(Mode, N),
	div__do(Mode, N, Inf, P).

%!	div_rev_(+Mode:pmode, +N:posint, -P:prime) is semidet.
%!	div_rev_(+Mode:pmode, +N:posint, +Sup:posint, -P:prime) is semidet.
%
%	True if N is a composite number with greatest prime factor P.
%	Tests only prime factors less than or equal to Sup in the variant
%	with arity =4=.
%
%	Mode selects one of four variants:
%
%		* =direct= : does not use memoization
%		* =mem_read= : only reads memoization
%		* =mem_write= : reads and writes memoization
%		* =mem_sync= : reads and writes memoization with synchronization
%
%	Calls to this predicate are _not_ synchronized if Mode is =mem_write=.

div_rev_(Mode, N, P) :-
	div_rev_(Mode, N, N, P).

div_rev_(Mode, N, Sup, P) :-
	\+ test_(Mode, N),
	div_rev__do(Mode, N, Sup, P).

%!	fact_(+Mode:pmode, +N:posint, -PFs:list(pfact)) is det.
%
%	PFs is the list of all prime divisors of N in _ascending_ order of
%	prime factors.  Elements of PFs  are of the form =|P^F|= where _P_ is
%	the factor and _F_ is the exponent of the divisor.  PFs is the
%	singleton =|[N^1]|= if N is equal to =1= or N is a prime number.
%
%	Mode selects one of four variants:
%
%		* =direct= : does not use memoization
%		* =mem_read= : only reads memoization
%		* =mem_write= : reads and writes memoization
%		* =mem_sync= : reads and writes memoization with synchronization
%
%	Calls to this predicate are _not_ synchronized if Mode is =mem_write=.

fact_(_, 1, [1^1]) :- !.
fact_(Mode, N, PFs) :-
	fact__do(Mode, N, 2, PFs).

fact__do(Mode, N, P0, [P^F| PFs]) :-
	div_(Mode, N, P0, P), !,
	N0 is N // P,
	fact__div(P, N0, 1, F, N1),
	fact__do(Mode, N1, P, PFs).
fact__do(_, 1, _, []) :- !.
fact__do(_, N, _, [N^1]).

fact__div(P, N0, F0, F, N) :-
	N0 mod P =:= 0, !,
	N1 is N0 // P,
	F1 is F0 + 1,
	fact__div(P, N1, F1, F, N).
fact__div(_, N0, F0, F0, N0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	test__do(+N:posint) is semidet.

test__do(N) :-
	prime_prb:test_(N, _).

%	div__do(+N:posint, +Inf:posint, -P:prime) is semidet.

div__do(Mode, N, Inf, P) :-
	% Sup is isqrt(N),			% TODO: Integrate =isqrt= function from GMP?
	Sup is N,					%
	gen_(Mode, Inf, Sup, P),
	% N mod P =:= 0, !.			%
	(	P * P > N, !, fail		%
	;	N mod P =:= 0, !		%
	).							%

%	div_rev__do(+N:posint, +Sup:posint, -P:prime) is semidet.

div_rev__do(Mode, N, Sup, P) :-
	Sup1 is min(N >> 1, Sup),
	gen_rev_(Mode, 2, Sup1, P),
	N mod P =:= 0, !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
