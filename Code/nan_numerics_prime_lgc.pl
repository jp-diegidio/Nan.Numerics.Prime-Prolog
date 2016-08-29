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
	test_/1,		% +N:posint
	div_/2,			% +N:posint, -P:prime
	div_rev_/2,		% +N:posint, -P:prime
	fact_/2,		% +N:posint, -PFs:list(pfact)
	gen_/2,			% +Inf:posint, -P:prime
	gen_/3,			% +Inf:posint, +Sup:posint, -P:prime
	gen_p_/2,		% +L:prime, -P:prime
	gen_p_/3,		% +L:prime, +H:prime, -P:prime
	gen_rev_/2,		% +Sup:posint, -P:prime
	gen_rev_/3,		% +Inf:posint, +Sup:posint, -P:prime
	gen_rev_p_/2,	% +H:prime, -P:prime
	gen_rev_p_/3,	% +L:prime, +H:prime, -P:prime
	next_/2,		% +N:posint, -P:prime
	next_p_/2,		% +P0:prime, -P:prime
	prev_/2,		% +N:posint, -P:prime
	prev_p_/2,		% +P0:prime, -P:prime
	right_/2,		% +N:posint, -P:prime
	left_/2.		% +N:posint, -P:prime

/** <module> A simple prime number library :: logic

To allow for maximum performance, module =prime_lgc= provides _unsafe_
=public= (not exported) predicates that user code can call directly instead
of calling the _safe_ predicates exported by module =prime=.

For info on the implementation, see library(nan_numerics_prime).

*NOTE*: Predicates in this module are _unsafe_, i.e. do not validate input
arguments and are not steadfast.

@author		Julio P. Di Egidio
@version	1.2.3-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
@see		library(nan_numerics_prime)
@tbd		Integrate =isqrt= function from GMP?
*/

:- use_module(nan_numerics_prime_mem).
:- use_module(nan_numerics_prime_whl).
:- use_module(nan_numerics_prime_prb).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	test_(+N:posint) is semidet.
%
%	True if N is a prime number.

test_(N) :-
	prime_mem:get_(N), !.
test_(N) :-
	prime_whl:test_(N, Cert),
	test__do(Cert, N).

%!	div_(+N:posint, -P:prime) is semidet.
%
%	True if N is a composite number with P its smallest prime divisor.

div_(N, P) :-
	N > 3,
	\+ test_(N),
	div__do(N, P).

%!	div_rev_(+N:posint, -P:prime) is semidet.
%
%	True if N is a composite number with P its greatest prime divisor.

div_rev_(N, P) :-
	N > 3,
	\+ test_(N),
	div_rev__do(N, P).

%!	fact_(+N:posint, -PFs:list(pfact)) is det.
%
%	PFs is the list of all prime factors of N in _ascending_ order of the
%	prime divisors.
%
%	Elements of PFs are of the form =|P^F|= with _P_ the prime divisor and
%	_F_ the corresponding power.
%
%	If N is equal to =1= or if N is a prime number, PFs is =|[N^1]|=.

fact_(N, PFs) :-
	fact__sel_rev(N, [], PFsRev),
	reverse(PFsRev, PFs).

fact__sel_rev(N, PFs0, PFs) :-
	div_(N, P), !,
	N1 is N // P,
	fact__pfs(P, PFs0, PFs1),
	fact__sel_rev(N1, PFs1, PFs).
fact__sel_rev(P, PFs0, PFs) :-
	fact__pfs(P, PFs0, PFs).

fact__pfs(P, [P^F| PFs], [P^F1| PFs]) :- !, F1 is F + 1.
fact__pfs(P, PFs, [P^1| PFs]).

%!	gen_(+Inf:posint, -P:prime) is multi.
%!	gen_(+Inf:posint, +Sup:posint, -P:prime) is nondet.
%
%	Generates in _ascending_ order all prime numbers P greater than or
%	equal to Inf, and less than or equal to Sup in the variant with arity
%	=3=.  Fails if the prime to the left of Sup is less than the prime to
%	the right of Inf.

gen_(Inf, P) :-
	right_(Inf, L),
	gen_p_(L, P).

gen_(Inf, Sup, P) :-
	gen__lh(Inf, Sup, L, H),
	gen_p_(L, H, P).

gen__lh(Inf, Sup, L, H) :-
	Sup >= Inf,
	right_(Inf, L),
	left_(Sup, H),
	H >= L.

%!	gen_p_(+L:prime, -P:prime) is multi.
%!	gen_p_(+L:prime, +H:prime, -P:prime) is nondet.
%
%	Generates in _ascending_ order all prime numbers P starting from L, and
%	up to H in the variant with arity =3=.  Fails if H is less than L.

gen_p_(P, P).
gen_p_(L, P) :-
	next_p_(L, L1),
	gen_p_(L1, P).

gen_p_(P, H, P) :- P >= H, !.
gen_p_(P, _, P).
gen_p_(L, H, P) :-
	next_p_(L, L1),
	gen_p_(L1, H, P).

%!	gen_rev_(+Sup:prime, -P:prime) is nondet.
%!	gen_rev_(+Inf:posint, +Sup:posint, -P:prime) is nondet.
%
%	Generates in _descending_ order all prime numbers P less than or equal
%	to Sup, and greater than or equal to Inf in the variant with arity =3=.
%	Fails if Sup is equal to =1= or if the prime to the left of Sup is less
%	than the prime to the right of Inf.

gen_rev_(Sup, P) :-
	left_(Sup, H),
	gen_rev_p_(H, P).

gen_rev_(Inf, Sup, P) :-
	gen__lh(Inf, Sup, L, H),
	gen_rev_p_(L, H, P).

%!	gen_rev_p_(+H:prime, -P:prime) is multi.
%!	gen_rev_p_(+L:prime, +H:prime, -P:prime) is nondet.
%
%	Generates in _descending_ order all prime numbers P starting from H,
%	and down to L in the variant with arity =3=.  Fails if H is less than
%	L.

gen_rev_p_(2, 2) :- !.
gen_rev_p_(P, P).
gen_rev_p_(H, P) :-
	prev_p_(H, H1),
	gen_rev_p_(H1, P).

gen_rev_p_(L, P, P) :- P =< L, !.
gen_rev_p_(_, P, P).
gen_rev_p_(L, H, P) :-
	prev_p_(H, H1),
	gen_rev_p_(L, H1, P).

%!	next_(+N:posint, -P:prime) is det.
%
%	P is the smallest prime number greater than N.

next_(N, P) :-
	right_(N, P0),
	next__sel(N, P0, P).

next__sel(P0, P0, P) :- !,
	next_p_(P0, P).
next__sel(_, P, P).

%!	next_p_(+P0:prime, -P:prime) is det.
%
%	P is the smallest prime number greater than P0.

next_p_(P0, P) :-
	prime_mem:get_(P0, P), !.
next_p_(P0, P) :-
	N is P0 + 1,
	right_(N, P),
	prime_mem:add_(P0, P).

%!	prev_(+N:posint, -P:prime) is semidet.
%
%	P is the greatest prime number less than N.  Fails if N is less than or
%	equal to =2=.

prev_(N, P) :-
	left_(N, P0),
	prev__sel(N, P0, P).

prev__sel(P0, P0, P) :- !,
	prev_p_(P0, P).
prev__sel(_, P, P).

%!	prev_p_(+P0:prime, -P:prime) is semidet.
%
%	P is the greatest prime number less than P0.  Fails if P is equal to
%	=2=.

prev_p_(P0, P) :-
	prime_mem:get_(P, P0), !.
prev_p_(P0, P) :-
	N is P0 - 1,
	left_(N, P),
	prime_mem:add_(P, P0).

%!	right_(+N:posint, -P:prime) is det.
%
%	P is the smallest prime number greater than or equal to N.

right_(N, P) :-
	prime_whl:right_(N, W, Cert),
	right__sel(Cert, W, P).

right__sel(Cert, P, P) :-
	test__do(Cert, P), !.
right__sel(_, W, P) :-
	W1 is W + 2,
	right_(W1, P).

%!	left_(+N:posint, -P:prime) is semidet.
%
%	P is the greatest prime number less than or equal to N.  Fails if N is
%	equal to =1=.

left_(N, P) :-
	prime_whl:left_(N, W, Cert),
	left__sel(Cert, W, P).

left__sel(Cert, P, P) :-
	test__do(Cert, P), !.
left__sel(_, W, P) :-
	W1 is W - 2,
	left_(W1, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	test__do(+Cert:boolean, +N:posint) is semidet.

test__do(true, _) :- !.
test__do(_, N) :-
	prime_prb:test_(N, _).

%	div__do(+N:posint, -P:prime) is semidet.

div__do(N, P) :-
	% Sup is floor(sqrt(N)),		% TODO: Integrate =isqrt= function from GMP?
	gen_p_(2, P),
	% (	P > Sup, !, fail			% 
	(	P * P > N, !, fail			% 
	;	N mod P =:= 0
	), !.

%	div_rev__do(+N:posint, -P:prime) is semidet.

div_rev__do(N, P) :-
	Sup is N // 2,
	left_(Sup, H),
	gen_rev_p_(H, P),
	N mod P =:= 0, !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
