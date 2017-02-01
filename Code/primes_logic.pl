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

:- module(primes_logic,
	[	prime_lgc_test/1,       % +N:posint
		prime_lgc_right/2,      % +N:posint, -P:prime
		prime_lgc_left/2,       % +N:posint, -P:prime
		prime_lgc_next/2,       % +N:posint, -P:prime
		prime_lgc_next_p/2,     % +P0:prime, -P:prime
		prime_lgc_prev/2,       % +N:posint, -P:prime
		prime_lgc_prev_p/2,     % +P0:prime, -P:prime
		prime_lgc_gen/2,        % +Inf:posint, -P:prime
		prime_lgc_gen/3,        % +Inf:posint, +Sup:posint, -P:prime
		prime_lgc_gen_p/2,      % +L:prime, -P:prime
		prime_lgc_gen_p/3,      % +L:prime, +Sup:posint, -P:prime
		prime_lgc_gen_rev/2,    % +Sup:posint, -P:prime
		prime_lgc_gen_rev/3,    % +Inf:posint, +Sup:posint, -P:prime
		prime_lgc_gen_rev_p/2,  % +H:prime, -P:prime
		prime_lgc_gen_rev_p/3,  % +Inf:posint, +H:prime, -P:prime
		prime_lgc_div/2,        % +N:posint, -P:prime
		prime_lgc_div/3,        % +N:posint, +Inf:posint, -P:prime
		prime_lgc_div_p/3,      % +N:posint, +L:prime, -P:prime
		prime_lgc_div_rev/2,    % +N:posint, -P:prime
		prime_lgc_div_rev/3,    % +N:posint, +Sup:posint, -P:prime
		prime_lgc_div_rev_p/3,  % +N:posint, +H:prime, -P:prime
		prime_lgc_fact/2        % +N:posint, -Fs:list(pfact)
	]).

:- use_module(primes_wheel).
:- use_module(primes_probabilistic).

/** <module> A simple prime number library :: Logic

*|Library Nan.Numerics.Prime (nan_numerics_prime.pl)|*

Module =primes_logic= (nan_numerics_primes_logic.pl)
provides low-level predicates to test positive integer numbers for
primality, find consecutive prime numbers, generate prime numbers in some
interval, find divisors and factor numbers.

Predicates that only read the state can be safely called concurrently.
Calls to predicates that can write the state are _not_ synchronized if
_Mode_ is =mem_write=.

*NOTE*: Predicates in this module are _unsafe_, i.e. do not validate input
arguments and are not steadfast.

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_lgc_test(+N:posint) is semidet.
%
%	True if N is a prime number.

prime_lgc_test(N) :-
	prime_whl_test(N, Cert),
	test__sel(Cert, N).

test__sel(true, _) :- !.
test__sel(_, N) :-
	prime_prb_test(N, _).

%!	prime_lgc_right(+N:posint, -P:prime) is det.
%
%	P is the smallest prime number greater than or equal to N.

prime_lgc_right(N, P) :-
	prime_whl_right(N, W, Cert),
	right__sel(Cert, W, P).

right__sel(Cert, W, W) :-
	test__sel(Cert, W), !.
right__sel(_, W0, P) :-
	W1 is W0 + 1,
	prime_lgc_right(W1, P).

%!	prime_lgc_left(+N:posint, -P:prime) is semidet.
%
%	P is the greatest prime number less than or equal to N.
%	
%	Fails if N is equal to =1=.

prime_lgc_left(N, P) :-
	prime_whl_left(N, W, Cert),
	left__sel(Cert, W, P).

left__sel(Cert, W, W) :-
	test__sel(Cert, W), !.
left__sel(_, W0, P) :-
	W1 is W0 - 1,
	prime_lgc_left(W1, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_lgc_next(+N:posint, -P:prime) is det.
%
%	P is the smallest prime number greater than N.

prime_lgc_next(N, P) :-
	prime_lgc_right(N, P0),
	next__sel(N, P0, P).

next__sel(P0, P0, P) :- !,
	prime_lgc_next_p(P0, P).
next__sel(_, P0, P0).

%!	prime_lgc_next_p(+P0:prime, -P:prime) is det.
%
%	P is the smallest prime number greater than P0.

prime_lgc_next_p(P0, P) :-
	N is P0 + 1,
	prime_lgc_right(N, P).

%!	prime_lgc_prev(+N:posint, -P:prime) is semidet.
%
%	P is the greatest prime number less than N.
%	
%	Fails if N is less than or equal to =2=.

prime_lgc_prev(N, P) :-
	prime_lgc_left(N, P0),
	prev__sel(N, P0, P).

prev__sel(P0, P0, P) :- !,
	prime_lgc_prev_p(P0, P).
prev__sel(_, P0, P0).

%!	prime_lgc_prev_p(+P0:prime, -P:prime) is semidet.
%
%	P is the greatest prime number less than P0.
%	
%	Fails if P0 is less than or equal to =2=.

prime_lgc_prev_p(P0, P) :-
	N is P0 - 1,
	prime_lgc_left(N, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_lgc_gen(+Inf:posint, -P:prime) is multi.
%
%	Generates in _ascending_ order all prime numbers P greater than or
%	equal to Inf.

prime_lgc_gen(Inf, P) :-
	prime_lgc_right(Inf, L),
	prime_lgc_gen_p(L, P).

%!	prime_lgc_gen(+Inf:posint, +Sup:posint, -P:prime) is nondet.
%
%	Generates in _ascending_ order all prime numbers P greater than or
%	equal to Inf and less than or equal to Sup.
%	
%	Fails if there are no prime numbers between Inf and Sup.

prime_lgc_gen(Inf, Sup, P) :-
	prime_lgc_right(Inf, L),
	prime_lgc_gen_p(L, Sup, P).

%!	prime_lgc_gen_p(+L:prime, -P:prime) is multi.
%
%	Generates in _ascending_ order all prime numbers P greater than or
%	equal to L.

prime_lgc_gen_p(L, L).
prime_lgc_gen_p(L, P) :-
	prime_lgc_next_p(L, L1),
	prime_lgc_gen_p(L1, P).

%!	prime_lgc_gen_p(+L:prime, +Sup:posint, -P:prime) is nondet.
%
%	Generates in _ascending_ order all prime numbers P greater than or
%	equal to L and less than or equal to Sup.
%	
%	Fails if there are no prime numbers between L and Sup.

prime_lgc_gen_p(L, Sup, P) :-
	L =< Sup,
	gen_p__sel(L, Sup, P).

gen_p__sel(H, H, H) :- !.
gen_p__sel(L, _, L).
gen_p__sel(L, Sup, P) :-
	prime_lgc_next_p(L, L1),
	prime_lgc_gen_p(L1, Sup, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_lgc_gen_rev(+Sup:posint, -P:prime) is nondet.
%
%	Generates in _descending_ order all prime numbers P less than or
%	equal to Sup.
%	
%	Fails if there are no prime numbers less than or equal to Sup.

prime_lgc_gen_rev(Sup, P) :-
	prime_lgc_left(Sup, H),
	prime_lgc_gen_rev_p(H, P).

%!	prime_lgc_gen_rev(+Inf:posint, +Sup:posint, -P:prime) is nondet.
%
%	Generates in _descending_ order all prime numbers P greater than or
%	equal to Inf and less than or equal to Sup.
%	
%	Fails if there are no prime numbers between Inf and Sup.

prime_lgc_gen_rev(Inf, Sup, P) :-
	prime_lgc_left(Sup, H),
	prime_lgc_gen_rev_p(Inf, H, P).

%!	prime_lgc_gen_rev_p(+H:prime, -P:prime) is nondet.
%
%	Generates in _descending_ order all prime numbers P less than or
%	equal to H, and greater than or equal to =2=.
%	
%	Fails if there are no prime numbers between =2= and H.

prime_lgc_gen_rev_p(H, H).
prime_lgc_gen_rev_p(H, P) :-
	prime_lgc_prev_p(H, H1),
	prime_lgc_gen_rev_p(H1, P).

%!	prime_lgc_gen_rev_p(+Inf:posint, +H:prime, -P:prime) is nondet.
%
%	Generates in _descending_ order all prime numbers P greater than or
%	equal to Inf and less than or equal to H.
%	
%	Fails if there are no prime numbers between Inf and H.

prime_lgc_gen_rev_p(Inf, H, P) :-
	Inf =< H,
	gen_rev_p__sel(Inf, H, P).

gen_rev_p__sel(L, L, L) :- !.
gen_rev_p__sel(_, H, H).
gen_rev_p__sel(Inf, H, P) :-
	prime_lgc_prev_p(H, H1),
	prime_lgc_gen_rev_p(Inf, H1, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_lgc_div(+N:posint, -P:prime) is semidet.
%
%	True if N is a composite number with smallest prime factor P.

prime_lgc_div(N, P) :-
	% Test in [2; sqrt(N)]
	\+ prime_lgc_test(N),		% TODO: Only good for deterministic!? #####
	nth_integer_root_and_remainder(2, N, Sup, _),
	prime_lgc_gen(2, Sup, P),
	N mod P =:= 0, !.

%!	prime_lgc_div(+N:posint, +Inf:posint, -P:prime) is semidet.
%
%	True if N is a composite number with smallest prime factor P.
%	
%	Tests only prime factors greater than or equal to Inf.

prime_lgc_div(N, Inf, P) :-
	% Test in [Inf; N//2]
	\+ prime_lgc_test(N),		% TODO: Only good for deterministic!? #####
	Sup is N >> 1,
	prime_lgc_gen(Inf, Sup, P),
	N mod P =:= 0, !.

%!	prime_lgc_div_p(+N:posint, +L:prime, -P:prime) is semidet.
%
%	True if N is a composite number with smallest prime factor P.
%	
%	Tests only prime factors greater than or equal to L.

prime_lgc_div_p(N, L, P) :-
	% Test in [L; N//2]
	\+ prime_lgc_test(N),		% TODO: Only good for deterministic!? #####
	Sup is N >> 1,
	prime_lgc_gen_p(L, Sup, P),
	N mod P =:= 0, !.

%!	prime_lgc_div_rev(+N:posint, -P:prime) is semidet.
%
%	True if N is a composite number with greatest prime factor P.

prime_lgc_div_rev(N, P) :-
	% Test in [N//2; 2] (rev)
	\+ prime_lgc_test(N),		% TODO: Only good for deterministic!? #####
	Sup is N >> 1,
	prime_lgc_gen_rev(Sup, P),
	N mod P =:= 0, !.

%!	prime_lgc_div_rev(+N:posint, +Sup:posint, -P:prime) is semidet.
%
%	True if N is a composite number with greatest prime factor P.
%	
%	Tests only prime factors less than or equal to Sup.

prime_lgc_div_rev(N, Sup, P) :-
	% Test in [min(Sup, N//2); 2] (rev)
	\+ prime_lgc_test(N),		% TODO: Only good for deterministic!? #####
	Sup1 is min(Sup, N >> 1),
	prime_lgc_gen_rev(Sup1, P),
	N mod P =:= 0, !.

%!	prime_lgc_div_rev_p(+N:posint, +H:posint, -P:prime) is semidet.
%
%	True if N is a composite number with greatest prime factor P.
%	
%	Tests only prime factors less than or equal to H.

prime_lgc_div_rev_p(N, H, P) :-
	% Test in [min(H, N//2); 2] (rev)
	\+ prime_lgc_test(N),		% TODO: Only good for deterministic!? #####
	Sup1 is min(H, N >> 1),
	prime_lgc_gen_rev(Sup1, P),
	N mod P =:= 0, !.

%!	prime_lgc_fact(+N:posint, -Fs:list(pfact)) is det.
%
%	Fs is a list of all prime factors of N and corresponding exponents
%	in _ascending_ order of the factors.
%	
%	Elements of Fs are terms of the form =|s(P:prime^Exp:posint)|=.
%	
%	If N is equal to =1= or N is a prime number, Fs is =|[N^1]|=.

prime_lgc_fact(1, [1^1]) :- !.
prime_lgc_fact(N, Fs) :-
	fact__do(N, 2, Fs).

fact__do(N, L, [P^E| Fs]) :-
	prime_lgc_div(N, L, P), !,
	N1 is N // P,
	fact__red(P, N1, 1, N2, E),
	fact__do(N2, P, Fs).
fact__do(1, _, []) :- !.
fact__do(N, _, [N^1]).

fact__red(P, N0, E0, N, E) :-
	N0 mod P =:= 0, !,
	N1 is N0 // P,
	E1 is E0 + 1,
	fact__red(P, N1, E1, N, E).
fact__red(_, N0, E0, N0, E0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
