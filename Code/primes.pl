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

% TODO: Implement parallel factoring functions.
% TODO: Implement probabilitic test error estimates?
% TODO: Implement option for num. of probabilistic iterations?
% TODO: Implement prime counting/n-th prime functions.
% TODO: Implement deterministic tests?
% TODO: Improve compatibility with other Prolog systems.

:- module(primes,
	[	prime_test/1,         % +N
		prime_right/2,        % +N, ?P
		prime_left/2,         % +N, ?P
		prime_next/2,         % +N, ?P
		prime_prev/2,         % +N, ?P
		prime_gen/2,          % +Inf, ?P
		prime_gen/3,          % +Inf, +Sup, ?P
		prime_gen_rev/2,      % +Sup, ?P
		prime_gen_rev/3,      % +Inf, +Sup, ?P
		prime_div/2,          % +N, ?P
		prime_div/3,          % +N, +Inf, ?P
		prime_div_rev/2,      % +N, ?P
		prime_div_rev/3,      % +N, +Sup, ?P
		prime_fact/2,         % +N, ?Fs
		prime_prb_det_max/1,  % ?Max
		prime_prb_rep_def/1,  % ?Mul
		prime_whl_level/1,    % ?Lev
		prime_whl_det_max/1   % ?Max
	]).

:- use_module(primes_wheel,
	[	prime_whl_level/1   as prime_whl_level__,
		prime_whl_det_max/1 as prime_whl_det_max__
	]).
:- use_module(primes_probabilistic,
	[	prime_prb_det_max/1 as prime_prb_det_max__,
		prime_prb_rep_def/1 as prime_prb_rep_def__
	]).
:- use_module(primes_logic).

:- use_module(library(error)).

:- multifile
	error:has_type/2.

/** <module> A Simple Prime Number Library

Part of *|Nan.Numerics.Primes|* (nan/numerics/primes.pl)

This library implements a variant of the *Miller-Rabin* primality test that
is _deterministic_ for numbers up to =3317044064679887385961980=, otherwise
it is _probabilistic_ with the number of iterations fixed at =20=.  For
better performance, leverages a configurable prime wheel and optional
memoization of pairs of consecutive prime numbers.

*NOTE*: Since the primality test in use is _probabilistic_ in general, this
library is not suitable for cryptographic applications.

Module =prime= (nan_numerics_prime.pl)
provides predicates to test positive integer numbers for primality, find
consecutive prime numbers, generate prime numbers in some interval, find
divisors and factor numbers.  Also provides predicates to control the
underlying prime wheel and memoization, and predicates to save/load
comma-separated lists of prime numbers in some interval to/from a file or
stream.

*NOTE*: At initialization time, the prime wheel is set to level =6=.

*NOTE*: For maximum performance, predicates in this module are _not_
synchronized.  See the documentation accompanying each module and predicate
in this library for synchronization details.

*NOTE*: All predicates in this module are _safe_, i.e. validate input
arguments and ensure steadfastness.  For maximum performance, user code can
directly call the _unsafe_ =public= (not exported) predicates provided by
the following sub-modules:

$ Module =prime_lgc= (nan_numerics_prime_lgc.pl) :
provides low-level predicates to test positive integer numbers for
primality, find consecutive prime numbers, generate prime numbers in some
interval, find divisors and factor numbers.

$ Module =prime_prb= (nan_numerics_prime_prb.pl) :
provides low-level predicates to test candidate primality of positive
integer numbers based on a variant of the *|Miller-Rabin|* test -- as
explained above.

$ Module =prime_whl=(nan_numerics_prime_whl.pl) :
provides low-level predicates to test candidate primality of numbers and
find contiguous candidates based on a prime wheel generated by the first
_Lev_ prime numbers, where _Lev_ is the configurable level of the wheel.

$ Module =prime_mem= (nan_numerics_prime_mem.pl) :
provides low-level predicates for the memoization of pairs of consecutive
prime numbers.

$ Module =prime_pio= (nan_numerics_prime_pio.pl) :
provides low-level predicates to read/write comma-separated lists of
positive integer numbers from/to a file or stream.

Usage example:
--------------
==
?- pack_install(nan_numerics_prime).
true.

?- use_module(library(nan_numerics_prime)).
true.

?- time(prime_right(1234567891012345678901234567890123456789011111, P)).
% 1,227 inferences, 0.000 CPU in 0.010 seconds (0% CPU, Infinite Lips)
P = 1234567891012345678901234567890123456789011139.

?- time(prime_lgc_right(1234567891012345678901234567890123456789011111, P)).
% 1,227 inferences, 0.000 CPU in 0.010 seconds (0% CPU, Infinite Lips)
P = 1234567891012345678901234567890123456789011139.
==

This library was developed and tested with:
SWI-Prolog 7.3.25 - http://www.swi-prolog.org/

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016-2017 Julio P. Di Egidio
@license	GNU GPLv3

@tbd		Implement parallel factoring functions.
@tbd		Implement probabilitic test error estimates?
@tbd		Implement option for num. of probabilistic iterations?
@tbd		Implement prime counting/n-th prime functions.
@tbd		Implement deterministic tests?
@tbd		Improve compatibility with other Prolog systems.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_test(+N:posint) is semidet.
%
%	True if N is a prime number.
%	
%	@error  type_error(posint, N)

prime_test(N) :-
	must_be(posint, N),
	prime_lgc_test(N).

%!	prime_right(+N:posint, ?P:prime) is det.
%
%	P is the smallest prime number greater than or equal to N.
%	
%	@error  type_error(posint, N)

prime_right(N, P) :-
	must_be(posint, N),
	prime_lgc_right(N, P_), P = P_.

%!	prime_left(+N:posint, ?P:prime) is semidet.
%
%	P is the greatest prime number less than or equal to N.
%	
%	Fails if N is equal to =1=.
%	
%	@error  type_error(posint, N)

prime_left(N, P) :-
	must_be(posint, N),
	prime_lgc_left(N, P_), P = P_.

%!	prime_next(+N:posint, ?P:prime) is det.
%
%	P is the smallest prime number greater than N.
%	
%	@error  type_error(posint, N)

prime_next(N, P) :-
	must_be(posint, N),
	prime_lgc_next(N, P_), P = P_.

%!	prime_prev(+N:posint, ?P:prime) is semidet.
%
%	P is the greatest prime number less than N.
%	
%	Fails if N is less than or equal to =2=.
%	
%	@error  type_error(posint, N)

prime_prev(N, P) :-
	must_be(posint, N),
	prime_lgc_prev(N, P_), P = P_.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_gen(+Inf:posint, ?P:prime) is nondet.
%
%	Generates in _ascending_ order all prime numbers P greater than or
%	equal to Inf.
%	
%	@error  type_error(posint, Inf)

prime_gen(Inf, P) :-
	must_be(posint, Inf),
	prime_lgc_gen(Inf, P_), P = P_.

%!	prime_gen(+Inf:posint, +Sup:posint, ?P:prime) is nondet.
%
%	Generates in _ascending_ order all prime numbers P greater than or
%	equal to Inf and less than or equal to Sup.
%	
%	Fails if there are no prime numbers between Inf and Sup.
%	
%	@error  type_error(posint, Inf)
%	@error  type_error(posint, Sup)

prime_gen(Inf, Sup, P) :-
	must_be(posint, Inf),
	must_be(posint, Sup),
	prime_lgc_gen(Inf, Sup, P_), P = P_.

%!	prime_gen_rev(+Sup:posint, ?P:prime) is nondet.
%
%	Generates in _descending_ order all prime numbers P less than or
%	equal to Sup.
%	
%	Fails if there are no prime numbers less than or equal to Sup.
%	
%	@error  type_error(posint, Sup)

prime_gen_rev(Sup, P) :-
	must_be(posint, Sup),
	prime_lgc_gen_rev(1, Sup, P_), P = P_.

%!	prime_gen_rev(+Inf:posint, +Sup:posint, ?P:prime) is nondet.
%
%	Generates in _descending_ order all prime numbers P greater than or
%	equal to Inf and less than or equal to Sup.
%	
%	Fails if there are no prime numbers between Inf and Sup.
%	
%	@error  type_error(posint, Inf)
%	@error  type_error(posint, Sup)

prime_gen_rev(Inf, Sup, P) :-
	must_be(posint, Inf),
	must_be(posint, Sup),
	prime_lgc_gen_rev(Inf, Sup, P_), P = P_.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_div(+N:posint, ?P:prime) is semidet.
%
%	True if N is a composite number with smallest prime factor P.
%	
%	@error  type_error(posint, N)

prime_div(N, P) :-
	must_be(posint, N),
	prime_lgc_div(N, P_), P = P_.

%!	prime_div(+N:posint, +Inf:posint, ?P:prime) is semidet.
%
%	True if N is a composite number with smallest prime factor P.
%	
%	Tests only prime factors greater than or equal to Inf.
%	
%	@error  type_error(posint, N)
%	@error  type_error(posint, Inf)

prime_div(N, Inf, P) :-
	must_be(posint, N),
	must_be(posint, Inf),
	prime_lgc_div(N, Inf, P_), P = P_.

%!	prime_div_rev(+N:posint, ?P:prime) is semidet.
%
%	True if N is a composite number with greatest prime factor P.
%	
%	@error  type_error(posint, N)

prime_div_rev(N, P) :-
	must_be(posint, N),
	prime_lgc_div_rev(N, P_), P = P_.

%!	prime_div_rev(+N:posint, +Sup:posint, ?P:prime) is semidet.
%
%	True if N is a composite number with greatest prime factor P.
%	
%	Tests only prime factors less than or equal to Sup.
%	
%	@error  type_error(posint, N)
%	@error  type_error(posint, Sup)

prime_div_rev(N, Sup, P) :-
	must_be(posint, N),
	must_be(posint, Sup),
	prime_lgc_div_rev(N, Sup, P_), P = P_.

%!	prime_fact(+N:posint, ?Fs:list(pfact)) is det.
%
%	Fs is a list of all prime factors of N and corresponding exponents
%	in _ascending_ order of the factors.
%	
%	Elements of Fs are of the form =|s(P:prime^Exp:posint)|=.
%	
%	If N is equal to =1= or N is a prime number, Fs is =|[N^1]|=.
%	
%	@error  type_error(posint, N)

prime_fact(N, Fs) :-
	must_be(posint, N),
	prime_lgc_fact(N, Fs_), Fs = Fs_.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_prb_det_max(?Max:posint) is det.
%
%	Max is the maximum number for which the test is deterministic.
%
%	This value is fixed and equal to =3317044064679887385961980=.

prime_prb_det_max(Max) :-
	prime_prb_det_max__(Max_), Max = Max_.

%!	prime_prb_rep_def(?Rep:posint) is det.
%
%	Rep is the default number of iterations for the probabilistic test.
%
%	This value is fixed and equal to =20=.

prime_prb_rep_def(Rep) :-
	prime_prb_rep_def__(Rep_), Rep = Rep_.

%!	prime_whl_level(?Lev:nonneg) is det.
%
%	Lev is the current level of the wheel.

prime_whl_level(Lev) :-
	prime_whl_level__(Lev_), Lev = Lev_.

%!	prime_whl_det_max(?Max:posint) is det.
%
%	Max is the maximum number for which the test is deterministic.

prime_whl_det_max(Max) :-
	prime_whl_det_max__(Max_), Max = Max_.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	error:has_type(+Type:atom, @Term:any) is semidet.
%
%	True if Term satisfies Type.
%
%	Extends library(error) with the following types:
%
%	| =prime=	| Prime number |
%	| =pfact=	| =|s(P:prime^Exp:posint)|= |
%	| =posint=	| =positive_integer= |

error:has_type(prime, Term) :-
	error:has_type(posint, Term),
	prime_lgc_test(Term).
%
error:has_type(pfact, Term) :-
	compound(Term),
	Term = P^E,
	error:has_type(prime, P),
	error:has_type(posint, E).
%
error:has_type(posint, Term) :-
	error:has_type(positive_integer, Term).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
