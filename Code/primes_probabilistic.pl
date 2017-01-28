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

% TODO: Implement test error estimates?
% TODO: Implement option for num. of iterations?

:- module(primes_probabilistic,
	[	prime_prb_test/2,     % +N, -Cert
		prime_prb_test/3,     % +N, +Rep, -Cert
		prime_prb_det_max/1,  % -Max
		prime_prb_rep_def/1   % -Rep
	]).

:- use_module(library(debug)).
:- use_module(library(random)).

/** <module> A Simple Prime Number Library :: Probabilistic

Part of *|Nan.Numerics.Primes|* (nan/numerics/primes.pl)

Module =primes_probabilistic= (nan/numerics/primes_probabilistic.pl)
provides predicates to test candidate primality of positive
integers based on a variant of the *|Miller-Rabin|* test that is
_deterministic_ for numbers up to =3317044064679887385961980=, otherwise it
is _probabilistic_ with the number of iterations fixed at =20=.

Predicates in this module can be safely called concurrently.

*NOTE*:
  - Predicates in this module do not validate their input.

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016-2017 Julio P. Di Egidio
@license	GNU GPLv3
@see		https://en.wikipedia.org/wiki/Miller-Rabin_primality_test

@tbd		Implement test error estimates?
@tbd		Implement option for num. of iterations?
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_prb_test(+N:posint, -Cert:bool) is semidet.
%!	prime_prb_test(+N:posint, +Rep:posint, -Cert:bool) is semidet.
%
%	True if N is a candidate prime number.
%
%	Cert is =true= if N is certainly prime, otherwise it is =false=.

prime_prb_test(N, Cert) :-
	prime_prb_rep_def(Rep),
	prime_prb_test(N, Rep, Cert).

prime_prb_test(1, _, _) :- !, fail.
prime_prb_test(2, _, true) :- !.
prime_prb_test(N, Rep, Cert) :-
	test__ws(N, Rep, Ws, Cert),
	\+ comp_(N, Ws).

%!	prime_prb_det_max(-Max:posint) is det.
%
%	Max is the maximum number for which the test is deterministic.
%
%	This value is fixed and equal to =3317044064679887385961980=.

prime_prb_det_max(Max) :-
	det__max(Max).

%!	prime_prb_rep_def(-Rep:posint) is det.
%
%	Rep is the default number of iterations for the probabilistic test.
%
%	This value is fixed and equal to =20=.

prime_prb_rep_def(20).							% TODO: Review this. ####

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	det__max(-Max:posint) is det.

det__max(3317044064679887385961980).

%	det__ws(+N:posint, -Ws:list(posint)) is semidet.

det__ws(N, Ws) :-
	det__ws__tbl(Sup, Ws), N < Sup, !.

det__ws__tbl(2047,                [2]).
det__ws__tbl(1373653,             [2, 3]).
det__ws__tbl(25326001,            [2, 3, 5]).
det__ws__tbl(3215031751,          [2, 3, 5, 7]).
det__ws__tbl(2152302898747,       [2, 3, 5, 7, 11]).
det__ws__tbl(3474749660383,       [2, 3, 5, 7, 11, 13]).
det__ws__tbl(341550071728321,     [2, 3, 5, 7, 11, 13, 17]).
det__ws__tbl(3825123056546413051, [2, 3, 5, 7, 11, 13, 17, 19, 23]).
det__ws__tbl(318665857834031151167461,
	[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]).
det__ws__tbl(3317044064679887385961981,
	[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]).	% 25 digits

%	prb__ws(+N:posint, +Rep:posint, -Ws:list(posint)) is det.

prb__ws(N, Rep, Ws) :-
	assertion(N > 3),
	N2 is N - 2,
	findall(W,
	(	between(1, Rep, _),
		random_between(2, N2, W)
	), Ws).

%	test__ws(+N:posint, +Rep:posint, -Ws:list(posint), -Cert:bool) is det.

test__ws(N, _, Ws, true) :-
	det__ws(N, Ws), !.
test__ws(N, Rep, Ws, false) :-
	prb__ws(N, Rep, Ws).

%	comp_(+N:posint +Ws:list(posint)) is semidet.

comp_(N, Ws) :-
	comp__n0_r0_d(N, N0, R0, D),
	comp__do(Ws, (N, N0, R0, D)).

comp__do([W| _], NRD) :-
	comp__w(W, NRD), !.
comp__do([_| Ws], NRD) :-
	comp__do(Ws, NRD).

comp__w(W, (N, N0, R0, D)) :-		% TODO: Review (optimise loops). #####
	X is powm(W, D, N),
	X =\= 1, X =\= N0,
	forall(
		between(1, R0, S),
		N0 =\= powm(X, 1 << S, N)
	).

comp__n0_r0_d(N, N0, R0, D) :-
	N0 is N - 1,
	comp__n0_r0_d__do(N0, 0, R, D),
	R0 is R - 1.

comp__n0_r0_d__do(N0, R0, R, D) :-
	N0 /\ 1 =:= 0, !,
	N1 is N0 >> 1,
	R1 is R0 + 1,
	comp__n0_r0_d__do(N1, R1, R, D).
comp__n0_r0_d__do(D, R, R, D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
