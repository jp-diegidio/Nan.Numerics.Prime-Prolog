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

:- module(primes_probabilistic,
	[	prime_prb_test/2,     % +N, -Cert
		prime_prb_acc/1,      % -Acc
		prime_prb_det_max/1   % -Max
	]).

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
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_prb_test(+N:posint, -Cert:pcert) is semidet.
%
%	True if N is a candidate prime number.
%	
%	The minimum accuracy of the test is defined by prime_prb_acc/1.
%	
%	Cert is a term of the form =|pcert(N, true)|= if N is certainly prime,
%	otherwise it is a term of the form =|pcert(N, Acc)|=.

prime_prb_test(1, _) :- !, fail.
prime_prb_test(2, pcert(2, true)) :- !.
prime_prb_test(N, Cert) :-
	test__ws(N, Ws, Cert),
	\+ comp__ws(N, Ws).

%!	prime_prb_acc(-Acc:nonneg) is det.
%
%	Acc is the minimum accuracy of the test.
%	
%	Equivalently, =|2^(-Acc)|= is the maximum probability of error.

prime_prb_acc(Acc) :-
	prb__acc(Acc).

%!	prime_prb_det_max(-Max:posint) is det.
%
%	Max is the maximum number for which the test is deterministic.

prime_prb_det_max(Max) :-
	det__max(Max).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	test__ws(+N:posint, -Ws:list(posint), -Cert:pcert) is det.

test__ws(N, Ws, pcert(N, true)) :-
	det__ws(N, Ws), !.
test__ws(N, Ws, pcert(N, Acc)) :-
	prb__ws(N, Ws),
	prb__acc(Acc).

%	comp__ws(+N:posint +Ws:list(posint)) is semidet.

comp__ws(N, Ws) :-
	comp__n0_r0_d(N, N0, R0, D),
	comp__do(Ws, (N, N0, R0, D)).

comp__do([W| _], NRD) :-
	comp__w(W, NRD), !.
comp__do([_| Ws], NRD) :-
	comp__do(Ws, NRD).

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

comp__w(W, (N, N0, R0, D)) :-
	X is powm(W, D, N),
	X =\= 1, X =\= N0,
	forall(
		between(1, R0, S),
		N0 =\= powm(X, 1 << S, N)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	det__max(-Max:posint) is det.

det__max(3317044064679887385961980).	% < 82 bits
% 3317044064679887385961980
% 2417851639229258349412352

%	det__ws(+N:posint, -Ws:list(posint)) is semidet.

det__ws(N, Ws) :-
	det__ws__tbl(Sup, Ws), N < Sup, !.

%	det__ws__tbl(-Sup:posint, -Ws:list(posint)) is multi.

det__ws__tbl(2047, [2]).
det__ws__tbl(1373653, [2, 3]).
det__ws__tbl(25326001, [2, 3, 5]).
det__ws__tbl(3215031751, [2, 3, 5, 7]).
det__ws__tbl(2152302898747, [2, 3, 5, 7, 11]).
det__ws__tbl(3474749660383, [2, 3, 5, 7, 11, 13]).
det__ws__tbl(341550071728321, [2, 3, 5, 7, 11, 13, 17]).
det__ws__tbl(3825123056546413051, [2, 3, 5, 7, 11, 13, 17, 19, 23]).
det__ws__tbl(318665857834031151167461, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]).
det__ws__tbl(3317044064679887385961981, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]).

%	prb__ws(+N:posint, -Ws:list(posint)) is det.
%	prb__ws(+N:posint, +Acc:nonneg, -Ws:list(posint)) is det.

prb__ws(N, Ws) :-
	(prb__rs__tbl(Sup, Rep), N < Sup, !; Rep = 1),
	M is N - 2,
	findall(W,
	(	between(1, Rep, _),
		random_between(2, M, W)
	), Ws).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	prb__acc(-Acc:nonneg) is det.
%	prb__rs__tbl(-Sup:posint, -Rep:posint) is multi.

:- include(primes_probabilistic_inc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
