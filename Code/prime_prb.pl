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

% TODO: Implement test error estimates?
% TODO: Implement option for num. of iterations?

:- module(prime_prb, []).

:- public
	test_/2,		% +N:posint, -Cert:boolean
	det_max_/1,		% -Max:posint
	prb_rep_/1.		% -Rep:posint

/** <module> A simple prime number library :: Probabilistic

*|Nan.Numerics.Prime (nan_numerics_prime.pl)|*

Module =prime_prb= (nan_numerics_prime_prb.pl)
provides low-level predicates to test candidate primality of positive
integer numbers based on a variant of the *|Miller-Rabin|* test that is
_deterministic_ for numbers up to =3317044064679887385961980=, otherwise it
is _probabilistic_ with the number of iterations fixed at =20=.

Predicates in this module can be safely called concurrently.

*NOTE*: Predicates in this module are _unsafe_, i.e. do not validate input
arguments and are not steadfast.

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
@see		https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
@tbd		Implement test error estimates?
@tbd		Implement option for num. of iterations?
*/

:- use_module(library(debug)).
:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	test_(+N:posint, -Cert:boolean) is semidet.
%
%	True if N is a candidate prime number.
%
%	Cert is =true= if N is certainly prime, otherwise it is =false=.
%
%	This predicate can be safely called concurrently.

test_(1, _) :- !, fail.
test_(2, true) :- !.
test_(N, Cert) :-
	test_as_(N, As, Cert),
	\+ comp_as_(N, As).

test_as_(N, As, true) :-
	det_as_(N, As), !.
test_as_(N, As, false) :-
	prb_as_(N, As).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	comp_as_(+N:posint +As:list(prime)) is semidet.

comp_as_(N, As) :-
	calc_nrd_(N, N0, R0, D),
	comp_as__do(As, (N, N0, R0, D)).

comp_as__do([A| _], NRD) :-
	comp_a_(A, NRD), !.
comp_as__do([_| As], NRD) :-
	comp_as__do(As, NRD).

comp_a_(A, (N, N0, R0, D)) :-
	X is powm(A, D, N),
	X =\= 1, X =\= N0,
	forall(
		between(1, R0, S),
		(	E is 1 << S,
			X2 is powm(X, E, N),
			X2 =\= N0
		)
	).

calc_nrd_(N, N0, R0, D) :-
	N0 is N - 1,
	calc_rd___do(N0, 0, R, D),
	R0 is R - 1.

calc_rd___do(N0, R0, R, D) :-
	N0 /\ 1 =:= 0, !,
	N1 is N0 >> 1,
	R1 is R0 + 1,
	calc_rd___do(N1, R1, R, D).
calc_rd___do(D, R, R, D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	det_max_(-Max:posint) is det.
%
%	Max is the maximum number for which the test is deterministic.
%
%	Max is fixed and equal to =3317044064679887385961980=.
%
%	*NOTE*: This predicate can be safely called concurrently.

det_max_(3317044064679887385961980).

%!	prb_rep_(-Rep:posint) is det.
%
%	Rep is the number of iterations for the probabilistic test.
%
%	Rep is fixed and equal to =20=.
%
%	*NOTE*: This predicate can be safely called concurrently.

prb_rep_(20).							% TODO: Review this. ####

%	prb_as_(+N:posint, -As:list(prime)) is det.

prb_as_(N, As) :-
	assertion(N > 3),
	prb_rep_(Rep),
	N2 is N - 2,
	findall(A,
	(	between(1, Rep, _),
		random_between(2, N2, A)
	), As).

%	det_as_(+N:posint, -As:list(prime)) is semidet.

det_as_(N, As) :-
	det_sas_(Sup, As), N < Sup, !.

det_sas_(2047, [2]).
det_sas_(1373653, [2, 3]).
det_sas_(25326001, [2, 3, 5]).
det_sas_(3215031751, [2, 3, 5, 7]).
det_sas_(2152302898747, [2, 3, 5, 7, 11]).
det_sas_(3474749660383, [2, 3, 5, 7, 11, 13]).
det_sas_(341550071728321, [2, 3, 5, 7, 11, 13, 17]).
det_sas_(3825123056546413051, [2, 3, 5, 7, 11, 13, 17, 19, 23]).
det_sas_(318665857834031151167461, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]).
det_sas_(3317044064679887385961981, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]).	% 25 digits

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
