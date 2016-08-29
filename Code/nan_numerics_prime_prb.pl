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
	prb_mul_/1.		% -Mul:posint

/** <module> A simple prime number library :: probabilistic

Module =prime_prb= provides low-level predicates to test candidate
primality of numbers based on a probabilistic primality test.

Implements a variant of the *Miller-Rabin* primality test that is
_deterministic_ for numbers up to =3317044064679887385961980=, otherwise
it is _probabilistic_ with the number of iterations fixed at =20=.

*NOTE*: Predicates in this module are not meant for public use.

@author		Julio P. Di Egidio
@version	1.2.3-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
@tbd		Implement test error estimates?
@tbd		Implement option for num. of iterations?
*/

:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	test_(+N:posint, -Cert:boolean) is semidet.
%
%	True if N is a candidate prime number.
%
%	Cert is =true= if N is certainly prime, otherwise it is =false=.

test_(2, true) :- !.
test_(N, Cert) :-
	N > 2,
	test_as_(N, As, Cert),
	\+ comp_as_(N, As).

test_as_(N, As, true) :-
	det_as_(N, As), !.
test_as_(N, As, false) :-
	prb_as_(N, As).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	comp_as_(+N:posint +As:list(prime)) is semidet.

comp_as_(N, As) :-
	calc_n0sd_(N, N0, S0, D),
	comp_as_(As, N0, S0, D).

comp_as_([A| _], N0, S0, D) :-
	comp_a_(A, N0, S0, D), !.
comp_as_([_| As], N0, S0, D) :-
	comp_as_(As, N0, S0, D).

comp_a_(A, N0, S0, D) :-
	X is powm(A, D, N0 + 1),
	X =\= 1, X =\= N0,
	forall(
		between(1, S0, R),
		(	D2 is 1 << R,
			X2 is powm(X, D2, N0 + 1),
			X2 =\= N0
		)
	).

calc_n0sd_(N, N0, S0, D) :-
	N0 is N - 1,
	calc_n0sd___do(N0, 0, S, D),
	S0 is S - 1.

calc_n0sd___do(N0, A0, S, D) :-
	N0 /\ 1 =:= 0, !,
	N1 is N0 >> 1,
	A1 is A0 + 1,
	calc_n0sd___do(N1, A1, S, D).
calc_n0sd___do(D, S, S, D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	det_max_(-Max:posint) is det.
%
%	Max is the maximum number for which the test is deterministic.

det_max_(3317044064679887385961980).

%!	prb_mul_(-Mul:posint) is det.
%
%	Mul is the number of iterations for the probabilistic test.

prb_mul_(20).						% TODO: Review this. ####

%	prb_as_(+N:posint, -As:list(prime)) is semidet.

prb_as_(N, As) :-
	N2 is N - 2,
	findall(A,
	(	between(1, 20/*mul*/, _),
		random(2, N2, A)
	), As).

%	det_as_(+N:posint, -As:list(prime)) is semidet.

det_as_(N, As) :-
	det_sas_(Max, As), N < Max, !.

det_sas_(2047, [2]).
det_sas_(1373653, [2, 3]).
det_sas_(25326001, [2, 3, 5]).
det_sas_(3215031751, [2, 3, 5, 7]).
det_sas_(2152302898747, [2, 3, 5, 7, 11]).
det_sas_(3474749660383, [2, 3, 5, 7, 11, 13]).
det_sas_(341550071728321, [2, 3, 5, 7, 11, 13, 17]).
det_sas_(3825123056546413051, [2, 3, 5, 7, 11, 13, 17, 19, 23]).
det_sas_(318665857834031151167461, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]).
det_sas_(3317044064679887385961981, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
