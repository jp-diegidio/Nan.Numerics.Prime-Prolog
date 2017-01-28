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

:- module(primes_probabilistic_tests, []).

/** <module> A Simple Prime Number Library :: Probabilistic tests

Part of *|Nan.Numerics.Primes|* (nan/numerics/primes.pl)

Tests for module =primes_probabilistic= (nan/numerics/primes_probabilistic.pl).

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016-2017 Julio P. Di Egidio
@license	GNU GPLv3
*/

:- initialization
	use_module(loader),
	module_path('primes_probabilistic.pl', Path),
	use_module(Path).

:- use_module(library(plunit)).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t__cert(N, true) :- prime_prb_det_max(Max), N =< Max, !.
t__cert(_, false).

% Woodall primes: http://www.prothsearch.net/woodall.html

t__woodall(I, N) :-
	Is = [2, 3, 6, 30, /*75, 81, */115, 123, 249, 362, 384, 462, 512, 751, 822],
	member(I, Is),
	N is I * (1 << I) - 1.		% TODO: I<<I ? #####

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_prb_test).

t__rep(20).

t__c(fail, 1).
t__c(true, 2).
t__c(true, 3).
t__c(fail, 4).
t__c(fail, N) :- t__woodall(75, N0), N is N0 + 2.
t__c(fail, N) :- t__woodall(81, N0), N is N0 + 2.

t__f(N) :-
	t__c(fail, N).

t__t(N, Cert) :-
	t__c(true, N),
	t__cert(N, Cert).

test(prime_prb_test__f,
[	forall((t__rep(Rep), t__f(N))),
	fail
]) :-
	prime_prb_test(N, Rep, _).

test(prime_prb_test__t,
[	forall((t__rep(Rep), t__t(N, C0))),
	true(Cert == C0)
]) :-
	prime_prb_test(N, Rep, Cert).

test(prime_prb_test__f_def,
[	forall(t__f(N)),
	fail
]) :-
	prime_prb_test(N, _).

test(prime_prb_test__t_def,
[	forall(t__t(N, C0)),
	true(Cert == C0)
]) :-
	prime_prb_test(N, Cert).

:- end_tests(prime_prb_test).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_prb_test_w).

t__rep(20).

t__c(N, Cert) :-
	t__woodall(_, N),
	t__cert(N, Cert).

test(prime_prb_test_w__t,
[	forall((t__rep(Rep), t__c(N, C0))),
	true(Cert == C0)
]) :-
	prime_prb_test(N, Rep, Cert).

test(prime_prb_test_w__t_def,
[	forall(t__c(N, C0)),
	true(Cert == C0)
]) :-
	prime_prb_test(N, Cert).

:- end_tests(prime_prb_test_w).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
