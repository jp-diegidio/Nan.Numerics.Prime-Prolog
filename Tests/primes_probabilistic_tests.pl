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

:- module(primes_probabilistic_tests,
	[	expected_acc/1   % -Lev
	]).

:- multifile
	prolog:message//1.

/** <module> A Simple Prime Number Library :: Probabilistic tests

Part of *|Nan.Numerics.Primes|* (nan/numerics/primes.pl)

Tests for module =primes_probabilistic= (nan/numerics/primes_probabilistic.pl).

*NOTE*: Running tests in this module will throw an error if the probabilistic
accuracy is not as returned by expected_acc/1.  The error term is of the
form =|error(primes_probabilistic_tests:accuracy(Expected, Actual), _)|=.

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

%!	expected_acc(-Acc:nonneg) is det.
%
%	Acc is the probabilistic accuracy that is expected for testing.

expected_acc(80).

t__cert(N, pcert(N, true)) :-
	prime_prb_det_max(Max), N =< Max, !.
t__cert(N, pcert(N, Acc)) :-
	prime_prb_acc(Acc).

t__assert_acc :-
	expected_acc(TAcc),
	prime_prb_acc(Acc),
	t__assert_acc__sel(TAcc, Acc).

t__assert_acc__sel(TAcc, TAcc) :- !.
t__assert_acc__sel(TAcc, Acc) :-
	throw(error(primes_probabilistic_tests:accuracy(TAcc, Acc), _)).

prolog:message(error(primes_probabilistic_tests:accuracy(TAcc, Acc), _)) -->
	[ 'Probabilistic accuracy must be ~d, was ~d.'-[TAcc, Acc] ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Woodall primes: W_n := n * 2^n - 1
% http://www.prothsearch.net/woodall.html

t__woodall(I, N) :-
	N is (I << I) - 1.

t__w__gen(I, N) :-
	member(I,
	[	2, 3, 6, 30, 75, 81, 115, 123, 249, 362, 384, 462, 512, 751, 822
	]),
	t__woodall(I, N).

t__c(fail, 1).
t__c(true, 2).
t__c(true, 3).
t__c(fail, 4).
t__c(true, 5).
t__c(fail, 6).
t__c(true, 7).
t__c(fail, 8).
t__c(fail, 9).
t__c(fail, 10).
t__c(true, 11).
t__c(true, N) :- t__woodall(5312, N).

t__f(N) :-
	t__c(fail, N).

t__t(N, Cert) :-
	t__c(true, N),
	t__cert(N, Cert).

t__w(N, Cert) :-
	t__w__gen(_, N),
	t__cert(N, Cert).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_prb_test, [setup(t__assert_acc)]).

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

:- begin_tests(prime_prb_test_w, [setup(t__assert_acc)]).

test(prime_prb_test_w__t_def,
[	forall(t__w(N, C0)),
	true(Cert == C0)
]) :-
	prime_prb_test(N, Cert).

:- end_tests(prime_prb_test_w).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
