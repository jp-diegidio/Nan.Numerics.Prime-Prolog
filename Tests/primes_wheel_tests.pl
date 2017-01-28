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

:- module(primes_wheel_tests,
	[	expected_level/1   % -Lev
	]).

:- multifile
	prolog:message//1.

/** <module> A Simple Prime Number Library :: Wheel tests

Part of *|Nan.Numerics.Primes|* (nan/numerics/primes.pl)

Tests for module =primes_wheel= (nan/numerics/primes_wheel.pl).

*NOTE*: Running tests in this module will throw an error if the wheel is
not at the level returned by expected_level/1.  The error term is of the
form =|error(primes_wheel_tests:level(Expected, Actual), _)|=.

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016-2017 Julio P. Di Egidio
@license	GNU GPLv3
*/

:- initialization
	use_module(loader),
	module_path('primes_wheel.pl', Path),
	use_module(Path).

:- use_module(library(plunit)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	expected_level(-Lev:nonneg) is det.
%
%	Lev is the wheel level required for testing.  This is fixed at =4=.

expected_level(5).

t__cert(N, true) :- N < 169, !.
t__cert(_, false).

t__assert_lev :-
	expected_level(TLev),
	prime_whl_level(Lev),
	t__assert_lev__sel(TLev, Lev).

t__assert_lev__sel(TLev, TLev) :- !.
t__assert_lev__sel(TLev, Lev) :-
	throw(error(primes_wheel_tests:level(TLev, Lev), _)).

prolog:message(error(primes_wheel_tests:level(TLev, Lev), _)) -->
	[ 'Wheel level must be ~d, was ~d.'-[TLev, Lev] ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_whl_test, [setup(t__assert_lev)]).

t__c(fail, 1).
t__c(true, 2).
t__c(true, 3).
t__c(fail, 4).
t__c(true, 5).
t__c(fail, 6).
t__c(fail, 10).
t__c(true, 11).
t__c(fail, 12).
t__c(fail, 112).
t__c(true, 113).
t__c(fail, 114).
t__c(fail, 120).
t__c(fail, 121).
t__c(fail, 122).
t__c(fail, 168).
t__c(true, 169).
t__c(fail, 170).
t__c(fail, 220).
t__c(true, 221).
t__c(fail, 222).

t__f(N) :-
	t__c(fail, N).

t__t(N, Cert) :-
	t__c(true, N),
	t__cert(N, Cert).

test(prime_whl_test__f,
[	forall(t__f(N)),
	fail
]) :-
	prime_whl_test(N, _).

test(prime_whl_test__t,
[	forall(t__t(N, C0)),
	true(Cert == C0)
]) :-
	prime_whl_test(N, Cert).

:- end_tests(prime_whl_test).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_whl_right, [setup(t__assert_lev)]).

t__c(1, 2).
t__c(2, 2).
t__c(3, 3).
t__c(4, 5).
t__c(5, 5).
t__c(6, 7).
t__c(10, 11).
t__c(11, 11).
t__c(12, 13).
t__c(112, 113).
t__c(113, 113).
t__c(114, 127).
t__c(120, 127).
t__c(121, 127).
t__c(122, 127).
t__c(168, 169).
t__c(169, 169).
t__c(170, 173).
t__c(220, 221).
t__c(221, 221).
t__c(222, 223).

t__t(N, P, Cert) :-
	t__c(N, P),
	t__cert(P, Cert).

test(prime_whl_right__t,
[	forall(t__t(N, P0, C0)),
	true((P, Cert) == (P0, C0))
]) :-
	prime_whl_right(N, P, Cert).

:- end_tests(prime_whl_right).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(prime_whl_left, [setup(t__assert_lev)]).

t__c(2, 2).
t__c(3, 3).
t__c(4, 3).
t__c(5, 5).
t__c(6, 5).
t__c(10, 7).
t__c(11, 11).
t__c(12, 11).
t__c(112, 109).
t__c(113, 113).
t__c(114, 113).
t__c(120, 113).
t__c(121, 113).
t__c(122, 113).
t__c(168, 167).
t__c(169, 169).
t__c(170, 169).
t__c(220, 211).
t__c(221, 221).
t__c(222, 221).

t__t(N, P, Cert) :-
	t__c(N, P),
	t__cert(P, Cert).

test(prime_whl_left__f,
[	fail
]) :-
	prime_whl_left(1, _, _).

test(prime_whl_left__t,
[	forall(t__t(N, P0, C0)),
	true((P, Cert) == (P0, C0))
]) :-
	prime_whl_left(N, P, Cert).

:- end_tests(prime_whl_left).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
