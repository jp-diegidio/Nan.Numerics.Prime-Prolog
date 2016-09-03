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

:- module(test_all,				% TODO: Check integration with SWI pack system. #####
	[	test_all/0,		% 
		test_all/1		% +Spec
	]).

/** <module> A simple prime number library :: test

Predicates to run all tests.  (Entry point for SWI pack system.)

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

:- use_module(library(plunit)).

:- consult('prime_mem.test').
:- consult('prime_whl.test').
:- consult('prime_prb.test').
:- consult('prime_lgc.test').
:- consult('prime.test').

:- set_test_options(			% TODO: Check integration with SWI pack system. #####
	[	load(always),
		run(manual),
		silent(false),
		sto(false),
		cleanup(false)
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	test_all is det.

test_all :-
	test_all__do(run_tests).

%!	test_all(+Spec) is det.

test_all(Spec) :-
	test_all__do(run_tests(Spec)).

:- meta_predicate
	test_all__do(0).

test_all__do(G) :-
	prime:prime_whl_init(4),
	time(G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile
	user:message_hook/3.

user:message_hook(prime_mem:fill_begin(_), _, _).
user:message_hook(prime_mem:fill_end, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
