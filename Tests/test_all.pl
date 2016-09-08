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

/** <module> A simple prime number library :: All tests

Predicates to run all tests.  (Entry point for SWI-Prolog pack system.)

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

:- use_module(library(plunit)).

:- use_module(prime_pio_test).
:- use_module(prime_mem_test).
:- use_module(prime_whl_test).
:- use_module(prime_prb_test).
:- use_module(prime_lgc_test).
% :- use_module(prime_test).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_test_options(			% TODO: Check integration with SWI pack system. #####
	[	load(always),
		run(manual),
		silent(false),
		sto(false),
		cleanup(false)
	]).

t__rnd_seed(0).		% Fixed for testing

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	test_all is det.
%
%	Prepares the test context then calls time/1 around run_tests/0.

test_all :-
	test_all__do(run_tests).

%!	test_all(+Spec) is det.
%
%	Prepares the test context then calls time/1 around run_tests/1.

test_all(Spec) :-
	test_all__do(run_tests(Spec)).

:- meta_predicate
	test_all__do(0).

test_all__do(G) :-
	t__rnd_seed(RndSeed),
	set_random(seed(RndSeed)),
	prime_whl_test:lev_(WLev),
	prime_whl:init_(WLev),
	time(G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile
	user:message_hook/3.

user:message_hook(prime_whl:init_begin(Lev), silent, _) :-
	print_message(informational, prime_whl:init_begin(Lev)).
user:message_hook(prime_whl:init_next(Lev0), silent, _) :-
	print_message(informational, prime_whl:init_next(Lev0)).
user:message_hook(prime_whl:init_end(Lev0), silent, _) :-
	print_message(informational, prime_whl:init_end(Lev0)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
