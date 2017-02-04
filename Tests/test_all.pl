%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*	Nan.Common.Debugging
	Nan.Common.Debugging/Prolog 1.1.0
	Common Debug Logic in Prolog
	Copyright 2017 Julio P. Di Egidio
	<mailto:julio@diegidio.name>
	<http://julio.diegidio.name/Projects/Nan.Common.Debugging/>
	
	This file is part of Nan.Common.Debugging.
	
	Nan.Common.Debugging is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	Nan.Common.Debugging is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with Nan.Common.Debugging.  If not, see <http://www.gnu.org/licenses/>.
*/

% (SWI-Prolog 7.3.25)

:- module(test_all,			% TODO: Check integration with SWI. #####
	[	test_all/0,  % 
		test_all/1   % +Spec
	]).

:- multifile
	prolog:message//1.

/** <module> Common Debug Logic :: Test Runner

Part of *|Nan.Common.Debugging|* (nan/common/debugging.pl)

Predicates to run all loaded tests.

At load time, includes the file test_all_inc.pl if this exists, otherwise
issues a warning of the form =|test_all:include_not_found(test_all_inc)|=.
Tests can be defined in the included file, as well as they can be loaded
separately.

(Integrated with SWI-Prolog's pack system.)

@author		Julio P. Di Egidio
@version	1.1.0
@copyright	2017 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(plunit)).

:- set_test_options(		% TODO: Check integration with SWI. #####
	[	load(always),
		run(manual),
		silent(false),
		sto(false),
		cleanup(false)
	]).

t__rnd_seed(0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	test_all is det.
%
%	Prepares the test context then calls run_tests/0 wrapped in time/1.

test_all :-
	test__do(run_tests).

%!	test_all(+Spec:atom) is det.
%
%	Prepares the test context then calls run_tests/1 wrapped in time/1.

test_all(Spec) :-
	test__do(run_tests(Spec)).

%	test__do(:GRun) is det.

:- meta_predicate
	test__do(0).

test__do(GRun) :-
	test__rnd,
	time(GRun).

test__rnd :-
	t__rnd_seed(Seed),
	set_random(seed(Seed)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prolog:message(test_all:include_not_found(File)) -->
	[ 'Include file not found: ~w.'-[File] ].

:- if(exists_file('test_all_inc.pl')).
:- include(test_all_inc).
:- else.
:- print_message(warning, test_all:include_not_found(test_all_inc)).
:- endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
