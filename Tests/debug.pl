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

:- module(debug, []).

:- multifile
	prolog:message//1.

/** <module> Common Debug Logic :: Code Documentation

Part of *|Nan.Common.Debugging|* (nan/common/debugging.pl)

Starts the documentation server on port =8001= with editing disabled.

At load time, includes the file test_all.pl if this exists, otherwise
issues a warning of the form =|debug:include_not_found(test_all)|=.

Use doc_browser/0 to open the default browser.

(Integrated with SWI-Prolog's documentation system.)

@author		Julio P. Di Egidio
@version	1.1.0
@copyright	2017 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(doc_http)).

prolog:message(debug:browser_help) -->
	['Use doc_browser/0 to open the default browser.'].

prolog:message(debug:include_not_found(File)) -->
	[ 'Include file not found: ~w.'-[File] ].

:- if(exists_file('test_all.pl')).
:- consult(test_all).
:- else.
:- print_message(warning, debug:include_not_found(test_all)).
:- endif.

:- portray_text(true).
:- doc_server(8001, [edit(false)]).
:- print_message(information, debug:browser_help).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
