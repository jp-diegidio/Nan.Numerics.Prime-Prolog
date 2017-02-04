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

:- module(locator,
	[	file_sdir/1,  % ?Dir
		file_path/2,  % +File, -Path
		file_path/3   % +Dir, +File, -Path
	]).

:- multifile
	prolog:message//1.

/** <module> Common Debug Logic :: File Locator

Part of *|Nan.Common.Debugging|* (nan/common/debugging.pl)

Support predicates for locating files.

At load time, includes the file locator_inc.pl if this exists, otherwise
issues a warning of the form =|locator:include_not_found(locator_inc)|=.
Search directories can be defined in the included file, as well as they can
be loaded separately or defined dynamically.  See file_sdir/1 for details.

(Integrated with SWI-Prolog's pack system.)

@author		Julio P. Di Egidio
@version	1.1.0
@copyright	2017 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	file_sdir(?Dir:atom) is nondet.
%
%	Enumerates the search directories.
%
%	This predicate is =multifile= and =dynamic=: additional clauses can be
%	defined in the included file locator_inc.pl, as well as can be loaded
%	form a separate file and can also be defined dynamically.  Initially,
%	the list of clauses is empty.

:- multifile
	file_sdir/1.

:- dynamic
	file_sdir/1.

%!	file_path(+File:atom, -Path:atom) is det.
%
%	Returns a full path for given file name if the file is found in any
%	one of the search directories.
%	
%	Stops the search and succeeds at the first match encountered.
%	Throws an error of the form =|locator:file_not_found(sdir, File)|=
%	if the file could not be found.
%	
%	It is equivalent to:
%	==
%	file_path(File, Path) :-
%		file_sdir(Dir),
%		file_path(Dir, File, Path), !.
%	==

file_path(File, Path) :-
	file_sdir(Dir),
	file_path__do(Dir, File, Path), !.
file_path(File, _) :-
	file_path__throw(sdir, File).

%!	file_path(+Dir:atom, +File:atom, -Path:atom) is det.
%
%	Returns a full path by concatenating given directory and file name.
%	
%	Throws an error of the form =|locator:file_not_found(dir(Dir), File)|=
%	if the file cannot be found.

file_path(Dir, File, Path) :-
	file_path__do(Dir, File, Path), !.
file_path(Dir, File, _) :-
	file_path__throw(dir(Dir), File).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_path__do(Dir, File, Path) :-
	atom_concat(Dir, File, Path),
	exists_file(Path).

file_path__throw(Dir, File) :-
	throw(error(locator:file_not_found(Dir, File), _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prolog:message(locator:include_not_found(File)) -->
	[ 'Include file not found: ~w.'-[File] ].

:- if(exists_file('locator_inc.pl')).
:- include(locator_inc).
:- else.
:- print_message(warning, locator:include_not_found(locator_inc)).
:- endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
