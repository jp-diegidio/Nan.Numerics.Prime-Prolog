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

:- module(loader, []).

:- public
	load_module/1,		% +File:atom
	load_module/2.		% +Dir:atom, +File:atom

/** <module> Code loader

Predicates for loading code files.  (Meant to facilitate integration with
SWI-Prolog pack system.)

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dir_('../Code/').
dir_('../prolog/').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	load_module(+File:atom) is semidet.
%
%	Repeats calling =|load_module/2|= with _Dir_ successively set to
%	=|'../Code/'|= and =|'../prolog/'|=,
%	until the call succeeds.  Fails if File does not exist in any _Dir_.

load_module(File) :-
	dir_(Dir),
	load_module(Dir, File), !.

%!	load_module(+Dir:atom, +File:atom) is semidet.
%
%	Concatenates Dir and File then calls system:use_module/1 with the
%	resulting path.  Fails if File does not exist in Dir.

load_module(Dir, File) :-
	atom_concat(Dir, File, Path),
	exists_file(Path),
	use_module(Path).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
