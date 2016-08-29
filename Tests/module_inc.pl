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

/*	A simple prime number library :: module_inc

@author		Julio P. Di Egidio
@version	1.2.5-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

module_inc(File) :-
	module_inc_('../Code/', File), !.
module_inc(File) :-
	module_inc_('../prolog/', File).

module_inc_(Dir, File) :-
	atomic_concat(Dir, File, Path),
	exists_file(Path),
	use_module(Path).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
