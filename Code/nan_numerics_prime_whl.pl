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

% (SWI-Prolog 7.3.24)

:- module(prime_whl, []).

:- public
	test_/2,		% +N:posint, -Cert:boolean
	right_/3,		% +N:posint, -P:posint, -Cert:boolean
	left_/3,		% +N:posint, -P:posint, -Cert:boolean
	lev_/1.			% -Lev:posint

/** <module> A simple prime number library :: wheel

The module =prime_whl= provides low-level predicates to test candidate
primality of numbers based on a prime wheel of level =4=.

*NOTE*: Predicates in this module are not meant for public use.

@author		Julio P. Di Egidio
@version	1.2.1-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	test_(+N:posint, -Cert:boolean) is semidet.
%
%	True if N is a candidate prime number.
%
%	Cert is =true= if N is certainly prime, otherwise it is =false=.

test_(N, true) :-
	lr_a_(N, _, R), !, N == R.
test_(N, Cert) :-
	lro_p_(N, _, 0),
	cert_(N, Cert).

%!	right_(+N:posint, -P:posint, -Cert:boolean) is det.
%
%	P is the smallest candidate prime number greater than or equal to N.
%
%	Cert is =true= if P is certainly prime, otherwise it is =false=.

right_(N, P, true) :-
	lr_a_(N, _, P), !.
right_(N, P, Cert) :-
	lro_p_(N, _, ROff),
	P is N + ROff,
	cert_(P, Cert).

%!	left_(+N:posint, -P:posint, -Cert:boolean) is semidet.
%
%	P is the greatest candidate prime number less than or equal to N.
%	Fails if N equals =1=.
%
%	Cert is =true= if P is certainly prime, otherwise it is =false=.

left_(1, _, _) :- !, fail.
left_(N, P, true) :-
	lr_a_(N, P, _), !.
left_(N, P, Cert) :-
	lro_p_(N, LOff, _),
	P is N - LOff,
	cert_(P, Cert).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	lev_(-Lev:posint) is det.
%
%	Lev is the number of prime numbers generating this wheel.

lev_(4/*lev*/).

%	lr_a_(+N:posint, -L:posint, -R:posint) is semidet.

lr_a_(N, L, R) :-
	N < 11/*la+1*/,
	whl_a_(N, L, R).

%	lro_p_(+N:posint, -LOff:nonneg, -ROff:nonneg) is det.

lro_p_(N, LOff, ROff) :-
	N0 is N - 11/*la+1*/,
	I0 is N0 mod 210/*lp*/,
	whl_p_(I0, LOff, ROff).

%	cert_(+N:posint, -Cert:boolean) is det.

cert_(N, true) :-
	N < 121/*(la+1)^2*/, !.
cert_(_, false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

whl_lev_(4).

whl_la_(10).

whl_lp_(210).

whl_a_(1, 0, 2).
whl_a_(2, 2, 2).
whl_a_(3, 3, 3).
whl_a_(4, 3, 5).
whl_a_(5, 5, 5).
whl_a_(6, 5, 7).
whl_a_(7, 7, 7).
whl_a_(8, 7, 11).
whl_a_(9, 7, 11).
whl_a_(10, 7, 11).

whl_p_(0, 0, 0).
whl_p_(1, 1, 1).
whl_p_(2, 0, 0).
whl_p_(3, 1, 3).
whl_p_(4, 2, 2).
whl_p_(5, 3, 1).
whl_p_(6, 0, 0).
whl_p_(7, 1, 1).
whl_p_(8, 0, 0).
whl_p_(9, 1, 3).
whl_p_(10, 2, 2).
whl_p_(11, 3, 1).
whl_p_(12, 0, 0).
whl_p_(13, 1, 5).
whl_p_(14, 2, 4).
whl_p_(15, 3, 3).
whl_p_(16, 4, 2).
whl_p_(17, 5, 1).
whl_p_(18, 0, 0).
whl_p_(19, 1, 1).
whl_p_(20, 0, 0).
whl_p_(21, 1, 5).
whl_p_(22, 2, 4).
whl_p_(23, 3, 3).
whl_p_(24, 4, 2).
whl_p_(25, 5, 1).
whl_p_(26, 0, 0).
whl_p_(27, 1, 3).
whl_p_(28, 2, 2).
whl_p_(29, 3, 1).
whl_p_(30, 0, 0).
whl_p_(31, 1, 1).
whl_p_(32, 0, 0).
whl_p_(33, 1, 3).
whl_p_(34, 2, 2).
whl_p_(35, 3, 1).
whl_p_(36, 0, 0).
whl_p_(37, 1, 5).
whl_p_(38, 2, 4).
whl_p_(39, 3, 3).
whl_p_(40, 4, 2).
whl_p_(41, 5, 1).
whl_p_(42, 0, 0).
whl_p_(43, 1, 5).
whl_p_(44, 2, 4).
whl_p_(45, 3, 3).
whl_p_(46, 4, 2).
whl_p_(47, 5, 1).
whl_p_(48, 0, 0).
whl_p_(49, 1, 1).
whl_p_(50, 0, 0).
whl_p_(51, 1, 5).
whl_p_(52, 2, 4).
whl_p_(53, 3, 3).
whl_p_(54, 4, 2).
whl_p_(55, 5, 1).
whl_p_(56, 0, 0).
whl_p_(57, 1, 3).
whl_p_(58, 2, 2).
whl_p_(59, 3, 1).
whl_p_(60, 0, 0).
whl_p_(61, 1, 1).
whl_p_(62, 0, 0).
whl_p_(63, 1, 5).
whl_p_(64, 2, 4).
whl_p_(65, 3, 3).
whl_p_(66, 4, 2).
whl_p_(67, 5, 1).
whl_p_(68, 0, 0).
whl_p_(69, 1, 3).
whl_p_(70, 2, 2).
whl_p_(71, 3, 1).
whl_p_(72, 0, 0).
whl_p_(73, 1, 5).
whl_p_(74, 2, 4).
whl_p_(75, 3, 3).
whl_p_(76, 4, 2).
whl_p_(77, 5, 1).
whl_p_(78, 0, 0).
whl_p_(79, 1, 7).
whl_p_(80, 2, 6).
whl_p_(81, 3, 5).
whl_p_(82, 4, 4).
whl_p_(83, 5, 3).
whl_p_(84, 6, 2).
whl_p_(85, 7, 1).
whl_p_(86, 0, 0).
whl_p_(87, 1, 3).
whl_p_(88, 2, 2).
whl_p_(89, 3, 1).
whl_p_(90, 0, 0).
whl_p_(91, 1, 1).
whl_p_(92, 0, 0).
whl_p_(93, 1, 3).
whl_p_(94, 2, 2).
whl_p_(95, 3, 1).
whl_p_(96, 0, 0).
whl_p_(97, 1, 1).
whl_p_(98, 0, 0).
whl_p_(99, 1, 3).
whl_p_(100, 2, 2).
whl_p_(101, 3, 1).
whl_p_(102, 0, 0).
whl_p_(103, 1, 7).
whl_p_(104, 2, 6).
whl_p_(105, 3, 5).
whl_p_(106, 4, 4).
whl_p_(107, 5, 3).
whl_p_(108, 6, 2).
whl_p_(109, 7, 1).
whl_p_(110, 0, 0).
whl_p_(111, 1, 5).
whl_p_(112, 2, 4).
whl_p_(113, 3, 3).
whl_p_(114, 4, 2).
whl_p_(115, 5, 1).
whl_p_(116, 0, 0).
whl_p_(117, 1, 3).
whl_p_(118, 2, 2).
whl_p_(119, 3, 1).
whl_p_(120, 0, 0).
whl_p_(121, 1, 5).
whl_p_(122, 2, 4).
whl_p_(123, 3, 3).
whl_p_(124, 4, 2).
whl_p_(125, 5, 1).
whl_p_(126, 0, 0).
whl_p_(127, 1, 1).
whl_p_(128, 0, 0).
whl_p_(129, 1, 3).
whl_p_(130, 2, 2).
whl_p_(131, 3, 1).
whl_p_(132, 0, 0).
whl_p_(133, 1, 5).
whl_p_(134, 2, 4).
whl_p_(135, 3, 3).
whl_p_(136, 4, 2).
whl_p_(137, 5, 1).
whl_p_(138, 0, 0).
whl_p_(139, 1, 1).
whl_p_(140, 0, 0).
whl_p_(141, 1, 5).
whl_p_(142, 2, 4).
whl_p_(143, 3, 3).
whl_p_(144, 4, 2).
whl_p_(145, 5, 1).
whl_p_(146, 0, 0).
whl_p_(147, 1, 5).
whl_p_(148, 2, 4).
whl_p_(149, 3, 3).
whl_p_(150, 4, 2).
whl_p_(151, 5, 1).
whl_p_(152, 0, 0).
whl_p_(153, 1, 3).
whl_p_(154, 2, 2).
whl_p_(155, 3, 1).
whl_p_(156, 0, 0).
whl_p_(157, 1, 1).
whl_p_(158, 0, 0).
whl_p_(159, 1, 3).
whl_p_(160, 2, 2).
whl_p_(161, 3, 1).
whl_p_(162, 0, 0).
whl_p_(163, 1, 5).
whl_p_(164, 2, 4).
whl_p_(165, 3, 3).
whl_p_(166, 4, 2).
whl_p_(167, 5, 1).
whl_p_(168, 0, 0).
whl_p_(169, 1, 1).
whl_p_(170, 0, 0).
whl_p_(171, 1, 5).
whl_p_(172, 2, 4).
whl_p_(173, 3, 3).
whl_p_(174, 4, 2).
whl_p_(175, 5, 1).
whl_p_(176, 0, 0).
whl_p_(177, 1, 3).
whl_p_(178, 2, 2).
whl_p_(179, 3, 1).
whl_p_(180, 0, 0).
whl_p_(181, 1, 1).
whl_p_(182, 0, 0).
whl_p_(183, 1, 3).
whl_p_(184, 2, 2).
whl_p_(185, 3, 1).
whl_p_(186, 0, 0).
whl_p_(187, 1, 1).
whl_p_(188, 0, 0).
whl_p_(189, 1, 9).
whl_p_(190, 2, 8).
whl_p_(191, 3, 7).
whl_p_(192, 4, 6).
whl_p_(193, 5, 5).
whl_p_(194, 6, 4).
whl_p_(195, 7, 3).
whl_p_(196, 8, 2).
whl_p_(197, 9, 1).
whl_p_(198, 0, 0).
whl_p_(199, 1, 1).
whl_p_(200, 0, 0).
whl_p_(201, 1, 9).
whl_p_(202, 2, 8).
whl_p_(203, 3, 7).
whl_p_(204, 4, 6).
whl_p_(205, 5, 5).
whl_p_(206, 6, 4).
whl_p_(207, 7, 3).
whl_p_(208, 8, 2).
whl_p_(209, 9, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
