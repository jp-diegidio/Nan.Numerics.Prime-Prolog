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

:- module(primes_wheel,
	[	prime_whl_test/2,     % +N, -Cert
		prime_whl_right/3,    % +N, -P, -Cert
		prime_whl_left/3,     % +N, -P, -Cert
		prime_whl_lev/1,      % -Lev
		prime_whl_det_max/1   % -Max
	]).

/** <module> A Simple Prime Number Library :: Wheel

Part of *|Nan.Numerics.Primes|* (nan/numerics/primes.pl)

Module =primes_wheel= (nan/numerics/primes/primes_wheel.pl) provides
predicates to test candidate primality of numbers and to find contiguous
candidates, based on a fixed prime wheel.

For code docs syntax and meaning see nan_help_docs.txt.

*NOTE*:
  - Predicates in this module are not synchronised.
  - Predicates in this module do not validate their input.

*NOTE*:
  - This module is meant for *|internal use only|*.

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016-2017 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_whl_test(+N:posint, -Cert:pcert) is semidet.
%
%	True if N is a candidate prime number.
%	
%	Cert is =true= if N is certainly prime, otherwise it is =false=.

prime_whl_test(N, true) :-
	w__a_is(N), !,
	w__a(N, N, N).
prime_whl_test(N, Cert) :-
	w__p_I0(N, I0),
	w__p(I0, 0, 0),
	w__cert(N, Cert).

%!	prime_whl_right(+N:posint, -P:posint, -Cert:pcert) is det.
%
%	P is the smallest candidate prime number greater than or equal to N.
%	
%	Cert is =true= if P is certainly prime, otherwise it is =false=.

prime_whl_right(N, P, true) :-
	w__a_is(N), !,
	w__a(N, _, P).
prime_whl_right(N, P, Cert) :-
	w__p_I0(N, I0),
	w__p(I0, _, ROff),
	P is N + ROff,
	w__cert(P, Cert).

%!	prime_whl_left(+N:posint, -P:posint, -Cert:pcert) is semidet.
%
%	P is the greatest candidate prime number less than or equal to N.
%	
%	Fails if N is equal to =1=.
%	
%	Cert is =true= if P is certainly prime, otherwise it is =false=.

prime_whl_left(1, _, _) :- !, fail.
prime_whl_left(N, P, true) :-
	w__a_is(N), !,
	w__a(N, P, _).
prime_whl_left(N, P, Cert) :-
	w__p_I0(N, I0),
	w__p(I0, LOff, _),
	P is N - LOff,
	w__cert(P, Cert).

%!	prime_whl_lev(-Lev:nonneg) is det.
%
%	Lev is the level of the wheel.

prime_whl_lev(Lev) :-
	w__lev(Lev).

%!	prime_whl_det_max(-Max:posint) is det.
%
%	Max is the maximum number for which the test is deterministic.

prime_whl_det_max(Max) :-
	w__det_max(Max).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	w__lev(-Lev:nonneg) is det.
%	w__a_is(+N:posint) is semidet.
%	w__p_I0(+N:posint, -I0:nonneg) is det.
%	w__cert(+N:posint, -Cert:pcert) is det.
%	w__a(-N:posint, -L:posint, -R:posint) is multi.
%	w__p(-I0:nonneg, -LOff:posint, -ROff:posint) is multi.

:- include(primes_wheel_inc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
