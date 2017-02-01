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

:- module(primes_wheel_GEN,
	[	prime_whl_init/1,   % +Lev
		prime_whl_write/0,  % 
		prime_whl_write/1   % +Dst
	]).

:- multifile
	prolog:message//1.

/** <module> A Simple Prime Number Library :: Wheel Generation

*|Nan.Numerics.Primes (nan_numerics_prime.pl)|*

TODO: #####

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016-2017 Julio P. Di Egidio
@license	GNU GPLv3
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_whl_init(+Lev:nonneg) is det.

prime_whl_init(Lev) :-
	print_message(informational, primes_wheel:init_begin(Lev)),
	w__zro,
	prime_whl_init__do(0, Lev).

prime_whl_init__do(Lev0, Lev) :-
	Lev0 >= Lev, !,
	print_message(informational, primes_wheel:init_end(Lev0)).
prime_whl_init__do(Lev0, Lev) :-
	print_message(informational, primes_wheel:init_next(Lev0)),
	w__nxt,
	succ(Lev0, Lev1),
	prime_whl_init__do(Lev1, Lev).

%!	prime_whl_write is det.
%!	prime_whl_write(+Dst:(file; stream)) is det.

prime_whl_write :-
	prime_whl_write(current_output).

prime_whl_write(File) :-
	\+ is_stream(File), !,
	setup_call_cleanup(
		open(File, write, Stream),
		prime_whl_write(Stream),
		close(Stream)
	).
prime_whl_write(Stream) :-
	w__lev(Lev),
	print_message(informational, primes_wheel:write_begin(Lev)),
	prime_whl_write__head(Stream),
	prime_whl_write__pred(Stream, w__lev/1),
	prime_whl_write__pred(Stream, w__det_max/1),
	prime_whl_write__pred(Stream, w__a_is/1),
	prime_whl_write__pred(Stream, w__p_I0/2),
	prime_whl_write__pred(Stream, w__cert/2),
	prime_whl_write__pred(Stream, w__a/3),
	prime_whl_write__pred(Stream, w__p/3),
	prime_whl_write__foot(Stream),
	print_message(informational, primes_wheel:write_end).

prime_whl_write__pred(Stream, Fun/Ari) :-
	length(Args, Ari),
	Head =.. [Fun| Args],
	forall(
		clause(Head, Body),
		portray_clause(Stream, (Head :- Body))
	), nl(Stream).

prime_whl_write__head(Stream) :-
	write(Stream,
'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

'	).

prime_whl_write__foot(Stream) :-
	write(Stream,
'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	w__det_max(-Max:posint) is det.
%	w__a_is(+N:posint) is semidet.
%	w__p_I0(+N:posint, -I0:nonneg) is det.
%	w__cert(+N:posint, -Cert:pcertean) is det.

:- dynamic
	w__det_max/1,
	w__a_is/1,
	w__p_I0/2,
	w__cert/2.

%	w__sub is det.

w__sub :-
	w__p0(P0),
	w__pL(PL),
	P00 is P0 * P0,
	Max is P00 - 1,
	retractall(w__det_max(_)),
	retractall(w__a_is(_)),
	retractall(w__p_I0(_, _)),
	retractall(w__cert(_, _)),
	assertz(w__det_max(Max)),
	assertz((w__a_is(N) :- N < P0)),
	assertz((w__p_I0(N, I0) :- I0 is (N - P0) mod PL)),
	assertz((w__cert(N, true) :- N < P00, !)),
	assertz(w__cert(_, false)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	w__lev(-Lev:nonneg) is det.
%	w__p0(-P0:posint) is det.
%	w__pL(-PL:posint) is det.
%	w__a(-N:posint, -L:posint, -R:posint) is multi.
%	w__p(-I0:nonneg, -LOff:posint, -ROff:posint) is multi.

:- dynamic
	w__lev/1,
	w__p0/1,
	w__pL/1,
	w__a/3,
	w__p/3.

%	w__zro is det.

w__zro :-
	retractall(w__lev(_)),
	retractall(w__p0(_)),
	retractall(w__pL(_)),
	retractall(w__a(_, _, _)),
	retractall(w__p(_, _, _)),
	assertz(w__lev(0)),
	assertz(w__p0(2)),
	assertz(w__pL(1)),
	assertz(w__a(1, 1, 2)),
	assertz(w__p(0, 0, 0)),
	w__sub.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	w__nxt is det.

w__nxt :-
	retract(w__lev(Lev)),
	retract(w__p0(P0)),
	retract(w__pL(PL)),
	w__nxt__as(P0, PL),
	w__nxt__ps(P0, P1, Ps),
	w__nxt__rs(0, 0, Ps, P0, (P1, PL), (fail, 0)),
	w__nxt__ls(Lev, P0, P1, PL),
	w__sub.

w__nxt__as(P0, PL) :-
	w__nxt__a(P0, PL),
	w__nxt__as__do(P0, PL).

w__nxt__as__do(_, _) :-
	once(w__p(_, LOff, ROff)),
	LOff == 0, ROff == 0, !.
w__nxt__as__do(P0, PL) :-
	w__nxt__a(P0, PL), !,
	w__nxt__as__do(P0, PL).
w__nxt__as__do(_, _).

w__nxt__a(P0, PL) :-
	once(retract(w__p(I0, LOff, ROff))),
	N is I0 + P0,
	L is N - LOff,
	R is N + ROff,
	assertz(w__a(N, L, R)),
	I1 is I0 + PL,
	assertz(w__p(I1, LOff, ROff)).

w__nxt__ps(P0, P1, Ps) :-
	once(w__p(IOff, _, _)),
	P1 is P0 + IOff,
	findall((I1, LOff, ROff),
	(	retract(w__p(I0, LOff, ROff)),
		I1 is I0 - IOff
	), Ps).

w__nxt__rs(P0, _, _, P0, _, _) :- !.
w__nxt__rs(N0, A0, Ps, P0, (P1, PL), ILOffp0) :-
	L0 is A0 + P1,
	w__nxt__qs(Ps, (P0, A0, L0), ILOffp0, ILOffp1),
	N1 is N0 + 1,
	A1 is A0 + PL,
	w__nxt__rs(N1, A1, Ps, P0, (P1, PL), ILOffp1).

w__nxt__qs([], _, ILOffp0, ILOffp0).
w__nxt__qs([(I0, 0, 0)| Ps], (P0, A0, L0), (Ip0, _), ILOffp1) :-
	(I0 + L0) mod P0 =:= 0, !,
	w__nxt__q(Ps, I0, A0, Ip0, LOff1),
	w__nxt__qs(Ps, (P0, A0, L0), (Ip0, LOff1), ILOffp1).
w__nxt__qs([(I0, 0, 0)| Ps], (P0, A0, L0), _, ILOffp1) :- !,
	I1 is I0 + A0,
	assertz(w__p(I1, 0, 0)),
	w__nxt__qs(Ps, (P0, A0, L0), (I1, 0), ILOffp1).
w__nxt__qs([(I0, LOff, ROff)| Ps], (P0, A0, L0), (Ip0, LOffp0), ILOffp1) :-
	I1 is I0 + A0,
	LOff1 is LOff + LOffp0,
	assertz(w__p(I1, LOff1, ROff)),
	w__nxt__qs(Ps, (P0, A0, L0), (Ip0, LOffp0), ILOffp1).

w__nxt__q([(_, _, ROffj)| _], I0, A0, Ip0, LOff1) :-
	I1 is I0 + A0,
	Ip1 is Ip0 + 1,
	Ip2 is I1 - 1,
	ROff1 is ROffj + 1,
	forall(between(Ip1, Ip2, Ip),
	(	retract(w__p(Ip, LOffp, ROffp)),
		ROffp1 is ROffp + ROff1,
		assertz(w__p(Ip, LOffp, ROffp1))
	)),
	LOff1 is I1 - Ip0,
	assertz(w__p(I1, LOff1, ROff1)).
w__nxt__q([], _, _, _, 1) :-
	assertz(w__p(1, 1, 1)).

w__nxt__ls(Lev, P0, P1, PL) :-
	Lev1 is Lev + 1,
	PL1 is P0 * PL,
	assertz(w__lev(Lev1)),
	assertz(w__p0(P1)),
	assertz(w__pL(PL1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prolog:message(primes_wheel:init_begin(Lev)) -->
	[ 'Nan.Numerics.Primes::Wheel: Initialising level ~d: '-[Lev], flush ].
prolog:message(primes_wheel:init_next(Lev)) -->
	[ at_same_line, '~d'-[Lev], flush ].
prolog:message(primes_wheel:init_end(Lev)) -->
	[ at_same_line, '~d.'-[Lev] ].

prolog:message(primes_wheel:write_begin(Lev)) -->
	[ 'Nan.Numerics.Primes::Wheel: Writing level ~d: ...'-[Lev], flush ].
prolog:message(primes_wheel:write_end) -->
	[ at_same_line, 'done.' ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
