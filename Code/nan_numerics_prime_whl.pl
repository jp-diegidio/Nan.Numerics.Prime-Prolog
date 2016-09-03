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

:- module(prime_whl, []).

:- public
	test_/2,		% +N:posint, -Cert:boolean
	right_/3,		% +N:posint, -P:posint, -Cert:boolean
	left_/3,		% +N:posint, -P:posint, -Cert:boolean
	wlev_/1,		% -Lev:nonneg
	winit_/1.		% +Lev:nonneg

/** <module> A simple prime number library :: wheel

Module =prime_whl= provides low-level predicates to test candidate
primality of numbers and find contiguous candidate numbers based on a
dynamic prime wheel generated by the first _Lev_ prime numbers, where _Lev_
is the level of the wheel.

*NOTE*: Access to predicates in this module is not synchronized.  You can
call all predicates with no locking, except at initialization time and when
calling =winit_/1=.

*NOTE*: Predicates in this module are _unsafe_, i.e. do not validate input
arguments and are not steadfast.

@author		Julio P. Di Egidio
@version	1.3.0-beta
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
	w__a_is(N), !,
	w__a(N, N, N).
test_(N, Cert) :-
	w__p_I0(N, I0),
	w__p(I0, 0, 0),
	w__cert(N, Cert).

%!	right_(+N:posint, -P:posint, -Cert:boolean) is det.
%
%	P is the smallest candidate prime number greater than or equal to N.
%
%	Cert is =true= if P is certainly prime, otherwise it is =false=.

right_(N, P, true) :-
	w__a_is(N), !,
	w__a(N, _, P).
right_(N, P, Cert) :-
	w__p_I0(N, I0),
	w__p(I0, _, ROff),
	P is N + ROff,
	w__cert(P, Cert).

%!	left_(+N:posint, -P:posint, -Cert:boolean) is semidet.
%
%	P is the greatest candidate prime number less than or equal to N.
%	Fails if N is equal to =1=.
%
%	Cert is =true= if P is certainly prime, otherwise it is =false=.

left_(1, _, _) :- !, fail.
left_(N, P, true) :-
	w__a_is(N), !,
	w__a(N, P, _).
left_(N, P, Cert) :-
	w__p_I0(N, I0),
	w__p(I0, LOff, _),
	P is N - LOff,
	w__cert(P, Cert).

%!	wlev_(-Lev:nonneg) is det.
%
%	Lev is the level of this wheel.

wlev_(Lev) :-
	w__lev(Lev).

%!	winit_(+Lev:nonneg) is det.
%
%	Initializes (or resets) this wheel to level Lev.

winit_(Lev) :-
	print_message(information, prime_whl:winit__zro(Lev)),
	w__zro,
	winit__do(Lev).

winit__do(Lev) :-
	w__lev(Lev0),
	Lev0 >= Lev, !,
	print_message(information, prime_whl:winit__end(Lev0)).
winit__do(Lev) :-
	w__lev(Lev0),
	print_message(information, prime_whl:winit__nxt(Lev0)),
	w__nxt,
	winit__do(Lev).

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

%	w__a_is(+N:posint) is semidet.
%	w__p_I0(+N:posint, -I0:nonneg) is det.
%	w__cert(+N:posint, -Cert:boolean) is det.

:- dynamic
	w__a_is/1,
	w__p_I0/2,
	w__cert/2.

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

%	w__sub is det.

w__sub :-
	w__p0(P0),
	w__pL(PL),
	P00 is P0 * P0,
	retractall(w__a_is(_)),
	retractall(w__p_I0(_, _)),
	retractall(w__cert(_, _)),
	assertz((w__a_is(N) :- N < P0)),
	assertz((w__p_I0(N, I0) :- I0 is (N - P0) mod PL)),
	assertz((w__cert(N, true) :- N < P00, !)),
	assertz(w__cert(_, false)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% prime_whl:w__stat.
% prime_whl:w__list__sub.
% prime_whl:((w__zro,prime_whl:w__list)).
% prime_whl:((w__nxt,prime_whl:w__list)).
% prime_whl:((between(1,30,N),w__test(N,Cert),writeln([N,Cert]),fail;true)).

%	w__stat is det.

w__stat :-
	ignore(w__lev(Lev)),
	ignore(w__p0(P0)),
	ignore(w__pL(PL)),
	format('L(~d, ~d, ~d)~n', [Lev, P0, PL]).

%	w__list is det.

w__list :-
	w__stat,
	forall(w__a(P, L, R), format('A(~d, ~d, ~d)~n', [P, L, R])),
	forall(w__p(I0, LOff, ROff), format('P(~d, ~d, ~d)~n', [I0, LOff, ROff])).

%	w__list__sub is det.

w__list__sub :-
	listing(w__a_is),
	listing(w__p_I0),
	listing(w__cert).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile
	prolog:message//1.

prolog:message(prime_whl:winit__zro(Lev)) -->
	[ 'nan_numerics_prime: Preparing wheel [Lev=~d]: .'-[Lev], flush ].
prolog:message(prime_whl:winit__nxt(Lev0)) -->
	[ at_same_line, '~d'-[Lev0], flush ].
prolog:message(prime_whl:winit__end(Lev0)) -->
	[ at_same_line, '~d.'-[Lev0] ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization(w__zro, now).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
