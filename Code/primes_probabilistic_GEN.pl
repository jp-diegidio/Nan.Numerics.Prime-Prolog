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

:- module(primes_probabilistic_GEN,
	[	prime_prb_init/1,   % +Acc
		prime_prb_write/0,  % 
		prime_prb_write/1   % +Dst
	]).

:- multifile
	prolog:message//1.

/** <module> A Simple Prime Number Library :: Probabilistic Generation

Part of *|Nan.Numerics.Primes|* (nan/numerics/primes.pl)

TODO: #####

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016-2017 Julio P. Di Egidio
@license	GNU GPLv3
@see		https://en.wikipedia.org/wiki/Miller-Rabin_primality_test
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_prb_init(+Acc:nonneg) is det.

prime_prb_init(Acc) :-
	print_message(informational, primes_probabilistic:init_begin(Acc)),
	p__init(Acc),
	print_message(informational, primes_probabilistic:init_end).

%!	prime_prb_write is det.
%!	prime_prb_write(+Dst:(file; stream)) is det.

prime_prb_write :-
	prime_prb_write(current_output).

prime_prb_write(File) :-
	\+ is_stream(File), !,
	setup_call_cleanup(
		open(File, write, Stream),
		prime_prb_write(Stream),
		close(Stream)
	).
prime_prb_write(Stream) :-
	test__acc(Acc),
	print_message(informational, primes_probabilistic:write_begin(Acc)),
	prime_prb_write__head(Stream),
	prime_prb_write__pred(Stream, test__acc/1),
	prime_prb_write__pred(Stream, test__ws/3),
	prime_prb_write__pred(Stream, det__max/1),
	prime_prb_write__pred(Stream, det__ws/2),
	prime_prb_write__pred(Stream, prb__r/2),
	prime_prb_write__pred(Stream, prb__ws/3),
	prime_prb_write__pred(Stream, det__ws__tbl/2),
	prime_prb_write__pred(Stream, prb__r__tbl/2),
	prime_prb_write__foot(Stream),
	print_message(informational, primes_probabilistic:write_end).

prime_prb_write__pred(Stream, Fun/Ari) :-
	length(Args, Ari),
	Head =.. [Fun| Args],
	forall(
		clause(Head, Body),
		portray_clause(Stream, (Head :- Body))
	), nl(Stream).

prime_prb_write__head(Stream) :-
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

prime_prb_write__foot(Stream) :-
	write(Stream,
'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	test__ws(+N:posint, -Ws:list(posint), -Cert:bool) is det.

test__ws(N, Ws, true) :-
	det__ws(N, Ws), !.
test__ws(N, Ws, false) :-
	prb__r(N, Rep),
	prb__ws(N, Rep, Ws).

%	det__max(-Max:posint) is det.

det__max(3317044064679887385961980).	% < 82 bits

%	det__ws(+N:posint, -Ws:list(posint)) is semidet.

det__ws(N, Ws) :-
	det__ws__tbl(Sup, Ws), N < Sup, !.

%	det__ws__tbl(-Sup:posint, -Ws:list(posint)) is multi.

det__ws__tbl(2047, [2]).
det__ws__tbl(1373653, [2, 3]).
det__ws__tbl(25326001, [2, 3, 5]).
det__ws__tbl(3215031751, [2, 3, 5, 7]).
det__ws__tbl(2152302898747, [2, 3, 5, 7, 11]).
det__ws__tbl(3474749660383, [2, 3, 5, 7, 11, 13]).
det__ws__tbl(341550071728321, [2, 3, 5, 7, 11, 13, 17]).
det__ws__tbl(3825123056546413051, [2, 3, 5, 7, 11, 13, 17, 19, 23]).
det__ws__tbl(318665857834031151167461, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]).
det__ws__tbl(3317044064679887385961981, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]).

%	prb__ws(+N:posint, -Ws:list(posint)) is det.

prb__r(N, Rep) :-
	(	prb__r__tbl(Sup, Rep), N < Sup, !
	;	Rep = 1
	).

prb__ws(N, Rep, Ws) :-
	M is N - 2,
	findall(W,
	(	between(1, Rep, _),
		random_between(2, M, W)
	), Ws).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	test__acc(-Acc:nonneg) is det.
%	prb__r__tbl(-Sup:posint, -Rep:posint) is multi.

:- dynamic
	test__acc/1,
	prb__r__tbl/2.

%	p__init(+Acc:nonneg) is det.

p__init(Acc) :-
	retractall(test__acc(_)),
	retractall(prb__r__tbl(_, _)),
	p__init__r(Acc),
	assertz(test__acc(Acc)).

p__init__r(Acc) :-
	p__T_to_K(1, Acc, K),
	p__init__r__do(2, Acc, K).

p__init__r__do(_, _, K) :-
	K =< 81, !.
p__init__r__do(Rep, Acc, K) :-
	Sup is 1 << K,
	asserta(prb__r__tbl(Sup, Rep)),
	p__T_to_K(Rep, Acc, K1),
	Rep1 is Rep + 1,
	p__init__r__do(Rep1, Acc, K1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ?- time((J=100,(K=81;between(1,inf,I),K is 100*I),p__K_to_T(K,J,T),writeln([K,T]),(T=<1->!,fail;fail))).

% ?- time((J=100,between(1,inf,T),p__T_to_K(T,J,K),N is 1<<K,writeln([N,K,T]),(K=<81->!,fail;fail))).

%	T'(n,j)

p__N_to_T(N, J, T) :-
	N0 is N - 1,				% TODO: Verify this! #####
	ulog2(N0, K),
	p__K_to_T(K, J, T).

%	T(k,j)

p__K_to_T(K, J, T) :-
	E_max is 1 rdiv (1<<J),
	(	T = 1,
		p__E_1(K, 1, E)
	;	T = 2,
		p__E_2(K, 2, E)
	;	bloopx(3, (K//9), T),
		p__E_3(K, T, E)
	;	bloopx((K//9)+1, (K>>2), T),
		p__E_4(K, T, E)
	;	bloopx((K>>2)+1, inf, T),
		p__E_5(K, T, E)
	), E =< E_max, !.

%	K(t,j)

p__T_to_K(T, J, K) :-
	E_max is 1 rdiv (1<<J),
	(	T =:= 1,
		bloopx(81, inf, K),
		p__E_1(K, 1, E)
	;	T =:= 2,
		bloopx(81, inf, K),
		p__E_2(K, 2, E)
	;	bloopx(81, (T<<2), K),
		p__E_5(K, T, E)
	;	bloopx((T<<2)+1, (9*T), K),
		p__E_4(K, T, E)
	;	bloopx((9*T)+1, inf, K),
		p__E_3(K, T, E)
	), E =< E_max, !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	E_1(k,1)

p__E_1(K, 1, E) :-
	lsqrtx(K, S),
	E is ((K^2)<<4) rdiv (1<<(S<<1)).

%	E_2(k,2)

p__E_2(K, 2, E) :-
	p__E_1(K, 1, E_1),
	E is E_1 rdiv (4-4*E_1).

%	E_3(k,t)

p__E_3(K, T, E) :-
	usqrtx(((K^3)<<8)*(1<<(T<<1)), S1),
	lsqrtx(K*T, S2),
	lsqrtx(T*(1<<(S2<<2)), S3),
	E is S1 rdiv S3.

%	E_4(k,t)

p__E_4(K, T, E) :-
	usqrtx(K^15, S1),
	usqrtx(S1, S2),
	lsqrtx(49*(1<<(K + (T<<2))), S3),
	lsqrtx(1<<(K+((3*T)<<2)), S4),
	lsqrtx(S4, S5),
	E is 7*K rdiv ((5*(1<<(5*T)))<<2)
		+ S2 rdiv S3
		+ ((3*K)<<2) rdiv S5.

%	E_5(k,t)

p__E_5(K, T, E) :-
	usqrtx(K^15, S1),
	usqrtx(S1, S2),
	lsqrtx(49*(1<<(K + (T<<2))), S3),
	E is S2 rdiv S3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	ulog2(+N, -K) is det.

ulog2(N, K) :-
	between(1, inf, K),
	M is 1 << K,
	N =< M, !.

%	lsqrtx(+Exp, -S) is det.
%	usqrtx(+Exp, -S) is det.

lsqrtx(Exp, S) :-
	X is Exp,
	nth_integer_root_and_remainder(2, X, S, _).

usqrtx(Exp, S) :-
	X is Exp,
	nth_integer_root_and_remainder(2, X, S0, R),
	S is S0 + sign(R).

%	bloopx(+Exp_min, +Exp_max, -I) is nondet.

bloopx(Exp_min, Exp_max, I) :-
	Min is Exp_min,
	(Exp_max == inf -> Max = inf; Max is Exp_max),
	between(Min, Max, I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prolog:message(primes_probabilistic:init_begin(Acc)) -->
	[ 'Nan.Numerics.Primes::Probabilistic: Initialising accuracy ~d: ...'-[Acc], flush ].
prolog:message(primes_probabilistic:init_end) -->
	[ at_same_line, 'done.' ].

prolog:message(primes_probabilistic:write_begin(Acc)) -->
	[ 'Nan.Numerics.Primes::Probabilistic: Writing accuracy ~d: ...'-[Acc], flush ].
prolog:message(primes_probabilistic:write_end) -->
	[ at_same_line, 'done.' ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
