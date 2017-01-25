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

% TODO: Implement dynamic memoization.
% TODO: Fix code synchronization and visibility.
% TODO: Extend type =callable=.
% TODO: Upgrade infos to MD format.

% TODO: Implement parallel factoring functions.
% TODO: Implement prime counting/n-th prime functions.
% TODO: Implement probabilitic test error estimates?
% TODO: Implement option for num. of probabilistic iterations?
% TODO: Implement deterministic tests?
% TODO: Improve compatibility with other Prolog systems.

:- module(prime,
	[	prime_test/1,			% +N:posint
		prime_right/2,			% +N:posint, -P:prime
		prime_left/2,			% +N:posint, -P:prime
		prime_next/2,			% +N:posint, -P:prime
		prime_prev/2,			% +N:posint, -P:prime
		prime_gen/1,			% -P:prime
		prime_gen/2,			% +Inf:posint, -P:prime
		prime_gen/3,			% +Inf:posint, +Sup:posint, -P:prime
		prime_gen_rev/2,		% +Sup:posint, -P:prime
		prime_gen_rev/3,		% +Inf:posint, +Sup:posint, -P:prime
		prime_div/2,			% +N:posint, -P:prime
		prime_div_rev/2,		% +N:posint, -P:prime
		prime_fact/2,			% +N:posint, -PFs:list(pfact)
		%%
		prime_det_max/1,		% -Max:posint
		prime_prb_mul/1,		% -Mul:posint
		prime_whl_lev/1,		% -Lev:nonneg
		prime_whl_init/1,		% +Lev:nonneg
		prime_mem_count/1,		% -Cnt:nonneg
		prime_mem_fill/1,		% +Sup:posint
		prime_mem_fill/2,		% +Inf:posint, +Sup:posint
		prime_mem_clear/0,		% 
		%%
		prime_load_file/1,		% +File:file
		prime_load_file/2,		% +File:file, +Sup:posint
		prime_load_file/3,		% +File:file, +Inf:posint, +Sup:posint
		prime_save_file/2,		% +File:file, +Sup:posint
		prime_save_file/3,		% +File:file, +Inf:posint, +Sup:posint
		prime_load_stream/1,	% +Stream:stream
		prime_load_stream/2,	% +Stream:stream, +Sup:posint
		prime_load_stream/3,	% +Stream:stream, +Inf:posint, +Sup:posint
		prime_save_stream/2,	% +Stream:stream, +Sup:posint
		prime_save_stream/3		% +Stream:stream, +Inf:posint, +Sup:posint
	]).

/** <module> A simple prime number library

*|Nan.Numerics.Prime (nan_numerics_prime.pl)|*

This library implements a variant of the *Miller-Rabin* primality test that
is _deterministic_ for numbers up to =3317044064679887385961980=, otherwise
it is _probabilistic_ with the number of iterations fixed at =20=.  For
better performance, leverages a configurable prime wheel and optional
memoization of pairs of consecutive prime numbers.

*NOTE*: Since the primality test in use is _probabilistic_ in general, this
library is not suitable for cryptographic applications.

Module =prime= (nan_numerics_prime.pl)
provides predicates to test positive integer numbers for primality, find
consecutive prime numbers, generate prime numbers in some interval, find
divisors and factor numbers.  Also provides predicates to control the
underlying prime wheel and memoization, and predicates to save/load
comma-separated lists of prime numbers in some interval to/from a file or
stream.

*NOTE*: At initialization time, the prime wheel is set to level =6=.

*NOTE*: For maximum performance, predicates in this module are _not_
synchronized.  See the documentation accompanying each module and predicate
in this library for synchronization details.

*NOTE*: All predicates in this module are _safe_, i.e. validate input
arguments and ensure steadfastness.  For maximum performance, user code can
directly call the _unsafe_ =public= (not exported) predicates provided by
the following sub-modules:

$ Module =prime_lgc= (nan_numerics_prime_lgc.pl) :
provides low-level predicates to test positive integer numbers for
primality, find consecutive prime numbers, generate prime numbers in some
interval, find divisors and factor numbers.

$ Module =prime_prb= (nan_numerics_prime_prb.pl) :
provides low-level predicates to test candidate primality of positive
integer numbers based on a variant of the *|Miller-Rabin|* test -- as
explained above.

$ Module =prime_whl=(nan_numerics_prime_whl.pl) :
provides low-level predicates to test candidate primality of numbers and
find contiguous candidates based on a prime wheel generated by the first
_Lev_ prime numbers, where _Lev_ is the configurable level of the wheel.

$ Module =prime_mem= (nan_numerics_prime_mem.pl) :
provides low-level predicates for the memoization of pairs of consecutive
prime numbers.

$ Module =prime_pio= (nan_numerics_prime_pio.pl) :
provides low-level predicates to read/write comma-separated lists of
positive integer numbers from/to a file or stream.

Usage example:
--------------
==
?- pack_install(nan_numerics_prime).
true.

?- use_module(library(nan_numerics_prime)).
true.

?- time(prime_right(1234567891012345678901234567890123456789011111, P)).
% 1,227 inferences, 0.000 CPU in 0.010 seconds (0% CPU, Infinite Lips)
P = 1234567891012345678901234567890123456789011139.

?- time(prime_lgc:right(1234567891012345678901234567890123456789011111, P)).
% 1,227 inferences, 0.000 CPU in 0.010 seconds (0% CPU, Infinite Lips)
P = 1234567891012345678901234567890123456789011139.
==

This library was developed and tested with:
SWI-Prolog 7.3.25 - http://www.swi-prolog.org/

@author		Julio P. Di Egidio
@version	1.3.0-beta
@copyright	2016 Julio P. Di Egidio
@license	GNU GPLv3
@tbd		Implement parallel factoring functions.
@tbd		Implement prime counting/n-th prime functions.
@tbd		Implement probabilitic test error estimates?
@tbd		Implement option for num. of probabilistic iterations?
@tbd		Implement deterministic tests?
@tbd		Improve compatibility with other Prolog systems.
*/

:- use_module(nan_numerics_prime_lgc).
:- use_module(nan_numerics_prime_pio).
:- use_module(library(error)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_test(+N:posint) is semidet.
%
%	True if N is a prime number.
%
%	The corresponding _unsafe_ predicate is =|prime_lgc:test_/1|=.

prime_test(N) :-
	must_be(posint, N),
	prime_lgc:test_(N).

%!	prime_right(+N:posint, -P:prime) is det.
%
%	P is the smallest prime number greater than or equal to N.
%
%	The corresponding _unsafe_ predicate is =|prime_lgc:right_/2|=.

prime_right(N, P) :-
	must_be(posint, N),
	prime_lgc:right_(N, P_), P = P_.

%!	prime_left(+N:posint, -P:prime) is semidet.
%
%	P is the greatest prime number less than or equal to N.  Fails if N is
%	equal to =1=.
%
%	The corresponding _unsafe_ predicate is =|prime_lgc:left_/2|=.

prime_left(N, P) :-
	must_be(posint, N),
	prime_lgc:left_(N, P_), P = P_.

%!	prime_next(+N:posint, -P:prime) is det.
%
%	P is the smallest prime number greater than N.
%
%	The corresponding _unsafe_ predicates are =|prime_lgc:next_/2|=, and
%	=|prime_lgc:next_p_/2|= if N is definitely prime.

prime_next(N, P) :-
	must_be(posint, N),
	prime_lgc:next_(N, P_), P = P_.

%!	prime_prev(+N:posint, -P:prime) is semidet.
%
%	P is the greatest prime number less than N.  Fails if N is less than or
%	equal to =2=.
%
%	The corresponding _unsafe_ predicates are =|prime_lgc:prev_/2|=, and
%	=|prime_lgc:prev_p_/2|= if N is definitely prime.

prime_prev(N, P) :-
	must_be(posint, N),
	prime_lgc:prev_(N, P_), P = P_.

%!	prime_gen(-P:prime) is multi.
%!	prime_gen(+Inf:posint, -P:prime) is nondet.
%!	prime_gen(+Inf:posint, +Sup:posint, -P:prime) is nondet.
%
%	Generates in _ascending_ order all prime numbers P, greater than or
%	equal to Inf in the variants with arity =[2,3]=, and less than or equal
%	to Sup in the variant with arity =3=.  Fails if the prime to the right
%	of Inf is greater than the prime to the left of Sup.
%
%	The corresponding _unsafe_ predicates are =|prime_lgc:gen_/[2-3]|=, and
%	=|prime_lgc:gen_p_/[2-3]|= if the bounds are definitely prime.

prime_gen(P) :-
	prime_lgc:gen_(1, P_), P = P_.

prime_gen(Inf, P) :-
	must_be(posint, Inf),
	prime_lgc:gen_(Inf, P_), P = P_.

prime_gen(Inf, Sup, P) :-
	must_be(posint, Inf),
	must_be(posint, Sup),
	prime_lgc:gen_(Inf, Sup, P_), P = P_.

%!	prime_gen_rev(+Sup:posint, -P:prime) is nondet.
%!	prime_gen_rev(+Inf:posint, +Sup:posint, -P:prime) is nondet.
%
%	Generates in _descending_ order all prime numbers P less than or equal
%	to Sup, and greater than or equal to Inf in the variant with arity =3=.
%	Fails if Sup is equal to =1= or if the prime to the right of Inf is
%	greater than the prime to the left of Sup.
%
%	The corresponding _unsafe_ predicates are =|prime_lgc:gen_rev_/3|=,
%	and =|prime_lgc:gen_rev_p_/3|= if the bounds are definitely prime.

prime_gen_rev(Sup, P) :-
	must_be(posint, Sup),
	prime_lgc:gen_rev_(1, Sup, P_), P = P_.

prime_gen_rev(Inf, Sup, P) :-
	must_be(posint, Inf),
	must_be(posint, Sup),
	prime_lgc:gen_rev_(Inf, Sup, P_), P = P_.

%!	prime_div(+N:posint, -P:prime) is semidet.
%
%	True if N is a composite number with P its smallest prime divisor.
%
%	The corresponding _unsafe_ predicate is =|prime_lgc:div_/2|=.

prime_div(N, P) :-
	must_be(posint, N),
	prime_lgc:div_(N, P_), P = P_.

%!	prime_div_rev(+N:posint, -P:prime) is semidet.
%
%	True if N is a composite number with P its greatest prime divisor.
%
%	The corresponding _unsafe_ predicate is =|prime_lgc:div_rev_/2|=.

prime_div_rev(N, P) :-
	must_be(posint, N),
	prime_lgc:div_rev_(N, P_), P = P_.

%!	prime_fact(+N:posint, -PFs:list(pfact)) is det.
%
%	PFs is the list of all prime factors of N in _ascending_ order of the
%	prime divisors.
%
%	Elements of PFs are of the form =|P^F|= with _P_ the prime divisor and
%	_F_ the corresponding power.
%
%	If N is equal to =1= or N is a prime number, PFs is =|[N^1]|=.
%
%	The corresponding _unsafe_ predicate is =|prime_lgc:fact_/2|=.

prime_fact(N, PFs) :-
	must_be(posint, N),
	prime_lgc:fact_(N, PFs_), PFs = PFs_.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_det_max(-Max:posint) is det.
%
%	Max is the maximum number for which the primality test is still
%	_deterministic_.

prime_det_max(Max) :-
	prime_prb:det_max_(Max_), Max = Max_.

%!	prime_prb_mul(-Mul:posint) is det.
%
%	Mul is the number of iterations for the _probabilistic_ primality test.

prime_prb_mul(Mul) :-
	prime_prb:prb_mul_(Mul_), Mul = Mul_.

%!	prime_whl_lev(-Lev:nonneg) is det.
%
%	Lev is the level of the underlying prime wheel.

prime_whl_lev(Lev) :-
	prime_whl:wlev_(Lev_), Lev = Lev_.

%!	prime_whl_init(+Lev:nonneg) is det.
%
%	Initializes (or resets) the underlying prime wheel to level Lev.

prime_whl_init(Lev) :-
	must_be(nonneg, Lev),
	prime_whl:winit_(Lev).

%!	prime_mem_fill(+Sup:posint) is det.
%!	prime_mem_fill(+Inf:posint, +Sup:posint) is det.
%
%	Ensures that all pairs of consecutive prime numbers have been memoized
%	whose elements are less than or equal to Sup, and greater than or
%	equal to Inf in the variant with arity =2.

prime_mem_fill(1) :- !.
prime_mem_fill(Sup) :-
	must_be(posint, Sup),
	prime_lgc:left_(Sup, H),
	prime_mem:fill_(prime_lgc:next_, 2, H).

prime_mem_fill(Inf, 1) :- !,
	must_be(posint, Inf).
prime_mem_fill(Inf, Sup) :-
	must_be(posint, Inf),
	must_be(posint, Sup),
	prime_lgc:right_(Inf, L),
	prime_lgc:left_(Sup, H),
	prime_mem:fill_(prime_lgc:next_, L, H).

%!	prime_mem_clear is det.
%
%	Clears all memoization.

prime_mem_clear :-
	prime_mem:clear_.

%!	prime_mem_count(-Cnt:nonneg) is det.
%
%	Cnt is the total number of memoized pairs of prime numbers.

prime_mem_count(Cnt) :-
	prime_mem:count_(Cnt_), Cnt = Cnt_.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	prime_load_file(+File:file) is det.
%!	prime_load_file(+File:file, +Sup:posint) is det.
%!	prime_load_file(+File:file, +Inf:posint, +Sup:posint) is det.
%
%	Ensures memoization of pairs of consecutive prime numbers read from
%	File whose elements are greater than or equal to Inf in the variant
%	with arity =3=, and less than or equal to Sup in the variants with
%	arity =[2-3]=.  Reads File to the end-of-file, or until a prime
%	number greater than or equal to Sup is encountered in the variants with
%	arity =[2-3]=.
%
%	The accepted file format is a comma-separated list of consecutive prime
%	numbers. The file can be empty or contain a single prime number.
%
%	Encoding of file is =ascii=, type is =text=, stream buffer size is
%	=1024=.
%
%	@error	syntax_error(invalid_format)		Format is invalid.
%	@error	syntax_error(invalid_prime(P))		Value _P_ must be prime.
%	@error	syntax_error(invalid_consec(P0, P))	Value _P_ must be the successor prime of _P0_.
%	@error	Errors from system:open/4.
%	@error	Errors from system:read/2.

prime_load_file(File) :-
	must_be(file, File),
	load_file_(File).

prime_load_file(File, Sup) :-
	must_be(file, File),
	must_be(posint, Sup),
	load_file_(1, Sup, File).

prime_load_file(File, Inf, Sup) :-
	must_be(file, File),
	must_be(posint, Inf),
	must_be(posint, Sup),
	load_file_(Inf, Sup, File).

%!	prime_save_file(+File:file, +Sup:posint) is det.
%!	prime_save_file(+File:file, +Inf:posint, +Sup:posint) is det.
%
%	Writes to File all successive prime numbers less than or equal to Sup,
%	and greater than or equal to Inf in the variant with arity =3=.
%
%	The produced file format is a comma-separated list of prime numbers.
%	The file is empty if Sup is equal to =1= or if Sup is greater than Inf.
%
%	Encoding of file is =ascii=, type is =text=, buffering is =full=,
%	stream buffer size is =1024=.
%
%	@error	Errors from system:open/4.
%	@error	Errors from system:write/2.

prime_save_file(File, Sup) :-
	must_be(file, File),
	must_be(posint, Sup),
	save_file_(1, Sup, File).

prime_save_file(File, Inf, Sup) :-
	must_be(file, File),
	must_be(posint, Inf),
	must_be(posint, Sup),
	save_file_(Inf, Sup, File).

%!	prime_load_stream(+Stream:stream) is det.
%!	prime_load_stream(+Stream:stream, +Sup:posint) is det.
%!	prime_load_stream(+Stream:stream, +Inf:posint, +Sup:posint) is det.
%
%	Ensures memoization of pairs of consecutive prime numbers read from
%	Stream whose elements are greater than or equal to Inf in the variant
%	with arity =3=, and less than or equal to Sup in the variants with
%	arity =[2-3]=.  Reads Stream to the end-of-file, or until a prime
%	number greater than or equal to Sup is encountered in the variants with
%	arity =[2-3]=.
%
%	The accepted file format is a comma-separated list of consecutive prime
%	numbers. The file can be empty or contain a single prime number.
%
%	Encoding of stream is =ascii=, type is =text=, buffer size is =1024=.
%
%	@error	syntax_error(invalid_format)		Format is invalid.
%	@error	syntax_error(invalid_prime(P))		Value _P_ must be prime.
%	@error	syntax_error(invalid_consec(P0, P))	Value _P_ must be the successor prime of _P0_.
%	@error	Errors from system:read/2.

prime_load_stream(Stream) :-
	must_be(stream, Stream),
	load_stream_(Stream).

prime_load_stream(Stream, Sup) :-
	must_be(stream, Stream),
	must_be(posint, Sup),
	load_stream_(1, Sup, Stream).

prime_load_stream(Stream, Inf, Sup) :-
	must_be(stream, Stream),
	must_be(posint, Inf),
	must_be(posint, Sup),
	load_stream_(Inf, Sup, Stream).

%!	prime_save_stream(+Stream:stream, +Sup:posint) is det.
%!	prime_save_stream(+Stream:stream, +Inf:posint, +Sup:posint) is det.
%
%	Writes to Stream all successive prime numbers less than or equal to
%	Sup, and greater than or equal to Inf in the variant with arity =3=.
%
%	The produced file format is a comma-separated list of prime numbers.
%	The file is empty if Sup is equal to =1= or if Sup is greater than Inf.
%
%	Encoding of stream is =ascii=, type is =text=, buffering is =full=,
%	buffer size is =1024=.
%
%	@error	Errors from system:write/2.

prime_save_stream(Stream, Sup) :-
	must_be(stream, Stream),
	must_be(posint, Sup),
	save_stream_(1, Sup, Stream).

prime_save_stream(Stream, Inf, Sup) :-
	must_be(stream, Stream),
	must_be(posint, Inf),
	must_be(posint, Sup),
	save_stream_(Inf, Sup, Stream).

%	load_file_(+File:file) is det.
%	load_file_(+Inf:posint, +Sup:posint, +File:file) is det.

load_file_(File) :-
	prime_pio:open_(File, read, prime:load_stream_).

load_file_(Inf, Sup, File) :-
	prime_pio:open_(File, read, prime:load_stream_(Inf, Sup)).

%	save_file_(+Inf:posint, +Sup:posint, +File:file) is det.

save_file_(Inf, Sup, File) :-
	prime_pio:open_(File, write, prime:save_stream_(Inf, Sup)).

%	load_stream_(+Stream:stream) is det.
%	load_stream_(+Inf:posint, +Sup:posint, +Stream:stream) is det.

load_stream_(Stream) :-
	prime_pio:read_(Stream, prime:load_stream__add).

load_stream_(Inf, Sup, Stream) :-
	prime_pio:read_(Stream, prime:load_stream__add(Inf, Sup)).

load_stream__add(P0, P) :-
	prime_lgc:next_p_(P0, P_), P = P_, !,
	prime_mem:add_(P0, P).
load_stream__add(P0, P) :-
	prime_lgc:test_(P), !,
	syntax_error(invalid_consec(P0, P)).
load_stream__add(_, P) :-
	syntax_error(invalid_prime(P)).

load_stream__add(Inf, _, P0, _) :-
	P0 < Inf, !.
load_stream__add(_, Sup, P0, P) :-
	P =< Sup,
	load_stream__add(P0, P),
	P < Sup.

%	save_stream_(+Inf:posint, +Sup:posint, +Stream:stream) is det.

save_stream_(Inf, Sup, Stream) :-
	prime_pio:write_(Stream, prime_lgc:gen_(Inf, Sup)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!	error:has_type(+Type:atom, @Term:any) is semidet.
%
%	True if Term satisfies Type.
%
%	Extends library(error) with the following types:
%
%	| =prime=	| Prime number |
%	| =pmode=	| =|oneof([direct, mem_read, mem_write, mem_sync])|= |
%	| =pfact=	| =|P^F|= with _P_ =prime= and _F_ =posint= |
%	| =file=	| =fname= or =fpipe= |
%	| =fname=	| =text= |
%	| =fpipe=	| =atom= or =string= |
%	| =stream=	| Stream identifier |
%	| =posint=	| =positive_integer= |
%	| =|arith(Type)|=		| Arithmetic expr. that evaluates to _Type_ |
%	| =|var(Type)|=			| =var= or _Type_ |
%	| =|or(Type1, Type2)|=	| _Type1_ or _Type2_ |

:- multifile
	error:has_type/2.

error:has_type(or(Type1, Type2), Term) :-
	(	error:has_type(Type1, Term)
	;	error:has_type(Type2, Term)
	), !.
error:has_type(var(Type), Term) :-
	error:has_type(or(var, Type), Term).
error:has_type(arith(Type), Term) :-
	catch(Val is Term, _, fail),
	error:has_type(Type, Val).
error:has_type(posint, Term) :-
	error:has_type(positive_integer, Term).
error:has_type(stream, Term) :-
	is_stream(Term).
error:has_type(fname, Term) :-
	error:has_type(text, Term).
error:has_type(fpipe,  pipe(Cmd)) :-
	error:has_type(or(atom, string), Cmd).
error:has_type(file, Term) :-
	error:has_type(or(fname, fpipe), Term).
error:has_type(pfact, P^F) :-
	error:has_type(prime, P),
	error:has_type(posint, F).
error:has_type(pmode, Term) :-
	error:has_type(oneof([direct, mem_read, mem_write]), Term).
error:has_type(prime, Term) :-
	\+ \+ prime_test(Term).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile
	user:message_hook/3.

% user:message_hook(prime_mem:fill_begin(_, _), informational, _).
% user:message_hook(prime_mem:fill_end, informational, _).

user:message_hook(prime_whl:init_begin(Lev), silent, _) :-
	print_message(informational, prime_whl:init_begin(Lev)).
user:message_hook(prime_whl:init_next(Lev0), silent, _) :-
	print_message(informational, prime_whl:init_next(Lev0)).
user:message_hook(prime_whl:init_end(Lev0), silent, _) :-
	print_message(informational, prime_whl:init_end(Lev0)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization(prime_whl_init(6), now).				% TODO: Review this! #####

% :- initialization(prime_mem_fill(1, 100_000), now).		% TODO: Review this. #####

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
