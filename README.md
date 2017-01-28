# A Simple Prime Number Library in Prolog

Nan.Numerics.Primes
Nan.Numerics.Primes/Prolog 1.3.0-beta
A Simple Prime Number Library in Prolog
Copyright 2016-2017 Julio P. Di Egidio
Licensed under GNU GPLv3.
http://julio.diegidio.name/Projects/Nan.Numerics.Primes/
https://github.com/jp-diegidio/Nan.Numerics.Primes-Prolog/

=|library(nan_numerics_primes)|=

Module =prime= provides predicates to test (positive integer) numbers for
primality, find divisors and factor numbers, generate prime numbers in some
interval, find consecutive prime numbers, and save/load all prime numbers
up to some value to/from a file or stream.

All predicates in module =prime= are _safe_, i.e. validate input arguments
and ensure steadfastness.  For maximum performance, user code can directly
call the _unsafe_ =public= (not exported) predicates in module =prime_lgc=.

Implements a variant of the *Miller-Rabin* primality test that is
_deterministic_ for numbers up to =3317044064679887385961980=, otherwise
it is _probabilistic_ with the number of iterations fixed at =20=.

For better performance, leverages a prime wheel of level =6=, i.e.
generated by the first =6= prime numbers, and thread-local memoization of
pairs of consecutive prime numbers.

*NOTE*: Since the primality test in use is _probabilistic_ in general, this
library is not suitable for cryptographic applications.

This library was developed and tested with:
SWI-Prolog 7.3.25 - http://www.swi-prolog.org/

Usage example:

    ==
    ?- pack_install(nan_numerics_primes).
    true.
    
    ?- use_module(library(nan_numerics_primes)).
    true.

    ?- time(prime_right(1234567891012345678901234567890123456789011111, P)).
    % 1,227 inferences, 0.000 CPU in 0.010 seconds (0% CPU, Infinite Lips)
    P = 1234567891012345678901234567890123456789011139.
    ==

To be done: Implement parallel factoring functions.
To be done: Implement probabilitic test error estimates?
To be done: Implement option for num. of probabilistic iterations?
To be done: Implement prime counting/n-th prime functions.
To be done: Implement deterministic tests?
To be done: Improve compatibility with other Prolog systems.
