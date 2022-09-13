/*
	This is xoroshiro128starstar.c from:
		https://prng.di.unimi.it/xoshiro128starstar.c

	I made these changes:
		* Removed jump() and long_jump() since I don't need any
		massively parallel processing for verbii 
		* Added main() to generate test values for verbii unittests.

	Changes by Frank McIngvale, under same copyright as original.
*/

/*  Written in 2018 by David Blackman and Sebastiano Vigna (vigna@acm.org)

To the extent possible under law, the author has dedicated all copyright
and related and neighboring rights to this software to the public domain
worldwide. This software is distributed without any warranty.

See <http://creativecommons.org/publicdomain/zero/1.0/>. */

#include <stdint.h>

/* This is xoshiro128** 1.1, one of our 32-bit all-purpose, rock-solid
   generators. It has excellent speed, a state size (128 bits) that is
   large enough for mild parallelism, and it passes all tests we are aware
   of.

   Note that version 1.0 had mistakenly s[0] instead of s[1] as state
   word passed to the scrambler.

   For generating just single-precision (i.e., 32-bit) floating-point
   numbers, xoshiro128+ is even faster.

   The state must be seeded so that it is not everywhere zero. */


static inline uint32_t rotl(const uint32_t x, int k) {
	return (x << k) | (x >> (32 - k));
}


static uint32_t s[4];

uint32_t next(void) {
	const uint32_t result = rotl(s[1] * 5, 7) * 9;

	const uint32_t t = s[1] << 9;

	s[2] ^= s[0];
	s[3] ^= s[1];
	s[1] ^= s[2];
	s[0] ^= s[3];

	s[2] ^= t;

	s[3] = rotl(s[3], 11);

	return result;
}

// the remainder of this file is not in the original ...
#include <stdio.h>

void seed(uint32_t vals[4]) {
	s[0] = vals[0];
	s[1] = vals[1];
	s[2] = vals[2];
	s[3] = vals[3];
}

void do_test(uint32_t seedvals[4], int skip) {
	printf("SEED: ");
	for(int i=0; i<4; ++i) {
		printf("%u ", seedvals[i]);
	}
	printf("\n");
	seed(seedvals);
	// skip a number of values in order to allow any errors to
	// propogate a while
	printf("SKIP: %d\n", skip);
	for(int i=0; i<skip; ++i) {
		next();
	}
	for(int i=0; i<10; ++i) {
		printf("%u\n", next());
	}
}

int main(void) {
	uint32_t vals1[4] = {272011914U,1024715103U,86178555U,2213311456U};
	uint32_t vals2[4] = {4128913325U,2789112053U,2542547078U,4003650979U};
	uint32_t vals3[4] = {3320410395U,1737589964U,2730127134U,4025121456U};
	uint32_t vals4[4] = {1009041585U,640321664U,227863596U,3216424659U};

	do_test(vals1, 2937);
	do_test(vals2, 2937);
	do_test(vals3, 2937);
	do_test(vals4, 2937);	
}