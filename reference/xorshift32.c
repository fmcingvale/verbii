/*
	From:
		https://en.wikipedia.org/wiki/Xorshift
		
	Additional test code by Frank McIngvale added at the end.
*/

#include <stdint.h>

struct xorshift32_state {
  uint32_t a;
};

/* The state word must be initialized to non-zero */
uint32_t xorshift32(struct xorshift32_state *state)
{
	/* Algorithm "xor" from p. 4 of Marsaglia, "Xorshift RNGs" */
	uint32_t x = state->a;
	x ^= x << 13;
	x ^= x >> 17;
	x ^= x << 5;
	return state->a = x;
}

// ===== the remainder of this file is not in the original ... =====
#include <stdio.h>

void do_test(uint32_t seed, int skip) {
	struct xorshift32_state state;
	state.a = seed;
	printf("SEED: %u\n", seed);
	// skip a number of values in order to allow any errors to
	// propogate a while
	printf("SKIP: %d\n", skip);
	for(int i=0; i<skip; ++i) {
		xorshift32(&state);
	}
	for(int i=0; i<10; ++i) {
		printf("%u\n", xorshift32(&state));
	}
}

int main(void) {
	do_test(272011914U, 2937);
	do_test(640321664U, 2937);
	do_test(4003650979U, 2937);
	do_test(3216424659U, 2937);	
}