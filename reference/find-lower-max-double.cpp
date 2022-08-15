
// test code i used to find a value below the max double that chicken would
// print as a valid double
#include <stdint.h>
#include <stdio.h>

int main(void) {
	double V = 1.7976931348623157E+308;
	//double V = 1.79769313486231E+308;
	uint64_t *M = (uint64_t*)(&V);
	uint32_t H = ((*M) >> 32) & 0xffffffff;
	uint32_t L = (*M) & 0xffffffff;
	printf("High: %x\n", H);
	printf("Low: %x\n", L);
	printf("%.17g\n", V);

	*M = 0x7feffffffffffffb;
	printf("%.17g\n", V);

}
