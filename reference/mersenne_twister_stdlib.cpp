// generate expected values for mersenne twister PRNG using
// C++ standard implementation
//
// Copyright (c) 2022 Frank McIngvale, see LICENSE

#include <iostream>
#include <random>
using namespace std;

void run_mt(unsigned int seedval, int skip) {
	std::mt19937 gen;
	cout << "SEED: " << seedval << endl;
 	gen.seed(seedval);
	cout << "SKIP:" << skip << endl;
	for(int i=0; i<skip; ++i) {
		gen();
	}
	
	for(int i=0; i<10; ++i) {
		cout << gen() << endl;
	}
}

int main()
{
	// MT state vector is length 624 so want to run some multiple
	// of that in order to test the twist function at least a few times
	int skip = (int)(624 * 3.7);
	run_mt(3546416464, skip);
	run_mt(3994902905, skip);
	run_mt(4109379183, skip);
	run_mt(4215500625, skip);
	run_mt(908173132, skip);
}