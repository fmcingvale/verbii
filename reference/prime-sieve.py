"""
	Generates expected values for prime-sieve tests.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
"""

# fnv1a implemented from:
# https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
def fnv1a(text):
	PRIME = 16777619
	OFFSET = 2166136261
	hash = OFFSET
	for b in text:
		hash = hash ^ b
		hash = hash * PRIME
		hash = hash & 0xffffffff

	return hash

# this matches the algorithm in the verbii version
def prime_sieve(nr):
	from math import sqrt,floor
	flags = [True] * (nr+1)
	for i in range(2, floor(sqrt(nr)), 1):
		j = 2
		while(i*j <= nr):
			flags[i*j] = False
			j += 1

	primes = []
	for i in range(2,nr+1,1):
		if flags[i]:
			primes.append(i)

	return primes

p = prime_sieve(10000)
print("Found {0} primes".format(len(p)))
print("FINAL:",p[-1])
s = (" ".join(str(n) for n in p)).encode('ascii')
#print(s)
print("Text len:",len(s))
import zlib
print("CRC: {0:x}".format(zlib.crc32(s)))
print("FNV1a: {0:x}".format(fnv1a(s)))

# quick sanity check of fnv1a from expected value
#print("EXPECT: {0} = {1:x}".format("7a78f512", fnv1a(b"Hello world!")))
	

    
            
    