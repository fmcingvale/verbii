
all: mersenne_twister_stdlib xoshiro128plusplus xoshiro128starstar xorshift32

CFLAGS=

%.o: %.cpp
	g++ $(CFLAGS) -Wall -c $< -o $@

clean:
	rm -f *.exe gmon.out *.o *.a mersenne_twister_stdlib xoshiro128plusplus

mersenne_twister_stdlib: mersenne_twister_stdlib.cpp
	g++ $(CFLAGS) -Wall -o mersenne_twister_stdlib mersenne_twister_stdlib.cpp

xoshiro128plusplus: xoshiro128plusplus.c
	gcc $(CFLAGS) -Wall -o xoshiro128plusplus xoshiro128plusplus.c

xoshiro128starstar: xoshiro128starstar.c
	gcc $(CFLAGS) -Wall -o xoshiro128starstar xoshiro128starstar.c

xorshift32: xorshift32.c
	gcc $(CFLAGS) -Wall -o xorshift32 xorshift32.c

