
all: mersenne_twister_stdlib xoshiro128plusplus

CFLAGS=

%.o: %.cpp
	g++ $(CFLAGS) -Wall -c $< -o $@

clean:
	rm -f *.exe gmon.out *.o *.a mersenne_twister_stdlib xoshiro128plusplus

mersenne_twister_stdlib: mersenne_twister_stdlib.cpp
	g++ $(CFLAGS) -Wall -o mersenne_twister_stdlib mersenne_twister_stdlib.cpp

xoshiro128plusplus: xoshiro128plusplus.c
	gcc $(CFLAGS) -Wall -o xoshiro128plusplus xoshiro128plusplus.c
	

