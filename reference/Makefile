
all: mersenne_twister_stdlib

CFLAGS=

%.o: %.cpp
	g++ $(CFLAGS) -Wall -c $< -o $@

clean:
	rm -f *.exe gmon.out *.o *.a

mersenne_twister_stdlib: mersenne_twister_stdlib.cpp
	g++ $(CFLAGS) -Wall -o mersenne_twister_stdlib mersenne_twister_stdlib.cpp
	

