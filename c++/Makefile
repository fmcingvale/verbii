
LIB_SRC = reader.cpp interpreter.cpp native.cpp langtypes.cpp
HEADERS = reader.hpp interpreter.hpp native.hpp langtypes.hpp

LIB_OBJS = $(LIB_SRC:.cpp=.o)

TEST_SRC = test_reader.cpp

CFLAGS = -g
#CFLAGS = -g -O2 -fno-align-functions -fno-align-jumps -fno-align-labels -fno-align-loops -fno-optimize-strlen

#CFLAGS = -pg
#CFLAGS = -Os -DNDEBUG

all: test.exe repl.exe gctest.exe

%.o: %.cpp $(HEADERS)
	g++ $(CFLAGS) -Wall -c $< -o $@

mineylib.a: $(LIB_OBJS)
	ar rvs mineylib.a $(LIB_OBJS)

clean:
	rm -f *.exe gmon.out *.o *.a

test.exe: test.cpp $(TEST_SRC) mineylib.a
	g++ $(CFLAGS) -Wall -o test test.cpp $(TEST_SRC) mineylib.a -lgc -lgccpp

repl.exe: repl.cpp mineylib.a
	g++ $(CFLAGS) -Wall -o repl repl.cpp mineylib.a -lgc -lgccpp

gctest.exe: gctest.cpp
	g++ $(CFLAGS) -Wall -o gctest gctest.cpp -lgc -lgccpp
