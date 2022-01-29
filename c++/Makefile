
LIB_SRC = reader.cpp interpreter.cpp native.cpp tagging.cpp
HEADERS = reader.hpp interpreter.hpp native.hpp tagging.hpp

LIB_OBJS = $(LIB_SRC:.cpp=.o)

TEST_SRC = test_reader.cpp

#CFLAGS = -g 
#CFLAGS = -pg
CFLAGS = -O3 -DNDEBUG

all: test.exe repl.exe

%.o: %.cpp $(HEADERS)
	g++ $(CFLAGS) -Wall -c $< -o $@

mineylib.a: $(LIB_OBJS)
	ar rvs mineylib.a $(LIB_OBJS)

clean:
	rm -f *.exe gmon.out *.o *.a

test.exe: test.cpp $(TEST_SRC) mineylib.a
	g++ $(CFLAGS) -Wall -o test test.cpp $(TEST_SRC) mineylib.a

repl.exe: repl.cpp mineylib.a
	g++ $(CFLAGS) -Wall -o repl repl.cpp mineylib.a
