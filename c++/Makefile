
LIB_SRC = reader.cpp interpreter.cpp native.cpp langtypes.cpp xmalloc.cpp
HEADERS = reader.hpp interpreter.hpp native.hpp langtypes.hpp xmalloc.hpp

LIB_OBJS = $(LIB_SRC:.cpp=.o)

TEST_SRC = test_reader.cpp

# uncomment one set of GC_FLAGS/GC_LIBS to enable/disable GC
#GC_FLAGS = 
#GC_LIBS = 

GC_FLAGS = -DUSE_GCMALLOC
GC_LIBS = -lgc -lgccpp

CFLAGS = -g $(GC_FLAGS)
#CFLAGS = -pg $(GC_FLAGS)
# max optimization that works without crashing due to GC
#CFLAGS = -Os -DNDEBUG $(GC_FLAGS)

all: test.exe repl.exe # gctest.exe

%.o: %.cpp $(HEADERS)
	g++ $(CFLAGS) -Wall -c $< -o $@

mineylib.a: $(LIB_OBJS)
	ar rvs mineylib.a $(LIB_OBJS)

clean:
	rm -f *.exe gmon.out *.o *.a

test.exe: test.cpp $(TEST_SRC) mineylib.a
	g++ $(CFLAGS) -Wall -o test test.cpp $(TEST_SRC) mineylib.a $(GC_LIBS)

repl.exe: repl.cpp mineylib.a
	g++ $(CFLAGS) -Wall -o repl repl.cpp mineylib.a $(GC_LIBS)

gctest.exe: gctest.cpp
	g++ $(CFLAGS) -Wall -o gctest gctest.cpp $(GC_LIBS)
