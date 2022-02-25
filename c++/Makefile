
LIB_SRC = reader.cpp interpreter.cpp native.cpp langtypes.cpp xmalloc.cpp syntax.cpp deserialize.cpp
HEADERS = reader.hpp interpreter.hpp native.hpp langtypes.hpp xmalloc.hpp syntax.hpp deserialize.hpp

LIB_OBJS = $(LIB_SRC:.cpp=.o)

TEST_SRC = test_reader.cpp

# uncomment one set of GC_FLAGS/GC_LIBS to enable/disable GC
#GC_FLAGS = 
#GC_LIBS = 

GC_FLAGS = -DUSE_GCMALLOC
GC_LIBS = -lgc -lgccpp

CFLAGS = -g $(GC_FLAGS)
#CFLAGS = -pg $(GC_FLAGS)
# max optimization that works under mingw-x64 (gcc 11)
#CFLAGS = -Os -DNDEBUG $(GC_FLAGS)
# this optimization level works fine under Ubuntu
#CFLAGS = -O3 -DNDEBUG $(GC_FLAGS)

all: repl.exe  # test.exe gctest.exe

%.o: %.cpp $(HEADERS)
	g++ $(CFLAGS) -Wall -c $< -o $@

verbii.a: $(LIB_OBJS)
	ar rvs verbii.a $(LIB_OBJS)

clean:
	rm -f *.exe gmon.out *.o *.a repl test

test.exe: test.cpp $(TEST_SRC) verbii.a
	g++ $(CFLAGS) -Wall -o test test.cpp $(TEST_SRC) verbii.a $(GC_LIBS)

repl.exe: repl.cpp verbii.a
	g++ $(CFLAGS) -Wall -o repl repl.cpp verbii.a $(GC_LIBS)

gctest.exe: gctest.cpp
	g++ $(CFLAGS) -Wall -o gctest gctest.cpp $(GC_LIBS)
