
LIB_SRC = interpreter.cpp native.cpp langtypes.cpp xmalloc.cpp deserialize.cpp opcodes.cpp util.cpp
HEADERS = interpreter.hpp native.hpp langtypes.hpp xmalloc.hpp deserialize.hpp opcodes.hpp util.hpp

LIB_OBJS = $(LIB_SRC:.cpp=.o)

ifdef REPL_ID # don't try to use libgc on repl.it
NO_GC = 1
endif

# run like "env NO_GC=1 make" to not use libgc without having to edit Makefile
ifdef NO_GC
GC_FLAGS =
GC_LIBS =
else
# for other platforms ...
# uncomment one set of GC_FLAGS/GC_LIBS to enable/disable GC
#GC_FLAGS = 
#GC_LIBS = 

GC_FLAGS = -DUSE_GCMALLOC
GC_LIBS = -lgccpp -lgc
endif

# uncomment one CFLAGS
# safe, slow, debugging build
#CFLAGS = -g $(GC_FLAGS)
#CFLAGS = -pg $(GC_FLAGS)
# max optimization that works under mingw-x64 (gcc 11/12)
CFLAGS = -Os -DNDEBUG $(GC_FLAGS)
# this optimization level works fine under Ubuntu
#CFLAGS = -O3 -DNDEBUG $(GC_FLAGS)

all: main

%.o: %.cpp $(HEADERS)
	g++ $(CFLAGS) -Wall -c $< -o $@

libverbii.a: $(LIB_OBJS)
	ar rvs libverbii.a $(LIB_OBJS)

clean:
	rm -rf main main.exe gmon.out *.o *.a x64

main: main.cpp libverbii.a
	g++ $(CFLAGS) -Wall -o main main.cpp libverbii.a $(GC_LIBS)
