
LIB_SRC = interpreter.cpp native.cpp langtypes.cpp xmalloc.cpp deserialize.cpp
HEADERS = interpreter.hpp native.hpp langtypes.hpp xmalloc.hpp deserialize.hpp

LIB_OBJS = $(LIB_SRC:.cpp=.o)

# uncomment one set of GC_FLAGS/GC_LIBS to enable/disable GC
#GC_FLAGS = 
#GC_LIBS = 

GC_FLAGS = -DUSE_GCMALLOC
GC_LIBS = -lgc -lgccpp

# uncomment one CFLAGS
# safe, slow, debugging build
#CFLAGS = -g $(GC_FLAGS)
#CFLAGS = -pg $(GC_FLAGS)
# max optimization that works under mingw-x64 (gcc 11)
#CFLAGS = -Os -DNDEBUG $(GC_FLAGS)
# this optimization level works fine under Ubuntu
CFLAGS = -O3 -DNDEBUG $(GC_FLAGS)

all: verbii

%.o: %.cpp $(HEADERS)
	g++ $(CFLAGS) -Wall -c $< -o $@

libverbii.a: $(LIB_OBJS)
	ar rvs libverbii.a $(LIB_OBJS)

clean:
	rm -f *.exe gmon.out *.o *.a verbii

verbii: main.cpp libverbii.a
	g++ $(CFLAGS) -Wall -o verbii main.cpp libverbii.a $(GC_LIBS)
