
all: main

CFLAGS=-verbose
#CFLAGS=-O3
#CFLAGS=-O4
#CFLAGS=-profile

CSC=/usr/local/chicken/bin/csc

clean:
	rm -f *.o *.obj main *.import.scm *.link PROFILE.*
	
langtypes.o: langtypes.scm
	${CSC} -c ${CFLAGS} -static -J langtypes.scm -o langtypes.o

errors.o: errors.scm
	${CSC} -c ${CFLAGS} -static -J errors.scm -o errors.o

deserializer.o: deserializer.scm
	${CSC} -c ${CFLAGS} -static -J deserializer.scm -o deserializer.o

interpreter.o: interpreter.scm
	${CSC} -c ${CFLAGS} -static -J interpreter.scm -o interpreter.o

native.o: native.scm
	${CSC}  -c ${CFLAGS} -static -J native.scm -o native.o

# note that compilation order is important here
main: main.scm langtypes.o errors.o interpreter.o deserializer.o native.o 
	${CSC} -o main ${CFLAGS} -static langtypes.o deserializer.o native.o interpreter.o main.scm

