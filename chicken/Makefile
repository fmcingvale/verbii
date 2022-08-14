
all: main

#CFLAGS=
#CFLAGS=-verbose
CFLAGS=-O3
#CFLAGS=-O4
#CFLAGS=-profile

CSC=/usr/local/chicken/bin/csc

clean:
	rm -f *.o *.obj main *.import.scm *.link PROFILE.*

main: main.scm langtypes.scm errors.scm interpreter.scm deserializer.scm native.scm
	${CSC} -o main ${CFLAGS} -static langtypes.scm errors.scm deserializer.scm native.scm interpreter.scm main.scm
