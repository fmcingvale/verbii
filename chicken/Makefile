
all: repl min

#CFLAGS=
CFLAGS=-O3
#CFLAGS=-O4
#CFLAGS=-profile

clean:
	rm -f *.o repl *.import.scm *.link PROFILE.*
	
langtypes.o: langtypes.scm
	./local-chicken/bin/csc -c ${CFLAGS} -static -J langtypes.scm -unit langtypes -o langtypes.o

errors.o: errors.scm
	./local-chicken/bin/csc -c ${CFLAGS} -static -J errors.scm -unit errors -o errors.o

deserializer.o: deserializer.scm
	./local-chicken/bin/csc -c ${CFLAGS} -static -J deserializer.scm -unit deserializer -o deserializer.o

interpreter.o: interpreter.scm
	./local-chicken/bin/csc -c ${CFLAGS} -static -J interpreter.scm -unit interpreter -o interpreter.o

native.o: native.scm
	./local-chicken/bin/csc -c ${CFLAGS} -static -J native.scm -unit native -o native.o

# note that compilation order is important here
repl: verbii.scm langtypes.o errors.o interpreter.o deserializer.o native.o 
	./local-chicken/bin/csc -o repl ${CFLAGS} -static langtypes.o deserializer.o native.o interpreter.o -uses langtypes -uses errors -uses deserializer -uses native -uses interpreter verbii.scm

min: min.scm langtypes.o errors.o interpreter.o deserializer.o native.o 
	./local-chicken/bin/csc -o min ${CFLAGS} -static langtypes.o deserializer.o native.o interpreter.o -uses langtypes -uses errors -uses deserializer -uses native -uses interpreter min.scm

