
all: repl

clean:
	rm -f *.o repl *.import.scm *.link
	
langtypes.o: langtypes.scm
	./local-chicken/bin/csc -c -static -J langtypes.scm -unit langtypes -o langtypes.o

deserializer.o: deserializer.scm
	./local-chicken/bin/csc -c -static -J deserializer.scm -unit deserializer -o deserializer.o

repl: verbii.scm langtypes.o deserializer.o
	./local-chicken/bin/csc -o repl -static langtypes.o deserializer.o -uses langtypes -uses deserializer verbii.scm


