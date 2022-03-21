
all: repl

clean:
	rm -f *.o repl *.import.scm *.link
	
langtypes.o: langtypes.scm
	./local-chicken/bin/csc -c -static -J langtypes.scm -unit langtypes -o langtypes.o

deserializer.o: deserializer.scm
	./local-chicken/bin/csc -c -static -J deserializer.scm -unit deserializer -o deserializer.o

interpreter.o: interpreter.scm
	./local-chicken/bin/csc -c -static -J interpreter.scm -unit interpreter -o interpreter.o

native.o: native.scm
	./local-chicken/bin/csc -c -static -J native.scm -unit native -o native.o

repl: verbii.scm langtypes.o deserializer.o interpreter.o native.o 
	./local-chicken/bin/csc -o repl -static langtypes.o deserializer.o native.o interpreter.o -uses langtypes -uses deserializer -uses native -uses interpreter verbii.scm


