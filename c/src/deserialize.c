/*
	Deserialize - load bytecode produced by compiler.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "deserialize.h"
#include "errors.h"

// NOTE: these routines use Strings internally, which are auto-collected, just have to make
// sure to either use strings or strdup() them before returning to interpreter.

// returns empty string on EOF
static const char* next_line(FILE *fp) {
	Object *line = newString("",0);
	// skip all \r, \n from previous line
	int c;
	while(1) {
		c = fgetc(fp);
		if(c == EOF || (c != '\r' && c != '\n'))
			break;
	}
	// carrover c from above loop
	while(c != EOF && c != '\r' && c != '\n') {
		string_addchar(line, c);
		c = fgetc(fp);
	}
	// serialized files are never binary, so this fine
	return string_cstr(line);
}

static const char* unescape_string(const char *src) {
	Object *out = newString("",0);
	while(*src != 0) {
		if(!strncmp(src, "%32", 3)) {
			string_addchar(out, ' ');
			src += 3;
		}
		else if(!strncmp(src, "%09", 3)) {
			string_addchar(out, '\t');
			src += 3;
		}
		else if(!strncmp(src, "%10", 3)) {
			string_addchar(out, '\n');
			src += 3;
		}
		else if(!strncmp(src, "%13", 3)) {
			string_addchar(out, '\r');
			src += 3;
		}
		else if(!strncmp(src, "%37", 3)) {
			string_addchar(out, '%');
			src += 3;
		}
		else {
			string_addchar(out, *src);
			++src;
		}
	}
	return string_cstr(out);
}
	
// see notes in C++ port

// deserialize & return next object from stream
Object *deserialize_stream(FILE *fp) {
	const char *line = next_line(fp);
	if(next_line) {
		switch(line[0]) {
			case 'M': 
				// metadata line - ignore and return next object
				return deserialize_stream(fp);
			case 'i': return parseInt(line+2);
			case 'f': return parseFloat(line+2);
			case 'b': return parseBool(line+2);
			case 'n': return newNull();
			case 'o': return newOpcode(parseInt(line+2)->data.i);
			case 's': return newString(unescape_string(line+2),-1);
			case 'y': return newSymbol(line+2,-1);
			case 'L': // list
				{
					VINT nr = parseInt(line+2)->data.i;
					Object *list = newList();
					// read next nr objects into list
					for(VINT i=0; i<nr; ++i)
						List_append(list, deserialize_stream(fp));

					return list;
				}
			case 'F': // lambda
				{
					Object *list = deserialize_stream(fp);
					if(!isList(list))
						error("Expecting list after F but got: %s", fmtStackPrint(list));

					return newLambda(list);
				}
			case 'W': // word definition
				{
					const char *name = line+2;
					Object *list = deserialize_stream(fp);
					if(!isList(list))
						error("Expecting list after W but got: %s", fmtStackPrint(list));

					// do not allow overwriting words when deserializing
					//printf("DEFINE WORD: %s\n", name);
					defineWord(name, list, FALSE);
					// this produces nothing extra
					return newVoid();
				}

			default:
				// this is a fatal error, so just print & exit -- trying to do an exception
				// makes the output weird
				printf("Unrecogized line in deserialize: %s\n", line);
				exit(1);
		}
	}
	return newVoid();
}
	