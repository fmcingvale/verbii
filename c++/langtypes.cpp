/*
	LangTypes - type system.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "langtypes.hpp"
#include "errors.hpp"
#include "xmalloc.hpp"
#include <sstream>
using namespace std;

// set maximum portable integer size based on lowest common denominator
// among host languages. currently (even though the port doesn't exist yet)
// this is javascript (not counting bignum, only native numeric types)
const VINT MAX_VINT = (1LL<<53) - 1; // 9007199254740991
const VINT MIN_VINT = -MAX_VINT;     // -9007199254740991

Object NULLOBJ; // default constructor creates the null object
Object VOIDOBJ = newVoid();

// check that int is valid or throw exception
void checkIntOrFail(VINT i) {
	#if 0
	if(i > MAX_VINT || i < MIN_VINT) {
		throw LangError("Integer overflow");
	}
	#endif
}

Object parseInt(const std::string &text) {
	try {
		return newInt(stoll(text));
	}
	catch (const invalid_argument&) {
		return NULLOBJ;
	}
}

Object parseFloat(const std::string &text) {
	try {
		return newFloat(stod(text));
	}
	catch (const invalid_argument&) {
		return NULLOBJ;
	}
}

Object newInt(VINT i) {
	checkIntOrFail(i);
	Object obj;
	obj.type = TYPE_INT;
	obj.data.i = i;
	return obj;
}

Object newNull() {
	Object obj;
	obj.type = TYPE_NULL;
	return obj;
}

Object newVoid() {
	Object obj;
	obj.type = TYPE_VOID;
	return obj;
}

Object newBool(bool b) {
	Object obj;
	obj.type = TYPE_BOOL;
	obj.data.b = b;
	return obj;
}

Object newLambda(ObjList *objlist) {
	Object obj;
	obj.type = TYPE_LAMBDA;
	obj.data.objlist = objlist;
	return obj;
}

Object newList() {
	Object obj;
	obj.type = TYPE_LIST;
	obj.data.objlist = new ObjList();
	return obj;
}

Object newList(ObjList *list) {
	Object obj;
	obj.type = TYPE_LIST;
	obj.data.objlist = list;
	return obj;
}

Object newFloat(double d) {
	Object obj;
	obj.type = TYPE_FLOAT;
	obj.data.d = d;
	return obj;
}

Object newString(const char *s, size_t len, bool keepPointer) {
	Object obj;
	obj.type = TYPE_STRING;
	if(keepPointer) {
		obj.data.str = s;
	}
	else {
		obj.data.str = x_strndup(s, len);
	}
	return obj;
}

Object newString(const string& s) {
	return newString(s.c_str(), s.length());
}

// like above
Object newSymbol(const char *s, size_t len, bool keepPointer) {
	Object obj;
	obj.type = TYPE_SYMBOL;
	if(keepPointer) {
		obj.data.str = s;
	}
	else {
		obj.data.str = x_strndup(s, len);
	}
	return obj;
}

Object newSymbol(const string& s) {
	return newSymbol(s.c_str(), s.length());
}

Object newClosure(ObjList *objlist, Object state) {
	Object obj;
	obj.type = TYPE_CLOSURE;
	obj.data.closure = (Closure*)x_malloc(sizeof(Closure));
	obj.data.closure->objlist = objlist;
	obj.data.closure->state = state;
	return obj;
}

ObjList *Object::asClosureFunc() const { return data.closure->objlist; }
Object Object::asClosureState() const { return data.closure->state; }

bool Object::opEqual(const Object &other) {
	switch(type) {
		case TYPE_NULL: return other.isNull();
		case TYPE_INT: return (other.type == TYPE_INT && other.data.i == data.i) ||
							(other.type == TYPE_FLOAT && other.data.d == data.i);
		case TYPE_FLOAT: return (other.type == TYPE_FLOAT && other.data.d == data.d) ||
							(other.type == TYPE_INT && other.data.i == data.d);
		case TYPE_BOOL: return other.type == TYPE_BOOL && other.data.b == data.b;
		case TYPE_LAMBDA: return false; // lambdas never equal any other object, even themselves
		case TYPE_CLOSURE: return false; // same for closures
		case TYPE_STRING: return other.type == TYPE_STRING && !strcmp(data.str,other.data.str);
		case TYPE_SYMBOL: return other.type == TYPE_SYMBOL && !strcmp(data.str,other.data.str);
		// lists are deep compared, via opEqual at each element
		case TYPE_LIST:	
			if(other.type != TYPE_LIST)
				return false;

			if(data.objlist->size() != other.data.objlist->size())
				return false;

			for(size_t i=0; i<data.objlist->size(); ++i) {
				if(!data.objlist->at(i).opEqual(other.data.objlist->at(i)))
					return false;
			}
			return true;

		default: 
			// i WANT to crash when I forget to add a new type ...
			throw LangError("Unsupported type in == : " + fmtStackPrint());
	}
}

bool Object::opGreater(const Object &other) {
	switch(type) {
		case TYPE_INT:
			if (other.type == TYPE_INT) return data.i > other.data.i;
			if (other.type == TYPE_FLOAT) return data.i > other.data.d;
			break;
		case TYPE_FLOAT: 
			if (other.type == TYPE_INT) return data.d > other.data.i;
			if (other.type == TYPE_FLOAT) return data.d > other.data.d;
			break;
		case TYPE_STRING:
			if (other.type == TYPE_STRING) return strcmp(data.str, other.data.str) > 0;
			break;
		case TYPE_SYMBOL:
			if (other.type == TYPE_SYMBOL) return strcmp(data.str, other.data.str) > 0;
			break;
		case TYPE_LIST:
		{
			if(other.type != TYPE_LIST)
				break;

			// like a string test, but on elements of list
			// its an error for elements to not be of the same types

			// loop over min of lengths
			size_t nr = min(data.objlist->size(), other.data.objlist->size());

			for(size_t i=0; i<nr; ++i) {
				if(data.objlist->at(i).opGreater(other.data.objlist->at(i))) {
					// found first element where self>other, so entire test is true
					return true;
				}
				else if(!data.objlist->at(i).opEqual(other.data.objlist->at(i))) {
					// !greater && !equal, so other[i] is less, so entire test is false
					return false;
				}
				// else self[i] == other[i]; continue with next element
			}
			// reaching this point means all are equal up to nr elements, so:
			// if self longer than other, self is greater
			if(data.objlist->size() > other.data.objlist->size())
				return true;
			// else other is longer or equal, so self is not greater
			else
				return false;	
		}
	}
	throw LangError("Cannot compare objects in >: " + fmtStackPrint() + " and " + other.fmtStackPrint());
}

bool isNumber(const Object &a) {
	return a.type == TYPE_INT || a.type == TYPE_FLOAT;
}

// get int or float as double
double castFloat(const Object &a) { 
	if(a.type == TYPE_INT) { 
		return (double)(a.data.i); 
	}
	else if(a.type == TYPE_FLOAT) {
		return a.data.d;
	}
	else {
		throw LangError("Cannot cast to float: " + a.fmtStackPrint());
	}
}

Object Object::opAdd(const Object &other) {
	// any cases that aren't handled fall through to single throw at end
	switch(type) {
		case TYPE_NULL: break;
		case TYPE_INT:
			if(other.type == TYPE_INT) {
				return newInt(data.i + other.data.i);
			}
			else if(other.type == TYPE_FLOAT) {
				return newFloat(data.i + other.data.d);
			}
			break;
		case TYPE_FLOAT:
			if(other.type == TYPE_INT) {
				return newFloat(data.d + other.data.i);
			}
			else if(other.type == TYPE_FLOAT) {
				return newFloat(data.d + other.data.d);
			}
			break;
		case TYPE_BOOL: break;
		case TYPE_LAMBDA: break;
		case TYPE_CLOSURE: break;
		// strings & symbols defined as immutable, so no changing this
		case TYPE_STRING:
			if(other.type == TYPE_STRING) {
				string s = this->data.str;
				s += other.data.str;
				return newString(s);
			}	
			break;
		case TYPE_SYMBOL:
			if(other.type == TYPE_SYMBOL) {
				string s = this->data.str;
				s += other.data.str;
				return newSymbol(s);
			}
			break;
		case TYPE_LIST:
			if(other.type == TYPE_LIST) {
				Object newlist = newList();
				for(auto obj : *data.objlist) {
					newlist.data.objlist->push_back(obj);
				}
				for(auto obj : *other.data.objlist) {
					newlist.data.objlist->push_back(obj);
				}
				return newlist;
			}
			break;

		default: 
			break; // just to be explicit that I mean to fall through
	}
			
	throw LangError("Bad operands for +: " + this->fmtDisplay() + " & " + other.fmtDisplay());
}

Object Object::opSubtract(const Object &other) {
	if(type == TYPE_INT && other.type == TYPE_INT) {
		return newInt(data.i - other.data.i);
	}
	else if(isNumber(*this) && isNumber(other)) {
		return newFloat(castFloat(*this) - castFloat(other));
	}
	throw LangError("Bad operands for -: " + this->fmtDisplay() + " & " + other.fmtDisplay());
}

Object Object::opMul(const Object &other) {
	if(type == TYPE_INT && other.type == TYPE_INT) {
		return newInt(data.i * other.data.i);
	}
	else if(isNumber(*this) && isNumber(other)) {
		return newFloat(castFloat(*this) * castFloat(other));
	}
	throw LangError("Bad operands for *: " + this->fmtDisplay() + " & " + other.fmtDisplay());
}

Object Object::opDivide(const Object &other) {
	if(isNumber(*this) && isNumber(other)) {
		double denom = castFloat(other);
		if(denom == 0) {
			throw LangError("Divide by zero");
		}
		return newFloat(castFloat(*this) / denom);
	}
	throw LangError("Bad operands for /: " + this->fmtDisplay() + " & " + other.fmtDisplay());
}

Object Object::opLength() {
	switch(type) {
		case TYPE_STRING:
		case TYPE_SYMBOL:
			return newInt(strlen(data.str));
		case TYPE_LIST:
			return newInt((int)(data.objlist->size()));
	}
	throw LangError("'length' not supported for object: " + fmtStackPrint());
}

Object Object::opSlice(VINT index, VINT nr) {
	int objsize;
	switch(type) {
		case TYPE_STRING:
		case TYPE_SYMBOL:
			objsize = strlen(data.str);
			break;

		case TYPE_LIST:
			objsize = (int)(data.objlist->size());
			break;

		default:
			throw LangError("Object doesn't support slicing: " + fmtStackPrint());
	}

	// adjust index & nr for negative & out of bounds conditions
	if(index < 0) { // index < 0 means count from end
 		index = objsize + index;
	}
	if(index < 0 || index >= objsize) { // out of bounds - return empty object
		if(type == TYPE_STRING || type == TYPE_SYMBOL)
			return newString("");
		else if(type == TYPE_LIST)
			return newList();
		else {
			throw LangError("Should never happen");
		}
	}
	if(nr < 0) { // nr < 0 means "copy all, starting at index"
		nr = objsize - index;
	}
	if((index+nr) > objsize) { // past end of object, truncate
		nr = objsize - index;
	}

	switch(type) {
		case TYPE_STRING: return newString(x_strndup(data.str+index,nr), 0, true); // give away pointer
		case TYPE_SYMBOL: return newSymbol(x_strndup(data.str+index,nr), 0, true); // give away pointer
		case TYPE_LIST:
			{
				Object r = newList();
				for(auto it=data.objlist->begin() + index;
						it < (data.objlist->begin() + index + nr);
						++it) 
					r.data.objlist->push_back(*it);

				return r;
			}
	}	
	throw LangError("Unreachable code");	
}

int FLOAT_PRECISION = 17;

/*
	"display printing" shows values in an undecorated format, so comparing
	with stack-printing:
		- floats DO NOT get a '#' prefix
		- strings are shown WITHOUT quotes
		- symbols are shown as strings
		- true/false do not get < .. >
*/
static string fmtDisplayObjList(ObjList *objlist, string open_delim, string close_delim) {
	string s = open_delim + " ";
	for(auto obj : *objlist) {
		s += obj.fmtDisplay() + " ";
	}
	s += close_delim;
	return s;
}

string Object::fmtDisplay() const {
	switch(type) {
		case TYPE_NULL: return "<null>";
		case TYPE_VOID: return "<*VOID*>";
		case TYPE_INT: return to_string(data.i);
		case TYPE_FLOAT: 
		{
			char buf[40];
			snprintf(buf, 39, "%.*g", FLOAT_PRECISION, data.d);
			return string(buf);
		}
		case TYPE_BOOL: return data.b ? "true" : "false";
		// these two are not meant to be printed, but can be shown in stack
		case TYPE_LAMBDA:
			return "<" + fmtDisplayObjList(data.objlist, "{", "}") + ">";
		case TYPE_CLOSURE:
			return "<" + fmtDisplayObjList(data.closure->objlist, "{", "}") +
				" :: " + data.closure->state.fmtDisplay() + ">";
		case TYPE_STRING: return string(data.str);
		case TYPE_SYMBOL: return string(data.str);
		case TYPE_LIST: return fmtDisplayObjList(data.objlist, "[", "]");

		default: throw LangError("str not implemented for this object type" + to_string(type));
	}
}

/*
 	"stack printing" shows objects in a way that their type can be deduced.
	 a common use for stack printing is debugging, so being able to easily 
	 distinguish types is important.
	 
 	specifically this means:
		- floats get a '#' prefix to distinguish them from integers
		- strings are shown as "..."
		- symbols get a ' prefix
		- true/false become <true>, <false> to distinguish them from symbols
*/
static string fmtStackPrintObjList(ObjList *objlist, string open_delim, string close_delim) {
	string s = open_delim + " ";
	for(auto obj : *objlist) {
		s += obj.fmtStackPrint() + " ";
	}
	s += close_delim;
	return s;
}

string Object::fmtStackPrint() const {
	switch(type) {
		case TYPE_NULL: return "<null>";
		case TYPE_VOID: return "<*VOID*>";
		case TYPE_INT: return to_string(data.i);
		case TYPE_FLOAT: 
		{
			char buf[40];
			// include # so type is clear
			snprintf(buf, 39, "#%.*g", FLOAT_PRECISION, data.d);
			return string(buf);
		}
		case TYPE_BOOL: return data.b ? "<true>" : "<false>";
		case TYPE_LAMBDA:
			return "<" + fmtStackPrintObjList(data.objlist, "{", "}") + ">";
		case TYPE_CLOSURE:
			return "<" + fmtStackPrintObjList(data.closure->objlist, "{", "}") +
				" :: " + data.closure->state.fmtStackPrint() + ">";
		// add " .. " so its clear on stack that it is a string
		case TYPE_STRING: return "\"" + string(data.str) + "\"";
		// opposite of above -- since strings get " .. " then i don't get a ' here
		case TYPE_SYMBOL: return "'" + string(data.str);
		case TYPE_LIST: return fmtStackPrintObjList(data.objlist, "[", "]");
		default: throw LangError("repr not implemented for object type " + to_string(type));
	}
}

ObjList *deepcopy(ObjList *objlist) {
	ObjList *newlist = new ObjList();
	for(auto obj : *objlist)
		newlist->push_back(obj.deepcopy());

	return newlist;
}

Object Object::deepcopy() {
	switch(type) {
		case TYPE_NULL:
		case TYPE_VOID:
		case TYPE_INT:
		case TYPE_FLOAT: 
		case TYPE_BOOL:
		case TYPE_LAMBDA:
		case TYPE_CLOSURE:
		case TYPE_STRING:
		case TYPE_SYMBOL:
			return *this;

		case TYPE_LIST:
			return newList(::deepcopy(data.objlist));
			
		default: throw LangError("deepcopy not implemented for object type " + to_string(type));
	}
}
