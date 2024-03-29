/*
	LangTypes - type system.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "langtypes.hpp"
#include "errors.hpp"
#include "xmalloc.hpp"
#include <sstream>
#include <iostream>
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

Object parseBool(const std::string &text) {
	if(text == "true")
		return newBool(true);
	else if(text == "false")
		return newBool(false);
	else
		throw LangError("Bad boolean literal: " + text);
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

Object newDict() {
	Object obj;
	obj.type = TYPE_DICT;
	obj.data.objdict = new ObjDict();
	return obj;
}

Object newFloat(double d) {
	Object obj;
	obj.type = TYPE_FLOAT;
	obj.data.d = d;
	return obj;
}

Object newString(const char *s, size_t len) {
	Object obj;
	obj.type = TYPE_STRING;
	obj.data.str = new StringData(s, len, false); // copy s
	return obj;
}

Object newString(const string& s) {
	return newString(s.c_str(), s.length());
}

Object newString(StringData *data) {
	Object obj;
	obj.type = TYPE_STRING;
	obj.data.str = data;
	return obj;
}

// like above
Object newSymbol(const char *s, size_t len) {
	Object obj;
	obj.type = TYPE_SYMBOL;
	obj.data.str = new StringData(s, len, false); // copy s
	return obj;
}

Object newSymbol(const string& s) {
	return newSymbol(s.c_str(), s.length());
}

Object newSymbol(StringData *data) {
	Object obj;
	obj.type = TYPE_SYMBOL;
	obj.data.str = data;
	return obj;
}

CallFrameData::CallFrameData() {
	outer = NULL;
	bound = false;
}

void CallFrameData::setOuterFrame(CallFrameData *outer) {
	this->outer = outer;
	// NOTE!! this is NOT the place to mark the outer frame
	// as linked -- it is marked when bind-lambda is called
	//
	// setOuterFrame is called on an invocation of a bound lambda
	// when a NEW frame needs to be linked to the original outer frame
	// that existed when the lambda was originally bound. so setOuterFrame
	// is not making a NEW permanent link to outer, only a link that exists as
	// long as this frame exists
}

CallFrameData *CallFrameData::findFrameUp(int levels) {
	auto frame = this;
	while(levels > 0) {
		if(!frame || !frame->outer)
			throw LangError("Bad level number in findFrameUp()");

		levels -= 1;
		frame = frame->outer;
	}
	return frame; // cannot be NULL due to above checks
}

Object CallFrameData::getFrameObj(int levels, int index) {
	//printf("GET OUTER OBJ, level=%d, index=%d\n", levels, index);
	if(index < 0 || index >= MAX_CALLFRAME_SLOTS)
		throw LangError("Out of bounds in CallFrameData::getFrameObj()");
	// go up number of levels
	auto frame = findFrameUp(levels);
	return frame->data[index];
}

void CallFrameData::setFrameObj(int levels, int index, Object obj) {
	//printf("SET OUTER OBJ, level=%d, index=%d\n", levels, index);
	if(index < 0 || index >= MAX_CALLFRAME_SLOTS)
		throw LangError("Out of bounds in CallFrameData::setFrameObj()");
	// go up number of levels
	auto frame = findFrameUp(levels);
	frame->data[index] = obj;
}

void CallFrameData::clear_data() {
	//Object n = newNull();
	//for(int i=0; i<MAX_CALLFRAME_SLOTS; ++i) 
	//	data[i] = n;
	memset((void*)data, 0, MAX_CALLFRAME_SLOTS*sizeof(Object));
}

Object newBoundLambda(Object lambda, CallFrameData *outer) {
	Object obj;
	obj.type = TYPE_BOUND_LAMBDA;
	obj.data.boundLambda = (BoundLambda*)x_malloc(sizeof(BoundLambda));
	obj.data.boundLambda->outer = outer;
	obj.data.boundLambda->objlist = lambda.asLambda();
	return obj;
}

Object newOpcode(uint64_t packed_opcode) {
	Object obj;
	obj.type = TYPE_OPCODE;
	obj.data.opcode = packed_opcode;
	return obj;
}

bool Object::isSymbol(const char *s, int n) const {
	if(type != TYPE_SYMBOL)
		return false;

	// compare full lengths if n==0
	if(n == 0) {
		if(strlen(s) != data.str->length())
			return false;
		else
			return !memcmp(s, data.str->as_cstr(), data.str->length());
	}
	// compare only n chars
	else {
		if(strlen(s) < n || data.str->length() < n)
			return false;
		else
			return !memcmp(s, data.str->as_cstr(), n);
	}
}

bool Object::opEqual(const Object &other) const {
	switch(type) {
		case TYPE_NULL: return other.isNull();
		case TYPE_INT: return (other.type == TYPE_INT && other.data.i == data.i) ||
							(other.type == TYPE_FLOAT && other.data.d == data.i);
		case TYPE_FLOAT: return (other.type == TYPE_FLOAT && other.data.d == data.d) ||
							(other.type == TYPE_INT && other.data.i == data.d);
		case TYPE_BOOL: return other.type == TYPE_BOOL && other.data.b == data.b;
		case TYPE_LAMBDA: return false; // lambdas never equal any other object, even themselves
		case TYPE_BOUND_LAMBDA: return false; // same
		case TYPE_OPCODE: return (other.type == TYPE_OPCODE) &&
				(other.data.opcode == data.opcode);
		case TYPE_STRING: 
			return other.type == TYPE_STRING && 
					data.str->length() == other.data.str->length() &&
					!memcmp(data.str->as_cstr(), other.data.str->as_cstr(), data.str->length());
		case TYPE_SYMBOL: 
			return other.type == TYPE_SYMBOL && 
					data.str->length() == other.data.str->length() &&
					!memcmp(data.str->as_cstr(), other.data.str->as_cstr(), data.str->length());		
		case TYPE_VOID: return other.type == TYPE_VOID;
		// lists are deep compared, via opEqual at each element
		case TYPE_LIST:	{
			if(other.type != TYPE_LIST)
				return false;

			if(data.objlist->size() != other.data.objlist->size())
				return false;

			for(size_t i=0; i<data.objlist->size(); ++i) {
				if(!data.objlist->at(i).opEqual(other.data.objlist->at(i)))
					return false;
			}
			return true;
		}
		// dicts are deep compared
		case TYPE_DICT: {
			if(other.type != TYPE_DICT)
				return false;

			if(data.objdict->size() != other.data.objdict->size())
				return false;

			for(const auto& pair: *data.objdict) {
				auto found = other.data.objdict->find(pair.first);
				if(found == other.data.objdict->end())
					return false; // key not in other

				if(!pair.second.opEqual(found->second))
					return false; // values not equal
			}
			return true;
		}

		default: 
			// i WANT to crash when I forget to add a new type ...
			throw LangError("Unsupported type in == : " + fmtStackPrint());
	}
}

bool Object::opGreater(const Object &other) const {
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
		case TYPE_SYMBOL:
			if(other.type == type) {
				auto r = memcmp(data.str->as_cstr(), other.data.str->as_cstr(), min(data.str->length(), other.data.str->length()));
				if(r == 0) {
					if(data.str->length() < other.data.str->length())
						return false;
					else if(data.str->length() > other.data.str->length())
						return true;
					else
						return false;
				}
				else
					return r > 0;
			}
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

Object Object::opAdd(const Object &other) const {
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
		case TYPE_BOUND_LAMBDA: break;
		// strings & symbols defined as immutable, so no changing this
		case TYPE_STRING:
			if(other.type == TYPE_STRING) {
				// newlen = sum of lengths + null byte
				int newlen = data.str->length() + other.data.str->length() + 1;
				char *buf = (char*)x_malloc(newlen);
				memcpy(buf, data.str->as_cstr(), data.str->length());
				memcpy(buf+data.str->length(), other.data.str->as_cstr(), other.data.str->length());
				// add null
				buf[newlen] = 0;
				return newString(new StringData(buf, newlen-1,true)); // give away pointer
			}	
			break;
		case TYPE_SYMBOL:
			if(other.type == TYPE_SYMBOL) {
				// newlen = sum of lengths + null byte
				int newlen = data.str->length() + other.data.str->length() + 1;
				char *buf = (char*)x_malloc(newlen);
				memcpy(buf, data.str->as_cstr(), data.str->length());
				memcpy(buf+data.str->length(), other.data.str->as_cstr(), other.data.str->length());
				// add null
				buf[newlen] = 0;
				return newSymbol(new StringData(buf, newlen-1,true)); // give away pointer
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

Object Object::opSubtract(const Object &other) const {
	if(type == TYPE_INT && other.type == TYPE_INT) {
		return newInt(data.i - other.data.i);
	}
	else if(isNumber(*this) && isNumber(other)) {
		return newFloat(castFloat(*this) - castFloat(other));
	}
	throw LangError("Bad operands for -: " + this->fmtDisplay() + " & " + other.fmtDisplay());
}

Object Object::opMul(const Object &other) const {
	if(type == TYPE_INT && other.type == TYPE_INT) {
		return newInt(data.i * other.data.i);
	}
	else if(isNumber(*this) && isNumber(other)) {
		return newFloat(castFloat(*this) * castFloat(other));
	}
	throw LangError("Bad operands for *: " + this->fmtDisplay() + " & " + other.fmtDisplay());
}

Object Object::opDivide(const Object &other) const {
	if(isNumber(*this) && isNumber(other)) {
		double denom = castFloat(other);
		if(denom == 0) {
			throw LangError("Divide by zero");
		}
		return newFloat(castFloat(*this) / denom);
	}
	throw LangError("Bad operands for /: " + this->fmtDisplay() + " & " + other.fmtDisplay());
}

int Object::length() const {
	switch(type) {
		case TYPE_STRING:
		case TYPE_SYMBOL:
			return data.str->length();
		case TYPE_LIST:
			return (int)(data.objlist->size());
		case TYPE_DICT:
			return (int)(data.objdict->size());
	}
	throw LangError("'length' not supported for object: " + fmtStackPrint());
}

Object Object::opLength() const {
	return newInt(length());
}

Object Object::opSlice(VINT index, VINT nr) const {
	int objsize;
	switch(type) {
		case TYPE_STRING:
		case TYPE_SYMBOL:
			objsize = data.str->length();
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
		if(type == TYPE_STRING)
			return newString("");
		else if(type == TYPE_SYMBOL)
			return newSymbol("");
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
		case TYPE_STRING: return newString(data.str->as_cstr()+index, nr);
		case TYPE_SYMBOL: return newSymbol(data.str->as_cstr()+index, nr);
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
		// voids usually will not be present in stored data, so print it a little
		// differently so it is easy to spot ... might indicate a programming error etc.
		case TYPE_VOID: return "<*void*>";
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
			return fmtDisplayObjList(data.objlist, "{", "}");
		case TYPE_BOUND_LAMBDA:
			return "<bound " + fmtDisplayObjList(data.boundLambda->objlist, "{", "}") + ">";
		// *IMPORTANT* code is allowed to rely on being able to call 'str' on either a string or
		// symbol and get a plain string
		case TYPE_STRING:
		case TYPE_SYMBOL: 
			{
				string s = "";
				int i;
				for(i=0; i<data.str->length(); ++i) {
					char c = data.str->as_cstr()[i];
					if(c < 32 || c > 126) {
						// turn non-printable chars into '%code'
						char buf[10];
						snprintf(buf, 9, "\\x%02x", (int)((unsigned char)c));
						s += buf;
					}
					else
						s += c;
				}
				return s;
			}
			
		case TYPE_LIST: return fmtDisplayObjList(data.objlist, "[", "]");
		case TYPE_DICT: {
			// in order to ensure consistent output across languages, print with sorted keys.
			// i believe the default c++ map takes care of this.
			string s = "{ ";
			for(const auto& pair: *data.objdict) {
				s += "\"" + pair.first + "\" => ";
				s += pair.second.fmtDisplay() + " ";
			}
			s += "}";
			return s;
		}
		case TYPE_OPCODE:
			return fmtStackPrint();

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

#include "opcodes.hpp"

string Object::fmtStackPrint() const {
	switch(type) {
		case TYPE_NULL: return "<null>";
		case TYPE_VOID: return "<*void*>";
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
			return fmtStackPrintObjList(data.objlist, "{", "}");
		case TYPE_BOUND_LAMBDA:
			return "<bound " + fmtStackPrintObjList(data.boundLambda->objlist, "{", "}") + ">";
		// add " .. " so its clear on stack that it is a string
		case TYPE_STRING: return "\"" + fmtDisplay() + "\"";
		// opposite of above -- since strings get " .. " then i don't get a ' here
		case TYPE_SYMBOL: return "'" + fmtDisplay();
		case TYPE_LIST: return fmtStackPrintObjList(data.objlist, "[", "]");
		case TYPE_DICT: {
			// as above, just with fmtStackPrint
			string s = "{ ";
			for(const auto& pair: *data.objdict) {
				s += "\"" + pair.first + "\" => ";
				s += pair.second.fmtStackPrint() + " ";
			}
			s += "}";
			return s;
		}
		case TYPE_OPCODE:
		{
			uint8_t code, A;
			uint16_t B;
			uint32_t C;
			opcode_unpack(data.opcode, code, A, B, C);
			string s = "#op( ";
			s += opcode_code_to_name(code);
			s += " " + to_string(A);
			s += " " + to_string(B);
			s += " " + to_string(C);
			s += " )";
			return s;
		}

		default: throw LangError("repr not implemented for object type " + to_string(type));
	}
}

ObjList *deepcopy(ObjList *objlist) {
	ObjList *newlist = new ObjList();
	for(auto obj : *objlist)
		newlist->push_back(obj.deepcopy());

	return newlist;
}

Object Object::deepcopy() const {
	switch(type) {
		// all atomic or read-only types just return themselves
		case TYPE_NULL:
		case TYPE_VOID:
		case TYPE_INT:
		case TYPE_FLOAT: 
		case TYPE_BOOL:
		case TYPE_LAMBDA:
		case TYPE_BOUND_LAMBDA:
		case TYPE_STRING:
		case TYPE_SYMBOL:
		case TYPE_OPCODE:
			return *this;

		case TYPE_LIST:
			return newList(::deepcopy(data.objlist));

		case TYPE_DICT: {
			Object obj = newDict();
			for(const auto& pair: *data.objdict) {
				(*obj.data.objdict)[pair.first] = pair.second.deepcopy();
			}
			return obj;
		}
	
		default: throw LangError("deepcopy not implemented for object type " + to_string(type));
	}
}
