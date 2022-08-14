/*
	LangTypes - type system.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Type mapping:

	Verbii			Javascript
	--------		--------------
	null			null
	void			Void (class)
	int				number
	float			Float (class)
	string			String (class)
	symbol			string
	lambda			Lambda (class)
	bound-lambda	Lambda (class)
	boolean			boolean
	list			Array (builtin object)
	dict			Map (builtin object)
	opcode			TBD
*/

class Float {
	constructor(value) {
		this.value = value;
	}
}

class String {
	constructor(value) {
		this.value = value;
	}
}

class Null {
	constructor() {
	}
}

class Void {
	constructor() {
	}
}

class LangError {
	constructor(msg) {
		this.msg = msg;
	}
}

// *** SYNC THIS WITH C++ IMPLEMENTATION ***
const MAX_CALLFRAME_SLOTS = 255; 

// CallFrameData is not a user-visible object (from verbii code)

// NOTE this is a naive implementation -- later needs to be able to reuse
// frames that didn't get linked to anything instead of creating a new frame
// at each function call
class CallFrameData {
	constructor() { 
		this.outer = null;
		this.data = Array();
		this.data.fill(newNull(), 0, MAX_CALLFRAME_SLOTS-1);
	}
	
	setOuterFrame(outer) { this.outer = outer; }
	getOuterFrame() { return this.outer; }

	// get/set object in frame up #levels (0 == this frame)
	getFrameObj(levels, index) {
		if(index < 0 || index >= MAX_CALLFRAME_SLOTS)
			throw new LangError("Out of bounds in CallFrameData::setLocalObj()");
		
		let frame = findFrameUp(levels);
		return frame.data[index];
	}

	setFrameObj(levels, index, obj) {
		if(index < 0 || index >= MAX_CALLFRAME_SLOTS)
			throw new LangError("Out of bounds in CallFrameData::setLocalObj()");

		let frame = findFrameUp(levels);
		frame.data[index] = obj;
	}

	findFrameUp(levels) {
		let frame = this;
		while(levels > 0) {
			if(frame == null || frame.outer == null)
				throw new LangError("Bad level number in findFrameUp()");

			levels -= 1;
			frame = frame.outer;
		}
		return frame; // cannot be NULL due to above checks
	}
}

// unlike other ports, using a unified type here for both bound and unbound lambdas
class Lambda {
	constructor(objlist, outer=null) {
		this.objlist = objlist;
		this.outer = outer;
	}
}

function isNull(obj) { return obj == null; }
function isVoid(obj) { return obj instanceof Void; }
function isInt(obj) { return typeof(obj) == "number"; }
function isFloat(obj) { return obj instanceof Float; }
function isBool(obj) { return typeof(obj) == "boolean"; }
function isSymbol(obj) { return typeof(obj) == "string"; }
function isString(obj) { return obj instanceof String; }
function isList(obj) { return obj instanceof Array; }
function isLambda(obj) { return obj instanceof Lambda; }
function isDict(obj) { return obj instanceof Map; }

// make a unified set of 'new' funcs for primitives & objects 
// to help with readability (should validate inputs later)
function newNull() { return null; }
function newBool(v) { return v; }
function newInt(i) { return i; }
function newSymbol(s) { return s; }
function newList() { return Array(); }
function newString(s) { return new String(s); }
function newFloat(f) { return new Float(f); }
function newLambda(objlist,outer=null) { return new Lambda(objlist,outer); }
function newVoid() { return new Void(); }
function newDict() { return new Map(); }

function fmtStackPrintObjlist(list, open_delim, close_delim) {
	let s = open_delim;
	for(let i=0; i<list.length; ++i)
		s += " " + fmtStackPrint(list[i]);
	
	return s + " " + close_delim;
}

function fmtStackPrint(obj) {
	if(isInt(obj))
		return obj.toString();
	else if(isFloat(obj))
		return "#" + obj.value.toString();
	else if(isBool(obj)) {
		return obj ? "<true>" : "<false>";
	}
	else if(isSymbol(obj))
		return "'" + obj;
	else if(isString(obj))
		return '"' + obj.value + '"';
	else if(isNull(obj))
		return "<null>";
	else if(isVoid(obj))
		return "<*void*>"; 
	else if(isList(obj))
		return fmtStackPrintObjlist(obj, "[", "]");
	else if(isLambda(obj)) {
		if(obj.outer == null) 
			return fmtStackPrintObjlist(obj.objlist, "{", "}");
		else
			return "<bound " + fmtStackPrintObjlist(obj.objlist, "{", "}") + ">";
	}
	else if(isDict(obj)) {
		let keys = Array();
		for(const k of obj.keys())
			keys.push(k);
		
		keys.sort(function(a,b) {
			if(a.value < b.value) return -1;
			else if(a.value == b.value) return 0;
			else return 1;
		});
		let s = "{ ";
		for(let i=0; i<keys.length; ++i)
			s += fmtStackPrint(keys[i]) + " => " + fmtStackPrint(obj.get(keys[i])) + " ";

		s += "}";
		return s;
	}
	else
		throw new LangError("Don't know how to stack-format object: " + obj.toString());
}

//for(let i=0; i<objs.length; ++i) {
//	process.stdout.write(fmtStackPrint(objs[i]) + "\n");
//	//console.log(typeof(objs[i]));	
//}
let objs = newList();

objs.push(newInt(123))
objs.push(newSymbol("Hello"))
objs.push(newBool(true))
objs.push(newBool(false))
objs.push(newFloat(1.25));
objs.push(newString("Hello string"));
objs.push(newNull());
objs.push(newVoid());
let l = newList();
l.push(newInt(123));
l.push(newString("abc"));
l.push(newSymbol("return"));
objs.push(newLambda(l,null));
let d = newDict();
d.set(newString("def"), newInt(123));
d.set(newString("abc"), newInt(123));
d.set(newString("ghi"), newInt(123));
objs.push(d);

process.stdout.write("The objects ...\n");

process.stdout.write(fmtStackPrint(objs) + "\n");

console.log(objs instanceof Array);
