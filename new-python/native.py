"""
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from C++ version
"""
from math import floor
from xml.dom.minidom import ReadOnlySequentialNamedNodeMap
from errors import LangError
from interpreter import Interpreter
from langtypes import LangLambda, MemArray, LangString, FLOAT_PRECISION, fmtDisplay, fmtStackPrint, \
				isNumeric

# see notes in C++ implementation of this function.
# this returns (quotient,mod) instead of taking mod as a return param.
def int_divmod(a, b):
	if b == 0:
		raise LangError("Divide by zero")
	
	quot = int(floor(float(abs(a)) / float(abs(b))))
	
	samesign = True if (a < 0 and b < 0) or (a >= 0 and b >= 0) else False

	if samesign:
		mod = a - quot*b
		return quot,mod
	else:
		mod = a + quot*b
		return -quot,mod

def builtin_divmod(I: Interpreter, a, b):
	quot,mod = int_divmod(a,b)
	I.pushInt(mod)
	I.pushInt(quot)

def builtin_define_word(I: Interpreter):
	name = I.syntax.nextWordOrFail()
	words = []
	while True:
		w = I.syntax.nextWordOrFail()
		#print("DEFINE WORD:",w)
		if w == ';':
			I.WORDS[name] = words
			return
		else:
			words.append(w)

# ( obj addr -- ) - save obj to addr
def builtin_set(I: Interpreter, obj, addr):
	# see if its a stack/locals address or allocated memory
	if type(addr) == int:
		if addr < 0 or addr >= I.SIZE_STACKLOCALS:
			raise LangError("Bad address in set!: " + str(addr))
		
		I.STACKLOCALS[addr] = obj
	elif isinstance(addr, MemArray):
		if addr.offset < 0 or addr.offset >= len(addr.mem):
			raise LangError("Offset out of bounds in set!")

		addr.mem[addr.offset] = obj
	else:
		raise LangError("Bad address in set!: " + fmtStackPrint(addr))
	
# ( addr -- obj ) load obj from addr and push to stack
def builtin_ref(I: Interpreter, addr):
	# see if its a stack/locals address or allocated memory
	if type(addr) == int:
		if addr < 0 or addr >= I.SIZE_STACKLOCALS: 
			raise LangError("Bad address in ref: " + str(addr))
		
		I.push(I.STACKLOCALS[addr])
	elif isinstance(addr, MemArray):
		if addr.offset < 0 or addr.offset >= len(addr.mem):
			raise LangError("Offset out of bounds in ref")

		I.push(addr.mem[addr.offset])
	else:
		raise LangError("Bad address in ref: " + fmtStackPrint(addr))

# set stack pointer from addr on stack
def builtin_setsp(I: Interpreter, addr):
	if addr < I.SP_MIN or addr > I.SP_EMPTY:
		raise LangError("Bad address in SP!: " + str(addr))
	
	I.SP = addr

# set locals pointer from addr on stack
def builtin_setlp(I: Interpreter, addr):
	if addr < I.LP_MIN or addr > I.LP_EMPTY:
		raise LangError("Bad address in LP!: " + str(addr))
	
	I.LP = addr

# pop top of stack and push to locals
def builtin_tolocal(I: Interpreter):
	if I.LP <= I.LP_MIN:
		raise LangError("Locals overflow")
	
	I.LP -= 1
	I.STACKLOCALS[I.LP] = I.pop()

# pop top locals and push to stack
def builtin_fromlocal(I: Interpreter):
	if I.LP >= I.LP_EMPTY:
		raise LangError("Locals underflow")
	
	I.push(I.STACKLOCALS[I.LP])
	I.LP += 1

def builtin_showdef(I):
	name = I.syntax.nextWordOrFail()
	if name not in I.WORDS:
		print("No such word: " + name)
		return

	wordlist = I.WORDS[name]
	sys.stdout.write(name + ": ")
	for w in wordlist:
		sys.stdout.write(str(w) + " ")

	print(";")

def popInt(I):
	obj = I.pop()
	if type(obj) == int:
		return int(obj)
	else:
		raise LangError("Expecting int but got: " + fmtStackPrint(obj))

def popIntOrFloat(I):
	obj = I.pop()
	if type(obj) == int or type(obj) == float:
		return float(obj)
	else:
		raise LangError("Expecting float or int but got: " + fmtStackPrint(obj))

def popString(I):
	obj = I.pop()
	if isinstance(obj, LangString):
		return obj.s
	else:
		raise LangError("Expecting string but got: " + fmtStackPrint(obj))

def popSymbol(I):
	obj = I.pop()
	if type(obj) == str:
		return obj
	else:
		raise LangError("Expecting symbol but got: " + fmtStackPrint(obj))

def popStringOrSymbol(I):
	obj = I.pop()
	if type(obj) == str: return obj
	elif isinstance(obj, LangString): return obj.s
	else: raise LangError("Expecting string or symbol but got: " + fmtStackPrint(obj))

def builtin_fadd(I):
	b = popIntOrFloat(I)
	a = popIntOrFloat(I)
	I.push(a+b)

def builtin_fsub(I):
	b = popIntOrFloat(I)
	a = popIntOrFloat(I)
	I.push(a-b)

def builtin_fmul(I):
	b = popIntOrFloat(I)
	a = popIntOrFloat(I)
	I.push(a*b)

def builtin_fdiv(I):
	b = popIntOrFloat(I)
	if b == 0:
		raise LangError("Floating point overflow")

	a = popIntOrFloat(I)
	I.push(a/b)

def builtin_add(I):
	b = I.pop()
	a = I.pop()

	print("ADD: {0} + {1}".format(fmtStackPrint(a),fmtStackPrint(b)))

	if isNumeric(a) and isNumeric(b):
		print("IS NUMERIC")
		I.push(a+b)
	elif type(a) == str and type(b) == str:
		print("ADD STRINGS")
		I.push(a+b)
	elif isinstance(a,LangString) and isinstance(b,LangString):
		print("IS LANG STRING")
		print(a.s+b.s)
	
		I.push(LangString(a.s+b.s))
	elif isinstance(a,MemArray) and type(b) == int:
		# make & modify a clone, else dup'ing then adding would give
		# you two of the same object
		a_ = a.clone()
		a_.offset += b
		I.push(a_)
	# now with swapped args
	elif isinstance(b,MemArray) and type(a) == int:
		# make & modify a clone, else dup'ing then adding would give
		# you two of the same object
		b_ = b.clone()
		b_.offset += a
		I.push(b_)
	else:
		raise LangError("Don't know how to add '{0}' and '{1}'".format(a,b))

def builtin_fsetprec(I, a):
	global FLOAT_PRECISION 
	FLOAT_PRECISION = a

def builtin_gt(I):
	b = popIntOrFloat(I)
	a = popIntOrFloat(I)
	I.push(a>b)

def builtin_puts(I, obj):
	if not isinstance(obj, LangString):
		raise LangError("puts requires string but got: " + fmtStackPrint(obj))

	sys.stdout.write(obj.s)

def builtin_wordlist(I):
	I.push(list(I.WORDS.keys()))
	
# reader interface
READER_WORDLIST = []
READER_POS = 0

def builtin_reader_open_file(I):
	global READER_WORDLIST
	global READER_POS
	filename = popString(I)
	READER_WORDLIST = open(filename, 'r').read().split()
	READER_POS = 0

def builtin_reader_open_string(I):
	global READER_WORDLIST
	global READER_POS
	READER_WORDLIST = popString(I).split()
	print("READER OPEN STRING, WORDS:",READER_WORDLIST)
	READER_POS = 0

def builtin_reader_next(I):
	global READER_WORDLIST
	global READER_POS
	if READER_POS >= len(READER_WORDLIST):
		print("READER NEXT DONE")
		I.push(None)
	else:
		w = READER_WORDLIST[READER_POS]
		READER_POS += 1
		print("READER NEXT RETURNING:",w)
		I.push(w)

def builtin_make_list(I):
	nr = popInt(I)
	objlist = []
	for i in range(nr):
		objlist.insert(0, I.pop())

	I.push(objlist)

def builtin_equals(I):
	b = I.pop()
	a = I.pop()

	if a is None: return b is None
	elif isNumeric(a): return isNumeric(b) and a==b
	elif type(a) == bool: return type(b) == bool and a==b
	elif type(a) == str: return type(b) == str and a==b
	elif isinstance(a,LangString): return isinstance(b,LangString) and a.s == b.s
	else:
		raise LangError("Unable to compare objects (==) {0} and {1}".format(fmtStackPrint(a),fmtStackPrint(b)))

def builtin_greater(I):
	b = I.pop()
	a = I.pop()

	if isNumeric(a) and isNumeric(b): return a>b
	elif type(a) == str and type(b) == str: return a>b
	elif isinstance(a,LangString) and isinstance(b,LangString): return a.s > b.s
	else:
		raise LangError("Unable to compare objects (>) {0} and {1}".format(fmtStackPrint(a),fmtStackPrint(b)))

def builtin_error(I):
	msg = popString(I)
	raise LangError(msg)

def builtin_slice(I, obj, index, nr):
	# ported from the C++ version
	if type(obj) == str: objsize = len(obj)
	elif isinstance(obj, LangString): objsize = len(obj.s)
	elif type(obj) == list: objsize = len(obj)
	elif isinstance(obj,MemArray): objsize = len(obj.mem)
	else: raise LangError("Object doesn't support slicing: " + fmtStackPrint(obj))
	
	if index < 0: index = objsize + index
	if index < 0 or index >= objsize:
		if type(obj) == str: return ""
		elif isinstance(obj, LangString): return LangString("")
		elif type(obj) == list: return []
		elif isinstance(obj,MemArray): return MemArray(0, 0)

	if nr < 0: nr = objsize - index
	if (index+nr) > objsize: nr = objsize - index

	if type(obj) == str: return obj[index:index+nr]
	elif isinstance(obj, LangString): return LangString(obj.s[index:index+nr])
	elif type(obj) == list: return obj[index:index+nr]
	elif isinstance(obj,MemArray):
		mem = MemArray(nr, 0)
		mem.mem = obj.mem[index:index+nr]
		return mem
		
	raise LangError("Unreachable code!!")

def builtin_unmake(I, obj):
	print("UNMAKE:", fmtStackPrint(obj))
	fn = lambda x: x
	if type(obj) == str: 
		seq = obj
		fn = lambda x: ord(x)
	elif isinstance(obj, LangString): 
		seq = obj.s
		fn = lambda x: ord(x)
	elif type(obj) == list: seq = obj
	elif isinstance(obj, LangLambda): seq = obj.objlist
	elif isinstance(obj, MemArray): seq = obj.mem

	for o in seq:
		I.push(fn(o))

	I.push(len(seq))

def builtin_make_string(I, nr):
	s = ''
	for i in range(nr):
		s = chr(popInt(I)) + s

	I.push(LangString(s))

def builtin_length(I, obj):
	if type(obj) == str: I.pushInt(len(obj))
	elif isinstance(obj, LangString): I.pushInt(len(obj.s))
	elif type(obj) == list: I.pushInt(len(obj))
	elif isinstance(obj,MemArray): I.pushInt(len(obj.mem))
	else: raise LangError("Object doesn't support 'length': " + fmtStackPrint(obj))

def builtin_make_word(I):
	name = popSymbol(I)
	objlist = I.pop()
	if type(objlist) != list:
		raise LangError("make-word expects list but got: " + fmtStackPrint(objlist))

	I.WORDS[name] = objlist

def builtin_append(I):
	obj = I.pop()
	_list = I.pop()
	if type(_list) != list:
		raise LangError("append expecting list but got: " + fmtStackPrint(_list))

	_list.append(obj)
	I.push(_list)

import sys
# the interpreter pops & checks the argument types, making the code shorter here
BUILTINS = {
	'+': ([], builtin_add),
	'-': ([int,int], lambda I,a,b: I.pushInt(a-b)),
	'*': ([int,int], lambda I,a,b: I.pushInt(a*b)),
	'/mod': ([int,int], builtin_divmod),
	'f+': ([], builtin_fadd),
	'f-': ([], builtin_fsub),
	'f*': ([], builtin_fmul),
	'f/': ([], builtin_fdiv),
	'f.setprec': ([int], builtin_fsetprec),
	'==': ([], lambda I: I.push(builtin_equals(I))),
	'>': ([], lambda I: I.push(builtin_greater(I))),
	'.c': ([int], lambda I,a: sys.stdout.write(chr(a))),
	# object means any type
	# format TOS for stack display and push string
	'repr': ([object], lambda I,o: I.push(LangString(fmtStackPrint(o)))),
	# format TOS as display string and push
	'str': ([object], lambda I,o: I.push(LangString(fmtDisplay(o)))),
	# print string from TOS
	'puts': ([object], builtin_puts),
	'int?': ([object], lambda I,o: I.push(type(o) == int)),
	'list?': ([object], lambda I,o: I.push(type(o) == list)),
	'string?': ([object], lambda I,o: I.push(isinstance(o,LangString))),
	'symbol?': ([object], lambda I,o: I.push(type(o) == str)),
	'null?': ([], lambda I: I.push(I.pop() is None)),
	'array?': ([object], lambda I,o: I.push(isinstance(o,MemArray))),
	# [] for no args
	'depth': ([], lambda I: I.push(I.SP_EMPTY - I.SP)),
	':': ([], builtin_define_word),
	'def': ([], builtin_define_word),
	'ref': ([object], builtin_ref),
	'set!': ([object,object], builtin_set),
	'SP': ([], lambda I: I.push(I.SP)),
	'SP!': ([int], builtin_setsp),
	'LP': ([], lambda I: I.push(I.LP)),
	'LP!': ([int], builtin_setlp),
	'>L': ([], builtin_tolocal),
	'L>': ([], builtin_fromlocal),
	'.showdef': ([], builtin_showdef),
	'.wordlist': ([], builtin_wordlist),
	'error': ([], builtin_error),

	# reader interface
	'reader-open-file': ([], builtin_reader_open_file),
	'reader-open-string': ([], builtin_reader_open_string),
	'reader-next': ([], builtin_reader_next),
	# compiler words
	'make-list': ([], builtin_make_list),
	'slice': ([object,int,int], lambda I,obj,index,nr: I.push(builtin_slice(I,obj,index,nr))),
	'unmake': ([object], lambda I,obj: builtin_unmake(I,obj)),
	'make-string': ([int], lambda I,nr: builtin_make_string(I,nr)),
	'length': ([object], lambda I,obj: builtin_length(I,obj)),
	'parse-int': ([], lambda I: I.pushInt(int(popStringOrSymbol(I)))),
	'make-word': ([], builtin_make_word),
	'append': ([], builtin_append),
}

