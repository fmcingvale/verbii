"""
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from C++ version
"""
from math import floor
from xml.dom.minidom import ReadOnlySequentialNamedNodeMap
from errors import LangError
from interpreter import Interpreter
from langtypes import LangLambda, LangString, fmtDisplay, fmtStackPrint, \
				isNumeric, LangClosure, deepcopy, isString, isSymbol, \
					isList, isClosure, isLambda, isDict, isInt, isFloat, isBool, \
						isNull, parseBool, LangVoid, isVoid, LangNull
import time, sys

# has to be set externally
NATIVE_CMDLINE_ARGS = []
ALLOW_OVERWRITE_WORDS = False
EXIT_ON_EXCEPTION = True

STARTUP_TIME = time.time()

# file for writing output (puts, .c)
FILE_STDOUT = sys.stdout

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

# ( obj addr -- ) - save obj to addr
def builtin_set(I: Interpreter, obj, addr: int):
	if addr < 0 or addr >= len(I.OBJMEM):
		raise LangError("Bad address in set!: " + str(addr))
	
	I.OBJMEM[addr] = obj
	
# ( addr -- obj ) load obj from addr and push to stack
def builtin_ref(I: Interpreter, addr: int):
	if addr < 0 or addr >= len(I.OBJMEM): 
		raise LangError("Bad address in ref: " + str(addr))
	
	I.push(I.OBJMEM[addr])

# set stack pointer from addr on stack
def builtin_setsp(I: Interpreter, addr):
	if addr < I.SP_MIN or addr > I.SP_EMPTY:
		raise LangError("Bad address in SP!: " + str(addr))
	
	I.SP = addr
	# stats
	I.min_run_SP = min(I.min_run_SP,I.SP)

# set locals pointer from addr on stack
def builtin_setlp(I: Interpreter, addr):
	if addr < I.LP_MIN or addr > I.LP_EMPTY:
		raise LangError("Bad address in LP!: " + str(addr))
	
	I.LP = addr
	# stats
	I.min_run_LP = min(I.min_run_LP,I.LP)

# pop top of stack and push to locals
def builtin_tolocal(I: Interpreter):
	if I.LP <= I.LP_MIN:
		raise LangError("Locals overflow")
	
	I.LP -= 1
	I.OBJMEM[I.LP] = I.pop()

# pop top locals and push to stack
def builtin_fromlocal(I: Interpreter):
	if I.LP >= I.LP_EMPTY:
		raise LangError("Locals underflow")
	
	I.push(I.OBJMEM[I.LP])
	I.LP += 1

def popInt(I):
	obj = I.pop()
	if isInt(obj):
		return int(obj)
	else:
		raise LangError("Expecting int but got: " + fmtStackPrint(obj))

def popIntOrFloat(I):
	obj = I.pop()
	if isNumeric(obj):
		return float(obj)
	else:
		raise LangError("Expecting float or int but got: " + fmtStackPrint(obj))

def popString(I,where):
	obj = I.pop()
	if isString(obj):
		return obj.s
	else:
		raise LangError("Expecting string (in {0}) but got: {1} ".format(where, fmtStackPrint(obj)))

def popSymbol(I):
	obj = I.pop()
	if isSymbol(obj):
		return obj
	else:
		raise LangError("Expecting symbol but got: " + fmtStackPrint(obj))

def popStringOrSymbol(I,where):
	obj = I.pop()
	if isSymbol(obj): return obj
	elif isString(obj): return obj.s
	else: raise LangError("Expecting string or symbol (in {0}) but got: {1}".format(where,fmtStackPrint(obj)))

def popList(I):
	obj = I.pop()
	if isList(obj):
		return obj
	else:
		raise LangError("Expecting list but got: " + fmtStackPrint(obj))

def builtin_add(I):
	b = I.pop()
	a = I.pop()

	#print("ADD: {0} + {1}".format(fmtStackPrint(a),fmtStackPrint(b)))

	if isInt(a) and isInt(b):
		# keep as integer and check limits
		I.pushInt(a+b)
	elif isNumeric(a) and isNumeric(b): # result is float
		#print("IS NUMERIC")
		I.push(a+b)
	elif isSymbol(a) and isSymbol(b):
		#print("ADD SYMBOLS")
		I.push(a+b)
	elif isString(a) and isString(b):
		#print("IS LANG STRING")
		#print(a.s+b.s)
		I.push(LangString(a.s+b.s))
	elif isList(a) and isList(b):
		I.push(a+b)
	else:
		raise LangError("Don't know how to add '{0}' and '{1}'".format(a,b))

def builtin_subtract(I):
	b = I.pop()
	a = I.pop()

	# like addition, keep int+int as special case
	if isInt(a) and isInt(b):
		I.pushInt(a-b)
	elif isNumeric(a) and isNumeric(b):
		#print("IS NUMERIC")
		I.push(a-b)
	else:
		raise LangError("Unable to subtract '{0}' and '{1}'".format(a,b))

def builtin_mul(I):
	b = I.pop()
	a = I.pop()

	# like above, int+int is special case
	if isInt(a) and isInt(b):
		I.pushInt(a*b)
	elif isNumeric(a) and isNumeric(b):
		I.push(a*b)
	else:
		raise LangError("Unable to multiply '{0}' and '{1}'".format(a,b))

def builtin_div(I):
	b = I.pop()
	a = I.pop()

	# *UNLIKE* above, result here is ALWAYS float
	if isNumeric(a) and isNumeric(b):
		if b == 0:
			raise LangError("Divide by zero")

		I.push(float(a)/float(b))
	else:
		raise LangError("Unable to divide '{0}' and '{1}'".format(a,b))

def builtin_fsetprec(I, a):
	import langtypes
	langtypes.FLOAT_PRECISION = a

def builtin_gt(I):
	b = popIntOrFloat(I)
	a = popIntOrFloat(I)
	I.push(a>b)

def builtin_puts(I, obj):
	if not isString(obj):
		raise LangError("puts requires string but got: " + fmtStackPrint(obj))

	FILE_STDOUT.write(obj.s)

def builtin_wordlist(I):
	I.push(list(I.WORDS.keys()))

def builtin_readfile(I):
	filename = popString(I, "read-file")
	I.push(LangString(open(filename, 'r').read()))

def builtin_make_list(I):
	nr = popInt(I)
	objlist = []
	for i in range(nr):
		objlist.insert(0, I.pop())

	I.push(objlist)

def test_equal(a,b):
	if isNull(a): return isNull(b)
	elif isNumeric(a): return isNumeric(b) and a==b
	elif isBool(a): return isBool(b) and a==b
	elif isSymbol(a): return isSymbol(b) and a==b
	elif isString(a): return isString(b) and a.s == b.s
	elif isLambda(a): return False # lambdas are never equal, even if its the same object
	elif isList(a):
		if not isList(b): return False
		if len(a) != len(b): return False
		for i in range(len(a)):
			if not test_equal(a[i],b[i]): return False
		return True
	elif isDict(a):
		if not isDict(b): return False
		if len(a) != len(b): return False
		for k in a.keys():
			if k not in b: return False
			if not test_equal(a[k],b[k]): return False
		return True
	else:
		raise LangError("Unable to compare objects (==) {0} and {1}".format(fmtStackPrint(a),fmtStackPrint(b)))

def builtin_equal(I):
	b = I.pop()
	a = I.pop()
	I.push(test_equal(a,b))

def builtin_greater(I):
	b = I.pop()
	a = I.pop()
	I.push(test_greater(a,b))

def test_greater(a,b):
	if isNumeric(a) and isNumeric(b): return a>b
	elif isSymbol(a) and isSymbol(b): return a>b
	elif isString(a) and isString(b): return a.s > b.s
	elif isList(a) and isList(b):
		# see c++ comments - do it like a string test, still raise error on nonequal types	
		# check up to max of common length
		nr = min(len(a),len(b))
		for i in range(nr):
			if test_greater(a[i],b[i]):
				# first a>b element --> entire result is greater
				return True
			elif not test_equal(a[i],b[i]):
				# (a != b) and (a not > b) so: b < a --> entire result is less
				return False
			# else a==b, continue to next element

		# first nr elements are equal, so length determines result
		return len(a) > len(b)
	else:
		raise LangError("Unable to compare objects (>) {0} and {1}".format(fmtStackPrint(a),fmtStackPrint(b)))

def builtin_error(I):
	msg = popString(I,'error')
	raise LangError(msg)

def builtin_slice(I, obj, index, nr):
	# ported from the C++ version
	if isSymbol(obj) or isList(obj): objsize = len(obj)
	elif isString(obj): objsize = len(obj.s)
	else: raise LangError("Object doesn't support slicing: " + fmtStackPrint(obj))
	
	if index < 0: index = objsize + index
	if index < 0 or index >= objsize:
		if isSymbol(obj): return ""
		elif isString(obj): return LangString("")
		elif isList(obj): return []

	if nr < 0: nr = objsize - index
	if (index+nr) > objsize: nr = objsize - index

	if isSymbol(obj): return obj[index:index+nr]
	elif isString(obj): return LangString(obj.s[index:index+nr])
	elif isList(obj): return obj[index:index+nr]
		
	raise LangError("Unreachable code!!")

def builtin_unmake(I, obj):
	#print("UNMAKE:", fmtStackPrint(obj))
	fn = lambda x: x
	if isSymbol(obj): 
		seq = obj
		fn = lambda x: ord(x)
	elif isString(obj): 
		seq = obj.s
		fn = lambda x: ord(x)
	elif isList(obj): seq = obj
	elif isLambda(obj): 
		# must deepcopy, see DESIGN-NOTES.md
		I.push(deepcopy(obj.objlist))
		return
	elif isClosure(obj):
		# as above, must deepcopy list -- state is meant to be modified, so it stays as-is
		I.push(deepcopy(obj.objlist))
		I.push(obj.state)
		return
	else:
		raise LangError("Don't know how to unmake object: " + fmtStackPrint(obj))

	for o in seq:
		I.push(fn(o))

	I.push(len(seq))

def builtin_make_string(I, nr):
	s = ''
	for i in range(nr):
		s = chr(popInt(I)) + s

	I.push(LangString(s))

def builtin_make_symbol(I, nr):
	s = ''
	for i in range(nr):
		s = chr(popInt(I)) + s

	I.push(s)

def builtin_make_lambda(I):
	_list = popList(I)
	# must deepcopy, see DESIGN-NOTES.txt
	I.push(LangLambda(deepcopy(_list)))

def builtin_length(I, obj):
	if isSymbol(obj) or isList(obj) or isDict(obj): I.pushInt(len(obj))
	elif isString(obj): I.pushInt(len(obj.s))
	else: raise LangError("Object doesn't support 'length': " + fmtStackPrint(obj))

def builtin_make_word(I):
	name = popSymbol(I)
	objlist = I.pop()
	if not isList(objlist):
		raise LangError("make-word expects list but got: " + fmtStackPrint(objlist))
	
	I.defineWord(name, objlist, ALLOW_OVERWRITE_WORDS)
	
def builtin_append(I):
	#print("APPEND:")
	#print(I.reprStack())

	obj = I.pop()
	_list = I.pop()
	if not isList(_list):
		raise LangError("append expecting list but got: " + fmtStackPrint(_list))

	_list.append(obj)
	I.push(_list)

def builtin_make_closure(I):
	state = I.pop()
	obj = I.pop()
	if isList(obj):
		# as with lambda, deepcopy objlist
		I.push(LangClosure(deepcopy(obj),state))
	elif isLambda(obj):
		I.push(LangClosure(deepcopy(obj.objlist),state))
	else:
		raise LangError("make-closure expects list or lambda but got:" + fmtStackPrint(obj))

def builtin_self_get(I):
	if I.closure is None: raise LangError("Attempting to reference unbound self")
	I.push(I.closure.state)

def builtin_self_set(I):
	if I.closure is None: raise LangError("Attempting to set unbound self")
	I.closure.state = I.pop()
	
def builtin_put(I):
	obj = I.pop()
	index = I.pop()
	dest = I.pop()

	if isList(dest):
		if not isInt(index): raise LangError("put requires index, got: " + fmtStackPrint(index))
		if index < 0: index += len(dest) # negative indexes
		if index < 0 or index >= len(dest): raise LangError("Index out of range in put")
		dest[index] = obj
		I.push(dest)
	elif isDict(dest):
		if not isString(index): raise LangError("put requires string key, got: " + fmtStackPrint(index))
		dest[index] = obj
		I.push(dest)
	else:
		raise LangError("Object does not support put: " + fmtStackPrint(dest))

def builtin_get(I):
	index = I.pop()
	obj = I.pop()

	if isSymbol(obj) or isList(obj):
		if not isInt(index): raise LangError("get requires index, got: " + fmtStackPrint(index))
		if index < 0: index += len(obj) # negative indexes
		if index < 0 or index >= len(obj): raise LangError("Index out of range in get")
		I.push(obj[index])
	elif isString(obj):
		if not isInt(index): raise LangError("get requires index, got: " + fmtStackPrint(index))
		if index < 0: index += len(obj.s) # negative indexes
		if index < 0 or index >= len(obj.s): raise LangError("Index out of range in get")
		I.push(LangString(obj.s[index]))
	elif isDict(obj):
		if not isString(index): raise LangError("get requires string key, got: " + fmtStackPrint(index))
		if index not in obj:
			I.push(LangVoid()) # missing key returns void
		else:
			I.push(obj[index])
	else:
		raise LangError("Object does not support get: " + fmtStackPrint(obj))
	
def builtin_bit_shr(I):
	nr = popInt(I)
	a = popInt(I)
	I.push((a>>nr) & 0xffffffff)

def builtin_bit_shl(I):
	nr = popInt(I)
	a = popInt(I)
	I.push((a<<nr) & 0xffffffff)

def builtin_set_exit_on_exception(I, flag):
	global EXIT_ON_EXCEPTION
	EXIT_ON_EXCEPTION = flag

def builtin_set_allow_overwrite_words(I, flag):
	global ALLOW_OVERWRITE_WORDS
	ALLOW_OVERWRITE_WORDS = flag

def builtin_deserialize(I):
	from deserialize import deserialize_stream
	fileIn = open(popString(I,"deserialize"), "r")
	deserialize_stream(I, fileIn)
	# no return, just loads words into interpreter

def builtin_prompt(I):
	prompt = popString(I,"prompt")
	# NOTE - ignore any user-set stdout since user needs to see prompt on screen
	sys.stdout.write(prompt)
	sys.stdout.flush()
	line = sys.stdin.readline()
	if len(line) == 0:
		I.push(LangVoid()) # eof
	else:
		# chop off any \r or \n
		while len(line) and line[-1] in "\r\n":
			line = line[:-1]

		I.push(LangString(line))

# ( filename -- ; open filename and write stdout there )
# ( void -- ; close any file attached to stdout and reset to normal stdout )
#
# this only redirects builtins 'puts' and '.c'. this does NOT redirect error messages
# and (builtin) prompts, they still go to the screen.
def builtin_open_as_stdout(I):
	global FILE_STDOUT
	obj = I.pop()
	if isVoid(obj):
		if FILE_STDOUT != sys.stdout:
			FILE_STDOUT.close()
			FILE_STDOUT = sys.stdout
	elif isString(obj):
		FILE_STDOUT = open(obj.s, "w")
	else:
		raise LangError("Unknown arg to open-as-stdout:" + fmtStackPrint(obj))

import os
# the interpreter pops & checks the argument types, making the code shorter here
BUILTINS = {
	'+': ([], builtin_add),
	'-': ([], builtin_subtract),
	'*': ([], builtin_mul),
	'/': ([], builtin_div),
	'/mod': ([int,int], builtin_divmod),
	'f.setprec': ([int], builtin_fsetprec),
	'==': ([], builtin_equal),
	'>': ([], builtin_greater),
	'.c': ([int], lambda I,a: FILE_STDOUT.write(chr(a))),
	# object means any type
	# format TOS for stack display and push string
	'repr': ([object], lambda I,o: I.push(LangString(fmtStackPrint(o)))),
	# format TOS as display string and push
	'str': ([object], lambda I,o: I.push(LangString(fmtDisplay(o)))),
	# print string from TOS
	'puts': ([object], builtin_puts),
	'int?': ([object], lambda I,o: I.push(isInt(o))),
	'float?': ([object], lambda I,o: I.push(isFloat(o))),
	'bool?': ([object], lambda I,o: I.push(isBool(o))),
	'list?': ([object], lambda I,o: I.push(isList(o))),
	'string?': ([object], lambda I,o: I.push(isString(o))),
	'symbol?': ([object], lambda I,o: I.push(isSymbol(o))),
	'null?': ([object], lambda I,o: I.push(isNull(o))),
	'void?': ([object], lambda I,o: I.push(isVoid(o))),
	'lambda?': ([object], lambda I,o: I.push(isLambda(o))),
	'closure?': ([object], lambda I,o: I.push(isClosure(o))),
	# [] for no args
	'depth': ([], lambda I: I.push(I.SP_EMPTY - I.SP)),
	'ref': ([int], builtin_ref),
	'set!': ([object,int], builtin_set),
	'SP': ([], lambda I: I.push(I.SP)),
	'SP!': ([int], builtin_setsp),
	'LP': ([], lambda I: I.push(I.LP)),
	'LP!': ([int], builtin_setlp),
	'>L': ([], builtin_tolocal),
	'L>': ([], builtin_fromlocal),
	'.wordlist': ([], builtin_wordlist),
	'error': ([], builtin_error),

	# compiler words
	'make-list': ([], builtin_make_list),
	'slice': ([object,int,int], lambda I,obj,index,nr: I.push(builtin_slice(I,obj,index,nr))),
	'unmake': ([object], lambda I,obj: builtin_unmake(I,obj)),
	'make-string': ([int], lambda I,nr: builtin_make_string(I,nr)),
	'make-symbol': ([int], lambda I,nr: builtin_make_symbol(I,nr)),
	'length': ([object], lambda I,obj: builtin_length(I,obj)),
	'parse-int': ([], lambda I: I.pushInt(int(popStringOrSymbol(I,'parse-int')))),
	'parse-float': ([], lambda I: I.push(float(popStringOrSymbol(I,'parse-int')))),
	'make-word': ([], builtin_make_word),
	'make-lambda': ([], builtin_make_lambda),
	'append': ([], builtin_append),
	'void': ([], lambda I: I.push(LangVoid())),
	'cmdline-args': ([], lambda I: I.push(NATIVE_CMDLINE_ARGS)),
	'.dumpword': ([], lambda I: I.push(deepcopy(I.lookupWordOrFail(popSymbol(I))))),
	'read-file': ([], builtin_readfile),
	'make-closure': ([], builtin_make_closure),
	'self': ([], builtin_self_get),
	'self!': ([], builtin_self_set),
	'put': ([], builtin_put),
	'get': ([], builtin_get),
	'deepcopy': ([object], lambda I,o: I.push(deepcopy(o))),
	'alloc': ([int], lambda I,nr: I.push(I.heap_alloc(nr))),
	',,del': ([], lambda I: I.deleteWord(popSymbol(I))),

	# note below -- the "& 0xffffffff" ensures I have an unsigned 32-bit value
	'bit-not': ([], lambda I: I.push((~popInt(I)) & 0xffffffff)), 
	'bit-and': ([], lambda I: I.push((popInt(I) & popInt(I)) & 0xffffffff)), 
	'bit-or': ([], lambda I: I.push((popInt(I) | popInt(I)) & 0xffffffff)), 
	'bit-xor': ([], lambda I: I.push((popInt(I) ^ popInt(I)) & 0xffffffff)), 
	'bit-shr': ([], builtin_bit_shr),
	'bit-shl': ([], builtin_bit_shl),
	
	'run-time': ([], lambda I: I.push(time.time()-STARTUP_TIME)),
	',,new-dict': ([], lambda I: I.push({})),

	# new words needed for running boot.verb
	'file-exists?': ([], lambda I: I.push(os.path.isfile(popString(I,'file-exists?')))),
	'file-mtime': ([], lambda I: I.push(os.path.getmtime(popString(I,'file-mtime')))),
	'open-as-stdout': ([], builtin_open_as_stdout),
	'deserialize': ([], builtin_deserialize),
	'prompt': ([], builtin_prompt),
	'set-exit-on-exception': ([bool], builtin_set_exit_on_exception),
	'set-allow-overwrite-words': ([bool], builtin_set_allow_overwrite_words),

	'time-string': ([], lambda I: I.push(LangString(time.strftime("%Y-%m-%d %H:%M:%S")))),
	'floor': ([], lambda I: I.push(int(floor(popIntOrFloat(I))))),
}

