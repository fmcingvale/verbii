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
				isNumeric, LangClosure, deepcopy
import time

# has to be set externally
NATIVE_CMDLINE_ARGS = []
ALLOW_OVERWRITE_WORDS = False

STARTUP_TIME = time.time()

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

def popString(I,where):
	obj = I.pop()
	if isinstance(obj, LangString):
		return obj.s
	else:
		raise LangError("Expecting string (in {0}) but got: {1} ".format(where, fmtStackPrint(obj)))

def popSymbol(I):
	obj = I.pop()
	if type(obj) == str:
		return obj
	else:
		raise LangError("Expecting symbol but got: " + fmtStackPrint(obj))

def popStringOrSymbol(I,where):
	obj = I.pop()
	if type(obj) == str: return obj
	elif isinstance(obj, LangString): return obj.s
	else: raise LangError("Expecting string or symbol (in {0}) but got: {1}".format(where,fmtStackPrint(obj)))

def popList(I):
	obj = I.pop()
	if type(obj) == list:
		return obj
	else:
		raise LangError("Expecting list but got: " + fmtStackPrint(obj))

def builtin_add(I):
	b = I.pop()
	a = I.pop()

	#print("ADD: {0} + {1}".format(fmtStackPrint(a),fmtStackPrint(b)))

	if type(a) == int and type(b) == int:
		# keep as integer and check limits
		I.pushInt(a+b)
	elif isNumeric(a) and isNumeric(b): # result is float
		#print("IS NUMERIC")
		I.push(a+b)
	elif type(a) == str and type(b) == str:
		#print("ADD SYMBOLS")
		I.push(a+b)
	elif isinstance(a,LangString) and isinstance(b,LangString):
		#print("IS LANG STRING")
		#print(a.s+b.s)
	
		I.push(LangString(a.s+b.s))
	elif type(a) == list and type(b) == list:
		I.push(a+b)
	else:
		raise LangError("Don't know how to add '{0}' and '{1}'".format(a,b))

def builtin_subtract(I):
	b = I.pop()
	a = I.pop()

	# like addition, keep int+int as special case
	if type(a) == int and type(b) == int:
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
	if type(a) == int and type(b) == int:
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
	if not isinstance(obj, LangString):
		raise LangError("puts requires string but got: " + fmtStackPrint(obj))

	sys.stdout.write(obj.s)

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
	if a is None: return b is None
	elif isNumeric(a): return isNumeric(b) and a==b
	elif type(a) == bool: return type(b) == bool and a==b
	elif type(a) == str: return type(b) == str and a==b
	elif isinstance(a,LangString): return isinstance(b,LangString) and a.s == b.s
	elif isinstance(a,LangLambda): return False # lambdas are never equal, even if its the same object
	elif type(a) == list:
		if type(b) != list: return False
		if len(a) != len(b): return False
		for i in range(len(a)):
			if not test_equal(a[i],b[i]): return False
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
	elif type(a) == str and type(b) == str: return a>b
	elif isinstance(a,LangString) and isinstance(b,LangString): return a.s > b.s
	elif type(a) == list and type(b) == list:
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
	if type(obj) == str: objsize = len(obj)
	elif isinstance(obj, LangString): objsize = len(obj.s)
	elif type(obj) == list: objsize = len(obj)
	else: raise LangError("Object doesn't support slicing: " + fmtStackPrint(obj))
	
	if index < 0: index = objsize + index
	if index < 0 or index >= objsize:
		if type(obj) == str: return ""
		elif isinstance(obj, LangString): return LangString("")
		elif type(obj) == list: return []

	if nr < 0: nr = objsize - index
	if (index+nr) > objsize: nr = objsize - index

	if type(obj) == str: return obj[index:index+nr]
	elif isinstance(obj, LangString): return LangString(obj.s[index:index+nr])
	elif type(obj) == list: return obj[index:index+nr]
		
	raise LangError("Unreachable code!!")

def builtin_unmake(I, obj):
	#print("UNMAKE:", fmtStackPrint(obj))
	fn = lambda x: x
	if type(obj) == str: 
		seq = obj
		fn = lambda x: ord(x)
	elif isinstance(obj, LangString): 
		seq = obj.s
		fn = lambda x: ord(x)
	elif type(obj) == list: seq = obj
	elif isinstance(obj, LangLambda): 
		# must deepcopy, see DESIGN-NOTES.md
		I.push(deepcopy(obj.objlist))
		return
	elif isinstance(obj, LangClosure):
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
	if type(obj) == str: I.pushInt(len(obj))
	elif isinstance(obj, LangString): I.pushInt(len(obj.s))
	elif type(obj) == list: I.pushInt(len(obj))
	else: raise LangError("Object doesn't support 'length': " + fmtStackPrint(obj))

def builtin_make_word(I):
	name = popSymbol(I)
	objlist = I.pop()
	if type(objlist) != list:
		raise LangError("make-word expects list but got: " + fmtStackPrint(objlist))
	
	I.defineWord(name, objlist, ALLOW_OVERWRITE_WORDS)
	
def builtin_append(I):
	#print("APPEND:")
	#print(I.reprStack())

	obj = I.pop()
	_list = I.pop()
	if type(_list) != list:
		raise LangError("append expecting list but got: " + fmtStackPrint(_list))

	_list.append(obj)
	I.push(_list)

def builtin_make_closure(I):
	state = I.pop()
	objlist = popList(I)
	# as with lambda, deepcopy objlist
	I.push(LangClosure(deepcopy(objlist),state))

def builtin_self_get(I):
	if I.closure is None: raise LangError("Attempting to reference unbound self")
	I.push(I.closure.state)

def builtin_self_set(I):
	if I.closure is None: raise LangError("Attempting to set unbound self")
	I.closure.state = I.pop()
	
def builtin_put(I):
	obj = I.pop()
	index = popInt(I)
	_list = popList(I)
	if index < 0 or index >= len(_list): raise LangError("Index out of range in put")
	_list[index] = obj
	I.push(_list)

def builtin_bit_shr(I):
	nr = popInt(I)
	a = popInt(I)
	I.push((a>>nr) & 0xffffffff)

def builtin_bit_shl(I):
	nr = popInt(I)
	a = popInt(I)
	I.push((a<<nr) & 0xffffffff)

import sys
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
	'.c': ([int], lambda I,a: sys.stdout.write(chr(a))),
	# object means any type
	# format TOS for stack display and push string
	'repr': ([object], lambda I,o: I.push(LangString(fmtStackPrint(o)))),
	# format TOS as display string and push
	'str': ([object], lambda I,o: I.push(LangString(fmtDisplay(o)))),
	# print string from TOS
	'puts': ([object], builtin_puts),
	'int?': ([object], lambda I,o: I.push(type(o) == int)),
	'float?': ([object], lambda I,o: I.push(type(o) == float)),
	'bool?': ([object], lambda I,o: I.push(type(o) == bool)),
	'list?': ([object], lambda I,o: I.push(type(o) == list)),
	'string?': ([object], lambda I,o: I.push(isinstance(o,LangString))),
	'symbol?': ([object], lambda I,o: I.push(type(o) == str)),
	'null?': ([], lambda I: I.push(I.pop() is None)),
	'lambda?': ([object], lambda I,o: I.push(isinstance(o,LangLambda))),
	'closure?': ([object], lambda I,o: I.push(isinstance(o,LangClosure))),
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
	'.showdef': ([], builtin_showdef),
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
	'null': ([], lambda I: I.push(None)),
	'cmdline-args': ([], lambda I: I.push(NATIVE_CMDLINE_ARGS)),
	'.dumpword': ([], lambda I: I.push(deepcopy(I.lookupWordOrFail(popSymbol(I))))),
	'read-file': ([], builtin_readfile),
	'make-closure': ([], builtin_make_closure),
	'self': ([], builtin_self_get),
	'self!': ([], builtin_self_set),
	'put': ([], builtin_put),
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
}

