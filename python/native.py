"""
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from C++ version
"""
from math import floor
import math
from xml.dom.minidom import ReadOnlySequentialNamedNodeMap
from errors import LangError
from interpreter import Interpreter
from langtypes import LangLambda, LangString, fmtDisplay, fmtStackPrint, \
				isNumeric, deepcopy, isString, isSymbol, \
				isList, isLambda, isDict, isInt, isFloat, isBool, \
				isNull, LangVoid, isVoid, LangOpcode, \
				LangBoundLambda, isBoundLambda, isOpcode
from opcodes import opcode_pack, opcode_name_to_code
import time, sys, os

# has to be set externally
ALLOW_OVERWRITE_WORDS = False
EXIT_ON_EXCEPTION = True
STACKTRACE_ON_EXCEPTION = True

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

def popLambda(I):
	obj = I.pop()
	if isLambda(obj):
		return obj
	else:
		raise LangError("Expecting lambda but got: " + fmtStackPrint(obj))

def popList(I):
	obj = I.pop()
	if isList(obj):
		return obj
	else:
		raise LangError("Expecting list but got: " + fmtStackPrint(obj))

def popDict(I):
	obj = I.pop()
	if isDict(obj):
		return obj
	else:
		raise LangError("Expecting dict but got: " + fmtStackPrint(obj))

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
	I.push(list(I._WORDS.keys()))

def builtin_file_read(I):
	filename = popString(I, "file-read")
    # 'rb' so it doesn't translate EOL chars ...
	f = open(filename,'rb')
    # ... but still want a string, not bytes
	buf = str(f.read(),'ascii')
	I.push(LangString(buf))
	f.close()

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
	elif isBoundLambda(a): return False # same for bound lambdas
	elif isOpcode(a): 
		from opcodes import opcode_unpack
		if not isOpcode(b): return False
		return a.code == b.code and a.A == b.A and a.B == b.B and a.C == b.C
	elif isVoid(a): return isVoid(b)
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

def builtin_extend(I):
	src = popList(I)
	_list = popList(I)
	_list.extend(src)
	I.push(_list)

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
		if index < 0 or index >= len(obj): I.push(LangVoid()) # out of bounds -> void
		else: I.push(obj[index])
	elif isString(obj):
		if not isInt(index): raise LangError("get requires index, got: " + fmtStackPrint(index))
		if index < 0: index += len(obj.s) # negative indexes
		if index < 0 or index >= len(obj.s): I.push(LangVoid()) # out of bounds -> void
		else: I.push(LangString(obj.s[index]))
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

def builtin_set_stacktrace_on_exception(I, flag):
	global STACKTRACE_ON_EXCEPTION
	STACKTRACE_ON_EXCEPTION = flag
	
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

def builtin_file_write(I):
	text = popString(I,'file-write')
	filename = popString(I,'file-write')
	f = open(filename, 'w')
	f.write(text)
	f.close()

def builtin_file_append(I):
	text = popString(I,'file-append')
	filename = popString(I,'file-append')
	f = open(filename, 'a')
	f.write(text)
	f.close()

def builtin_file_delete(I):
	filename = popString(I,'file-delete')
	if os.path.isfile(filename):
		os.remove(filename)

# name A B C make-opcode -> opcode
def builtin_make_opcode(I):
	C = popInt(I)
	B = popInt(I)
	A = popInt(I)
	name = popStringOrSymbol(I,"make-opcode")
	
	# range checks
	if A < 0 or A > 255:
		raise LangError("A must be [0-255] in make-opcode, got: " + str(A))

	if B < 0 or B > 65535:
		raise LangError("B must be [0-65535] in make-opcode, got: " + str(B));

	if C < 0 or C > 0x000fffff:
		raise LangError("C must be [0-1048575] in make-opcode, got: " + str(C))

	I.push(LangOpcode(opcode_pack(opcode_name_to_code(name), A, B, C)))

def builtin_opcode_packed(I):
	op = I.pop()
	if not isOpcode(op):
		raise LangError("Expecting opcode in opcode-packed but got: " + fmtStackPrint(op))

	I.push(op.packed())

def builtin_bind_lambda(I):
	_lambda = popLambda(I)
	# remember currently active frame and mark it as bound so that the
	# interpreter knows not to free/reuse it
	I.framedata.bound = True
	I.push(LangBoundLambda(_lambda, I.framedata))
	
def builtin_file_sep(I):
	I.push(LangString(os.sep))

def builtin_os_getcwd(I):
	I.push(LangString(os.getcwd()))
	
def builtin_atan2(I):
	x = popIntOrFloat(I)
	y = popIntOrFloat(I)
	I.push((math.atan2(y,x)))

def builtin_pow(I):
	y = popIntOrFloat(I)
	x = popIntOrFloat(I)
	I.push((math.pow(x,y)))

# FNV-1a 32-bit hash
# See notes in C++ version
def builtin_fnv_1a_32(I):
	text = popString(I,"fnv-1a-32")
	hash = 2166136261 # offset basis
	for c in text:
		hash = hash ^ ord(c)
		hash = hash * 16777619 # FNV prime
		# I added this
		hash = hash & 0xffffffff

	I.push(hash)

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
	'opcode?': ([object], lambda I,o: I.push(isOpcode(o))),
	'bound-lambda?': ([object], lambda I,o: I.push(isBoundLambda(o))),
	# [] for no args
	'depth': ([], lambda I: I.push(I.SP_EMPTY - I.SP)),
	'ref': ([int], builtin_ref),
	'set!': ([object,int], builtin_set),
	'SP': ([], lambda I: I.push(I.SP)),
	'SP!': ([int], builtin_setsp),
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
	'extend': ([], builtin_extend),
	'void': ([], lambda I: I.push(LangVoid())),
	'.dumpword': ([], lambda I: I.push(deepcopy(I.lookupWordOrFail(popSymbol(I))))),
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
	'cpu-time': ([], lambda I: I.push(time.process_time())),
	',,new-dict': ([], lambda I: I.push({})),

	# new words needed for running boot.verb
	'file-exists?': ([], lambda I: I.push(os.path.isfile(popString(I,'file-exists?')))),
	'file-mtime': ([], lambda I: I.push(os.path.getmtime(popString(I,'file-mtime')))),
	'open-as-stdout': ([], builtin_open_as_stdout),
	'deserialize': ([], builtin_deserialize),
	'prompt': ([], builtin_prompt),
	'set-exit-on-exception': ([bool], builtin_set_exit_on_exception),
	'set-allow-overwrite-words': ([bool], builtin_set_allow_overwrite_words),
	'set-stacktrace-on-exception': ([bool], builtin_set_stacktrace_on_exception),

	'time-string': ([], lambda I: I.push(LangString(time.strftime("%Y-%m-%d %H:%M:%S")))),
	'floor': ([], lambda I: I.push(int(floor(popIntOrFloat(I))))),

	'file-write': ([], builtin_file_write),
	'file-append': ([], builtin_file_append),
	'file-read': ([], builtin_file_read),
	'file-delete': ([], builtin_file_delete),
	
	'sys-platform': ([], lambda I: I.push(LangString("Python {0}.{1}.{2}".format(sys.version_info.major, sys.version_info.minor, sys.version_info.micro)))),

	'keys': ([], lambda I: I.push(list(popDict(I)))),

	'sin': ([], lambda I: I.push(math.sin(popIntOrFloat(I)))),
	'cos': ([], lambda I: I.push(math.cos(popIntOrFloat(I)))),
	'tan': ([], lambda I: I.push(math.tan(popIntOrFloat(I)))),
	'asin': ([], lambda I: I.push(math.asin(popIntOrFloat(I)))),
	'acos': ([], lambda I: I.push(math.acos(popIntOrFloat(I)))),
	'atan2': ([], builtin_atan2),
	'sqrt': ([], lambda I: I.push(math.sqrt(popIntOrFloat(I)))),
	'log': ([], lambda I: I.push(math.log(popIntOrFloat(I)))),
	'pow': ([], builtin_pow),
	'exp': ([], lambda I: I.push(math.exp(popIntOrFloat(I)))),

	# 'version 2' closures
	'make-opcode': ([], builtin_make_opcode),
	'opcode-packed': ([], builtin_opcode_packed),
	'bind-lambda': ([], builtin_bind_lambda),

	# more os/fileops
	'file-pathsep': ([], builtin_file_sep),
	'os-getcwd': ([], builtin_os_getcwd),

	# fast hash
	'fnv-1a-32': ([], builtin_fnv_1a_32),
}

