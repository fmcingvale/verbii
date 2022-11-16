from __future__ import annotations

from opcodes import opcode_pack, opcode_unpack
"""
	Langtypes - Types that don't exist in the host language.
	
	Copyright (c) 2022 Frank McIngvale, see LICENSE
"""

from errors import LangError
import sys
from io import BytesIO 

FLOAT_PRECISION = 17
# see c++ notes
MAX_VINT = (1<<53) - 1
MIN_VINT = -MAX_VINT

# i was using None for verbii null initially, but I think this led to too much
# ambiguity since in many places it is more pythonic to return None, NOT meaning
# the verbii null object. so to make the code more obvious in its intent, LangNull
# is now used for verbii null.
class LangNull(object):
	def __init___(self):
		pass

def isNull(obj): return isinstance(obj, LangNull)
def isInt(obj): return type(obj) == int
def isFloat(obj): return type(obj) == float
def isNumeric(obj): return type(obj) == int or type(obj) == float
def isBool(obj): return type(obj) == bool

def parseBool(text):
	if text == "true": return True
	elif text == "false": return False
	else: raise LangError("Bad boolean literal: " + text)

# need a void type that is differentiated from null (see c++ notes in langtypes.hpp)
class LangVoid(object):
	def __init__(self):
		pass

def isVoid(obj): return isinstance(obj, LangVoid)

# since symbols are far more common than strings,
# Python strings are used for symbols and this class is used
# for strings.
#
# verbii strings are defined as holding binary 8-bit data, NOT unicode.
# so the underlying storage here needs to be bytes and not a python string.
class LangString(object):
	# can be initialized with a plain ASCII string (which is converted to bytes),
	# or by passing bytes directly
	def __init__(self, s):
		# hmmm ... the initial write seems to be necessary to allow further appends as intended ..
		# i.e. BytesIO(val) then .write(val2) overwrites val
		if isinstance(s,str): self.sbuf = BytesIO(); self.sbuf.write(s.encode('ascii'))
		elif isinstance(s, bytes): self.sbuf = BytesIO(); self.sbuf.write(s)
		else: raise LangError("String requires ASCII string or bytes, got: " + str(s))
		
	# define these so LangString can be used directly for dict keys
	def __lt__(self, b):
		return self.sbuf.getvalue() < b.sbuf.getvalue()

	def __eq__(self, b):
		return isString(b) and self.sbuf.getvalue() == b.sbuf.getvalue()

	def __hash__(self):
		return hash(self.sbuf.getvalue())

def isString(obj): return isinstance(obj,LangString)
def isSymbol(obj): return type(obj) == str
def isList(obj): return type(obj) == list
def isDict(obj): return type(obj) == dict

class LangLambda(object):
	"from { ... } - a lambda/anonymous word"
	def __init__(self, objlist):
		self.objlist = objlist

	def __str__(self):
		return "<lambda>"

def isLambda(obj): return isinstance(obj, LangLambda)

class LangOpcode(object):
	def __init__(self, packed):
		self.code,self.A,self.B,self.C = opcode_unpack(packed)
		
	def packed(self):
		return opcode_pack(self.code,self.A,self.B,self.C)

def isOpcode(obj): return isinstance(obj, LangOpcode)

class CallFrameData(object):
	# this is the maximum number args + locals a function can have
	#
	# ** KEEP THIS SYNCED WITH THE C++ VALUE **
	MAX_CALLFRAME_SLOTS = 32

	def __init__(self):
		self.slots = [0] * self.MAX_CALLFRAME_SLOTS
		self.outer = None
		# this is set to True if bind-lambda is called while this frame is active
		self.bound = False

	def setOuterFrame(self, outer): 
		self.outer = outer

	def findFrameUp(self, levels):
		frame = self
		while levels > 0:
			if not frame.outer:
				raise LangError("Null outer frame in findFrameUp()")

			levels -= 1
			frame = frame.outer

		return frame

	# get/set object in frame @ given levels up (0 == local frame)
	def getFrameObj(self, levels, index):
		if index < 0 or index >= len(self.slots):
			raise LangError("Out of bounds in CallFrameData.getLocalObj()")

		frame = self.findFrameUp(levels)
		return frame.slots[index]

	def setFrameObj(self, levels, index, obj):
		if index < 0 or index >= len(self.slots):
			raise LangError("Out of bounds in CallFrameData.getLocalObj()")

		frame = self.findFrameUp(levels)
		frame.slots[index] = obj

class LangBoundLambda(object):
	def __init__(self, _lambda, outer):
		self.objlist = _lambda.objlist
		self.outer = outer

def isBoundLambda(obj): return isinstance(obj, LangBoundLambda)

def fmtDisplayObjlist(objlist, open_delim, close_delim):
	rlist = [open_delim]
	for o in objlist:
		rlist.append(fmtDisplay(o))

	rlist.append(close_delim)
	return ' '.join(rlist)

# format bytes for display as a string. turn non-printable chars into "\xCODE" and
# printable chars into their ASCII values.
def bytesFormatForDisplay(b):
	if not isinstance(b,bytes): raise LangError("Expecting bytes here, got: " + str(b))
	o = ""
	for c in b:
		if c < 32 or c > 126: o += "\\x{0:02x}".format(c)
		else: o += chr(c)

	return o

def fmtDisplay(obj):
	"see c++ comments for display vs. stack format"
	if isNull(obj):
		return "<null>"
	elif isVoid(obj):
		return "<*void*>"
	elif isInt(obj):
		return str(obj)
	elif isFloat(obj):
		fmt = "{0:." + str(FLOAT_PRECISION) + "g}"
		return fmt.format(obj)
	elif isBool(obj): 
		if obj is True: return "true"
		else: return "false"
	elif isLambda(obj):
		return  fmtDisplayObjlist(obj.objlist,"{","}")
	elif isBoundLambda(obj):
		return '<bound ' + fmtDisplayObjlist(obj.objlist,"{","}") + '>'
	elif isList(obj):
		return fmtDisplayObjlist(obj, '[', ']')
	elif isDict(obj):
		s = "{ "
		# have to sort keys to give identical output across languages
		for k in sorted(obj.keys()):
			s += '"' + fmtDisplay(k) + '" => ' + fmtDisplay(obj[k]) + " "
		s += "}"
		return s
	elif isString(obj):
		# turns bytes into printable string
		return bytesFormatForDisplay(obj.sbuf.getvalue())
		
	elif isSymbol(obj): return obj
		
	elif isOpcode(obj):
		return fmtStackPrint(obj)
	else:
		# special case ... very like i'm being called from an exception so 
		# print & exit() instead of raising LangError
		#raise LangError("Don't know how to print object: " + str(obj))
		print("Don't know how to print object: " + str(obj) + str(type(obj)))
		sys.exit(1)

def fmtStackPrintObjlist(objlist, open_delim, close_delim):
	rlist = [open_delim]
	for o in objlist:
		rlist.append(fmtStackPrint(o))

	rlist.append(close_delim)
	return ' '.join(rlist)

def fmtStackPrint(obj):
	"see c++ comments for display vs. stack format"
	if isNull(obj):
		return "<null>"
	elif isVoid(obj):
		return "<*void*>"
	elif isInt(obj):
		return str(obj)
	elif isFloat(obj):
		fmt = "{0:." + str(FLOAT_PRECISION) + "g}"
		return '#' + fmt.format(obj)
	elif isBool(obj): return "<true>" if obj else "<false>"
	elif isLambda(obj):
		return fmtStackPrintObjlist(obj.objlist,"{","}")
	elif isBoundLambda(obj):
		return '<bound ' + fmtStackPrintObjlist(obj.objlist,"{","}") + '>'
	elif isString(obj):
		# in a stack display, strings get " ... "
		return '"' + fmtDisplay(obj) + '"'
	elif isSymbol(obj):
		return "'" + fmtDisplay(obj)
	elif isList(obj):
		return fmtStackPrintObjlist(obj, '[', ']')
	elif isDict(obj):
		s = "{ "
		# have to sort keys to give identical output across languages
		for k in sorted(obj.keys()):
			s += fmtStackPrint(k) + ' => ' + fmtStackPrint(obj[k]) + " "
		s += "}"
		return s
	elif isOpcode(obj):
		from opcodes import opcode_code_to_name
		s = "#op( " + opcode_code_to_name(obj.code) + " " + str(obj.A) + " " + \
				str(obj.B) + " " + str(obj.C) + " )"
		return s
	else:
		# as above
		#raise LangError("Don't know how to print object: " + str(obj))
		print("Don't know how to print object: " + str(obj) + str(type(obj)))
		sys.exit(1)

def deepcopyObjlist(objlist):
	newlist = []
	for obj in objlist:
		newlist.append(deepcopy(obj))

	return newlist

# see c++ implementation & DESIGN-NOTES.md
def deepcopy(obj):
	if isNull(obj) or isVoid(obj) or isNumeric(obj) or isBool(obj) or \
		isLambda(obj) or \
		isSymbol(obj) or isOpcode(obj):
		return obj
	elif isString(obj): # strings are mutable
		return LangString(obj.sbuf.getvalue())
	elif isList(obj):
		return deepcopyObjlist(obj)
	elif isDict(obj):
		nd = {}
		for k in obj.keys():
			nd[k] = deepcopy(obj[k])
		return nd
	else:
		raise LangError("Don't know how to deepcopy: " + str(obj))