from __future__ import annotations
"""
	Langtypes - Types that don't exist in the host language.
	
	Copyright (c) 2022 Frank McIngvale, see LICENSE
"""

from errors import LangError
import sys

FLOAT_PRECISION = 17
# see c++ notes
MAX_VINT = (1<<53) - 1
MIN_VINT = -MAX_VINT

def isNull(obj): return obj is None
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
# for strings
class LangString(object):
	def __init__(self, s):
		self.s = s

	# define these so LangString can be used directly for dict keys
	def __lt__(self, b):
		return self.s < b.s

	def __eq__(self, b):
		return isString(b) and self.s == b.s

	def __hash__(self):
		return hash(self.s)

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

class LangClosure(object):
	def __init__(self, objlist, state):
		self.objlist = objlist
		self.state = state

	def __str__(self):
		pass

def isClosure(obj): return isinstance(obj, LangClosure)

def fmtDisplayObjlist(objlist, open_delim, close_delim):
	rlist = [open_delim]
	for o in objlist:
		rlist.append(fmtDisplay(o))

	rlist.append(close_delim)
	return ' '.join(rlist)

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
		return "<" + fmtDisplayObjlist(obj.objlist,"{","}") + ">"
	elif isClosure(obj):
		return "<" + fmtDisplayObjlist(obj.objlist,"{","}") + " :: " + fmtDisplay(obj.state) + ">"
	elif isList(obj):
		return fmtDisplayObjlist(obj, '[', ']')
	elif isDict(obj):
		s = "{ "
		# have to sort keys to give identical output across languages
		for k in sorted(obj.keys()):
			s += '"' + k + '" => ' + fmtDisplay(obj[k]) + " "
		s += "}"
		return s
	elif isString(obj):
		return obj.s
	elif isSymbol(obj):
		return obj
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
		return "<" + fmtStackPrintObjlist(obj.objlist,"{","}") + ">"
	elif isClosure(obj):
		return "<" + fmtStackPrintObjlist(obj.objlist,"{","}") + " :: " + fmtStackPrint(obj.state) + ">"
	elif isString(obj):
		# in a stack display, strings get " ... "
		return '"' + obj.s + '"'
	elif isSymbol(obj):
		return "'" + obj
	elif isList(obj):
		return fmtStackPrintObjlist(obj, '[', ']')
	elif isDict(obj):
		s = "{ "
		# have to sort keys to give identical output across languages
		for k in sorted(obj.keys()):
			s += '"' + k.s + '" => ' + fmtStackPrint(obj[k]) + " "
		s += "}"
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
		isLambda(obj) or isClosure(obj) or \
		isString(obj) or isSymbol(obj):
		return obj
	elif isList(obj):
		return deepcopyObjlist(obj)
	elif isDict(obj):
		nd = {}
		for k in obj.keys():
			nd[k] = deepcopy(obj[k])
		return nd
	else:
		raise LangError("Don't know how to deepcopy: " + str(obj))