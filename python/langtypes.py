from __future__ import annotations
"""
	Langtypes - Types that don't exist in the host language.
	
	Copyright (c) 2022 Frank McIngvale, see LICENSE
"""

from errors import LangError

FLOAT_PRECISION = 17
# see c++ notes
MAX_VINT = (1<<53) - 1
MIN_VINT = -MAX_VINT

def isInt(obj): return type(obj) == int
def isFloat(obj): return type(obj) == float
def isNumeric(obj): return type(obj) == int or type(obj) == float
def isBool(obj): return type(obj) == bool

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
	if obj is None:
		return "<null>"
	elif type(obj) is int:
		return str(obj)
	elif type(obj) is float:
		fmt = "{0:." + str(FLOAT_PRECISION) + "g}"
		return fmt.format(obj)
	elif obj is True:
		return "true"
	elif obj is False:
		return "false"
	elif isinstance(obj, LangLambda):
		return "<" + fmtDisplayObjlist(obj.objlist,"{","}") + ">"
	elif isinstance(obj, LangClosure):
		return "<" + fmtDisplayObjlist(obj.objlist,"{","}") + " :: " + fmtDisplay(obj.state) + ">"
	elif type(obj) == list:
		return fmtDisplayObjlist(obj, '[', ']')
	elif type(obj) == dict:
		s = "{ "
		# have to sort keys to give identical output across languages
		for k in sorted(obj.keys()):
			s += '"' + k + '" => ' + fmtDisplay(obj[k]) + " "
		s += "}"
		return s
	elif isinstance(obj, LangString):
		return obj.s
	elif type(obj) is str:
		return obj
	else:
		raise LangError("Don't know how to print object: " + str(obj))

def fmtStackPrintObjlist(objlist, open_delim, close_delim):
	rlist = [open_delim]
	for o in objlist:
		rlist.append(fmtStackPrint(o))

	rlist.append(close_delim)
	return ' '.join(rlist)

def fmtStackPrint(obj):
	"see c++ comments for display vs. stack format"
	if obj is None:
		return "<null>"
	elif type(obj) is int:
		return str(obj)
	elif type(obj) is float:
		fmt = "{0:." + str(FLOAT_PRECISION) + "g}"
		return '#' + fmt.format(obj)
	elif obj is True:
		return "<true>"
	elif obj is False:
		return "<false>"
	elif isinstance(obj, LangLambda):
		return "<" + fmtStackPrintObjlist(obj.objlist,"{","}") + ">"
	elif isinstance(obj, LangClosure):
		return "<" + fmtStackPrintObjlist(obj.objlist,"{","}") + " :: " + fmtStackPrint(obj.state) + ">"
	elif isinstance(obj, LangString):
		# in a stack display, strings get " ... "
		return '"' + obj.s + '"'
	elif type(obj) is str:
		return "'" + obj
	elif type(obj) == list:
		return fmtStackPrintObjlist(obj, '[', ']')
	elif type(obj) == dict:
		s = "{ "
		# have to sort keys to give identical output across languages
		for k in sorted(obj.keys()):
			s += '"' + k.s + '" => ' + fmtStackPrint(obj[k]) + " "
		s += "}"
		return s
	else:
		raise LangError("Don't know how to print object: " + str(obj))

def deepcopyObjlist(objlist):
	newlist = []
	for obj in objlist:
		newlist.append(deepcopy(obj))

	return newlist

# see c++ implementation & DESIGN-NOTES.md
def deepcopy(obj):
	if obj is None or type(obj) is int or type(obj) is float or obj is True or \
		obj is False or isinstance(obj, LangLambda) or isinstance(obj, LangClosure) or \
		isinstance(obj, LangString) or type(obj) is str:
		return obj
	elif type(obj) == list:
		return deepcopyObjlist(obj)
	elif type(obj) == dict:
		nd = {}
		for k in obj.keys():
			nd[k] = deepcopy(obj[k])
		return nd
	else:
		raise LangError("Don't know how to deepcopy: " + str(obj))