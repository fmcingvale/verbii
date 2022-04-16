from __future__ import annotations
"""
	Langtypes - Types that don't exist in the host language.
	
	Copyright (c) 2022 Frank McIngvale, see LICENSE
"""

from errors import LangError

FLOAT_PRECISION = 17
MAX_INT_31 = (1<<30) - 1
MIN_INT_31 = -MAX_INT_31

def isNumeric(obj):
	return type(obj) == int or type(obj) == float

# since symbols are far more common than strings,
# Python strings are used for symbols and this class is used
# for strings
class LangString(object):
	def __init__(self, s):
		self.s = s

class LangLambda(object):
	"from { ... } - a lambda/anonymous word"
	def __init__(self, objlist):
		self.objlist = objlist

	def __str__(self):
		return "<lambda>"

class LangClosure(object):
	def __init__(self, objlist, state):
		self.objlist = objlist
		self.state = state

	def __str__(self):
		pass

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
		return "<lambda>"
	elif isinstance(obj, LangClosure):
		return fmtStackPrint(obj)
	elif isinstance(obj, LangString):
		return obj.s
	elif type(obj) is str:
		# strings are symbols - not normally printed, so they get a ' to differentiate from strings
		return "'" + obj
	elif type(obj) == list:
		return fmtStackPrint(obj) # use stack format for both
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
		return "<lambda>"
	elif isinstance(obj, LangClosure):
		return "<" + fmtStackPrintObjlist(obj.objlist,"{","}") + " :: " + fmtStackPrint(obj.state) + ">"
	elif isinstance(obj, LangString):
		# in a stack display, strings get " ... "
		return '"' + obj.s + '"'
	elif type(obj) is str:
		# ... and symbols do not get ' here
		return obj
	elif type(obj) == list:
		return fmtStackPrintObjlist(obj, '[', ']')
	else:
		raise LangError("Don't know how to print object: " + str(obj))
