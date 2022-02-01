"""
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from C++ version
"""
from math import floor
from errors import LangError
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

def builtin_divmod(I, a, b):
	quot,mod = int_divmod(a,b)
	I.push(mod)
	I.push(quot)

# the interpreter pops & checks the argument types, making the code shorter here
BUILTINS = {
	'+': ([int,int], lambda I,a,b: I.push(a+b)),
	'-': ([int,int], lambda I,a,b: I.push(a-b)),
	'*': ([int,int], lambda I,a,b: I.push(a*b)),
	'/mod': ([int,int], builtin_divmod),

}

