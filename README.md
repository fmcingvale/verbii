
Core ideas:
# Simple - any languages features that become un-simple are out.
# Small & easy to port - ability to run on many host languages for fun/comparison purposes.
# Interpreted-only - no parsing, compiling, etc. Run directly from source text.

Types:
# integers - range [-1073741823, +1073741823] - values outside this range must raise an error
# booleans - values true & false - must be a distinct type from integers - must raise error when non-boolean received in boolean context (like 'if')
# ports can use any internal representation, but the integer range is chosen to fit in 31 bits to allow tagged values if desired, but tagging is not required

Limits/Requirements:
# Unittests must produce identical results to C++ reference implementation.
# Errors must be reported without exiting the interpreter.
# After errors, the stack should be cleared and not printed.
# It is allowed, but not required, for interpreter to restart after error.


