
Core ideas:
# Simple - any languages features that become un-simple are out.
# Small & easy to port - ability to run on many host languages for fun/comparison purposes.
# Interpreted-only - no parsing, compiling, etc. Run directly from source text.

Types:
# integers - range [-1073741823, +1073741823] - values outside this range must raise an error
# booleans - values true & false - must be a distinct type from integers
# ports can use any internal representation, but the integer range is chosen to fit in 31 bits to allow tagged values if desired, but tagging is not required


