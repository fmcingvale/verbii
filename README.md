
## Core ideas:
- Simple - any languages features that become un-simple are out.
- Small & easy to port - ability to run on many host languages for fun/comparison purposes.
	- Goal: Source for any particular port should be around 1000 lines or less of host language code (as counted by cloc), not counting any unittests in the host language.
	- Current stats (updated periodically):
	- C++ ~786 lines of native code+headers, excluding unittests
	- Python ~480 lines of native code, excluding unittests
	- Lua ~560 lines of native code, excluding unittests
- Interpreted-only - no parsing, compiling, etc. Run directly from source texts. In addition to simplicity, this allows things like forward-refererences for free, since nothing is evaluated until runtime. Plus it allows easy source-level introspection since nothing is translated, and can even do interesting things like rewrite the source at runtime.

## Types:
- integers - range [-1073741823, +1073741823] - values outside this range must raise an error
- booleans - values true & false - must be a distinct type from integers - must raise error when non-boolean received in boolean context (like "if")
- floats - these are the host language double type. originally i was going to make a custom "lite" floating point implementation, to avoid cross-language issues with rounding, etc., but even a minimal version was 100+ lines of code. so i decided native floats are ok ("simplicity" rule) and will deal with rounding issues in the implementation of unittests.
- ports can use any internal representation, but the integer range is chosen to fit in 31 bits to allow tagged values if desired, but tagging is not required. if a host language has a suitable object system, garbage collection, etc., those can be freely used as long as the type limitations are enforced. (currenly no ports use tagged values, however some scripting language have a 31 bit limit so this intended for compatibility with those languages)
- related to the above, ports can use any memory model they wish, the only requirement is that pointer addition works as expected in the language -- i.e. 'SP 1 + ref', '123 X 10 + set!', 'LP 2 + ref', etc.

## Limits/Requirements:
- Unittests must produce identical results to C++ reference implementation. i.e. a diff of the output of 'unittest_NAME' to 'expect_unittest_NAME' must be empty, with the exception that trailing-spaces and line endings (\r, \n, etc.) are allowed to differ from the expected result.
- All errors must be reported without crashing the program.
- After errors occur, the stack must be cleared and not printed (in order to match the expected results).
- It is allowed, but not required, for the interpreter to restart after errors. However, this must be transparent to the user.

## Language spec:

It's sort of weird to try and define a grammar since everything is word based and all this
can be fairly easily changed/expanded. However, these are the core words that I think
define what the language is.
```
program := item ...
item := integer
		if jump-true				( take jump-true if TOS is true )
		if jump-true jump-false		( take jump-true or jump-false based on TOS)
		var NAME SIZE				( alloc SIZE memory elements at address NAME )
		ref							( deference address and put value on stack )
		set!						( pop item and put in memory address )
		del NAME					( delete user-defined word )
		return						( stop running current wordlist )
		>>NAME						( forward jump to @NAME )
		<<NAME						( backward jump to @NAME)
		@NAME						( jump target )
		{ ... }						( lambda [anonymous functions] - nesting OK )
		call						( call lambda )
		NAME						( call a builtin or user-defined word )
		( ... )						( comment -- non-nesting )
		$< ... >					( for internal use in interpreter, don't use in user code )
NAME := any non-whitespace sequence that isn't a keyword above
```

## Known issues/TODO
- C++ implementation never frees memory/deletes objects. Need to implement or add libgc.
- C++ implementation -- need to make pointer/reference/value passing consistent as part of the above.
