
Random design notes on why things are the way that they are ...

void vs null
----------------------------

Originally there was a void type that was *only* used internally. It was used
to indicate a 'nothing' value since null is not nothing (a good example is parsing
JSON where null is a literal - it is more natural to have a void value that means
EOF (or similar) and null is the literal null value.

My initial decision was to make void *never* show up in verbii code. However, over time
I saw the usefulness of void in verbii code just like in the host language -- to differentiate
'null' (an actual object) from 'nothing'.

Some way that void is currently used:
	- as above, to differentiate 'null' from EOF in several places like parsing functions
	- trying to get a nonexistent key from a dictionary => void
		- so there is no need for a 'key-exists?' type native function
	- trying to get from a string/symbol/list wwith an out-of-bounds index returns void
		- this makes looping simpler in many cases -- no need to check against length,
		  just do list[i++] until void is returned.
	_ NOT YET IMPLEMENTED: void will eventually be used to delete items from dicts/lists instead
		of having seperate deletion functions
		
Consequences of void:
	- void can NEVER be a valid stored object
		- you can't tell the difference between a void stored in a list/dict from 
			trying to access a non-existent index/key
		- this is BY DESIGN and will not change

List literals & deepcopying:
----------------------------

A list literal: [ a b c ]
SHOULD have this meaning: 'a 'b 'c 3 make-list 

However, the parser will insert the list OBJECT [ a b c ] into the 
compiled code, NOT the sequence ending with 'make-list'. This can cause a
problem if the interpreter simply pushes lists when it encounters them.
An example demonstrates ...

: oops
	\ the INTENTION is start with an empty list ...
	[ ]
	\ then add some stuff
	1 append 2 append 3 append
	\ so I THINK I am returning [ 1 2 3 ] now ....
	;

Unfortunately, if the interpreter simply pushes the object that was created
by the compiler, then the SAME object will be reused on each call. So the
first call will return [ 1 2 3 ], the second call will return [ 1 2 3 1 2 3 ],
the next call [ 1 2 3 1 2 3 1 2 3 ], etc. 

Another common idiom is to build a function dynamically like a template:

: oops2
	[ 1 2 3 REPLACE_ME ] 3 rot put
	;

This SHOULD replace REPLACE_ME with the top item from the stack. However, if a second
caller calls 'oops2' then they will receive the same object as the first caller, so the
first caller's list is now wrong from their perspective.

Possible solutions:
	1. Rework the compiler so that it emits the longer code sequence ending with 'make-list'.
		* Ensures list literals always create a fresh list.
	2. Fix it in the interpreter.

Option 1 would be VERY invasive since the compiler makes the assumption that all parsed
entities compile to only 1 object, not a list of objects. This solution COULD be implemented,
but it would compilicate all callers of 'syntax-next' who would now need to expect possibly
multiple items.

Option 2 - fix it in the interpreter. When the interpreter encounters a list object in the
code stream, instead of pushing the object, it pushes a deep-copy of the object (since there
could be nested lists, and those would have the same "sharing the same object" problem as
above). At first glance, the downside of this would be the expensive deep-copy, however, if
Option 1 was implemented, then the interpreter would have to run the entire sequence ending
with 'make-list', which would be even MORE expensive. With Option 2, deep-copy can be implemented
in the host language and optimized as well as possible.

What about lambdas? 
-------------------

In verbii, lambdas are essentially just tagged lists. At one point I was going to get rid
of lambdas, until I realized the above issue and discovered they actually do have a use.

A lambda literal like: { a b c }
Has the semantic meaning: 'a 'b 'c 3 make-list make-lambda

So we're just shoving a list into this thing called a lambda. What's the benefit? The benefit
is that lambdas (unlike lists) are not writable -- once created, they are fixed. But there is
an important condition to take care of to ensure that lambdas are immutable.

Take this code: [ a b c ] dup make-lambda

Let's assume we've implemented Option 2 above, so that the code is INTERPRETED as:

	'a 'b 'c 3 make-list dup make-lambda

So even though make-list ensures we have a UNIQUE instance of a list, the following dup wrecks
it for the lambda. After make-lambda, if the dup'd list were modified then the lambda would
see those changes as well.

Therefore the second condition is: make-lambda must do a deep-copy of its input list.

Since lambdas are immutable, you can dup lambdas all day long with no need to deep-copy anything
again.

Side note: A really bad way to avoid list side effects would be to force a deep-copy on ANY 
copying operation like 'dup'. However, there are LOTS of good reasons to use shared lists,
so I think the much better solution is to only deep-copy where absolutely necessary to not
break the language semantics.

So going back to list literals in source code, take this: [ 1 2 3 { a b c } ]

When the interpreter receives this as an object and goes to deep-copy it, it does NOT have
to dig into the lambda at all -- a shallow copy is perfectly fine since as noted before it
is immutable.

Also given just a lambda: { a b c }, the compiler generates a lambda object -- the list inside
of it is already unique, so the interpreter can simply push it to the stack with no extra
handling. Lambda copying of lists ONLY occurs with an explict 'make-lambda' call.

How about 'unmake'?
-------------------

Even though lambdas are immutable, 'unmake' gives you back the list. So a naive unmake 
would do:
	{ a b c } unmake -> the [ a b c ] object from the lambda

Now this is a problem since [ a b c ] can be modified now which would alter ALL copies of
the lambda. So the solution is the same as make-lambda: lists must be deep-copied when unmaking
a lambda.

Lists do NOT have to be deep-copied on unmake. For example:

	[ a b [ c d ] e f ] unmake --> 'a 'b [ c d ] 'e 'f 5

The [ c d ] could be modified which would change any other copies of the original list, however,
those are the intended semantics. Lists ONLY have to be made unique upon creation from a literal --
once created, any dup'd copies are MEANT to be the SAME object.

Could other types have this problem??
--------------------------------------

Many types are implemented as objects in various host languages. A common example is floating
point values -- often they are objects while integers are native values. However, the above issue
occurs when doing a 'put' or 'append' operation which modifies part of an existing list. There
is no equivalent operation on other types (strings are immutable so they are atoms in verbii
as well). For example the operations:
	1 1 +
	1.2 2.3 +
	"hello" "world" +

... these all create new objects, they never modify existing objects. As long as objects cannot
be modified in-place, then this issue should never occur for other types.


