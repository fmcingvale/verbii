;;;==================================================================================
;;; verbii in chicken scheme
;;;
;;; all one long file for now since i haven't figured out chicken modules ...
;;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;==================================================================================

;; this is my first try using coops and even records, so ... may be some suboptimal stuff here ...

;(import coops-primitive-objects)
(import srfi-13) ; string library
(import srfi-34) ; exceptions
(import (chicken format)) ; fprintf
(import srfi-1) ; list library
(import coops)
(import dyn-vector)
(import miscmacros) ; inc! dec! 
; shorthand
(define slot slot-value)

;;;==================================================================================
;;; langtypes.scm
;;;==================================================================================

; need a null type that is distinct from '() so that '() can be the empty list
; ---- ugh ... that's obsolete since I have LangList, but I still prefer LangNull to '()
(define-record LangNull)
(set-record-printer! LangNull (lambda (obj out) (fprintf out "<null>")))

; and a void type distinct from null & '()
(define-record LangVoid)
(set-record-printer! LangVoid (lambda (obj out) (fprintf out "<VOID>")))

; in scheme a number like 123.0 is an integer; in verbii it is a float, so cannot
; use scheme floats directly (at least I haven't figure out a way)
(define-record LangFloat value)
(set-record-printer! LangFloat (lambda (obj out) (fprintf out "#~S" (LangFloat-value obj))))

(define-record LangLambda objlist)
; when printing from scheme, print verbosely for debugging ... normally just print <lambda>
(set-record-printer! LangLambda
	(lambda (obj out) (fprintf out "#lambda<~S>" (LangLambda-objlist obj))))

; like the Python port, plain strings are used as symbols, and LangString is used
; for strings. i want to avoid any weirdness with special chars (like ') in symbols,
; so usings strings seems easiest

; add a predicate to help with code readability
(define LangSymbol? string?)

; verbii string type
(define-class LangString ()
	((value initform: "" accessor: value)))

	(define (LangString? obj) (subclass? (class-of obj) LangString))

	; get i'th char
	(define-method (get (str LangString) index) 
		(string-ref (value str) index)) 
		
	(define-method (len (str LangString)) 
		(string-length (value str)))

; verbii lists are really arrays, so use a vector
(define-class LangList () (
		(objlist accessor: objlist initform: (make-dynvector 0 0)) ))

	(define-method (get (llist LangList) index)
		(if (or (< index 0) (>= index (dynvector-length (objlist llist))))
			(raise "Out of bounds in LangList"))
		(dynvector-ref (objlist llist) index))

	(define-method (push-back (llist LangList) obj)
		(dynvector-set! (objlist llist) (dynvector-length (objlist llist)) obj))

	(define-method (len (llist LangList))
		(dynvector-length (objlist llist)))

	(define (LangList? obj) (subclass? (class-of obj) LangList))

	(define (list->LangList lst) 
		(print "LANGLIST FROM LIST: " lst)
		(make LangList 'objlist (list->dynvector lst)))

;(define (LangNull? obj) (subclass? (class-of obj) LangNull))

(define (fmtStackPrint obj)
	;(print "FMT_STACK_PRINT:" obj)
	(cond
		((integer? obj) (number->string obj))
		((LangFloat? obj) (string-append "#" (number->string (LangFloat-value obj))))
		((boolean? obj) (if obj "true" "false"))
		((LangSymbol? obj) obj) ; obj is a string
		((LangString? obj) (string-append "\"" (value obj) "\""))
		((LangList? obj)
			(string-append 
				(dynvector-fold (lambda (i str obj) (string-append str " " (fmtStackPrint obj))) "[" 
					(objlist obj)) " ]"))
		((LangNull? obj) "<null>")
		((LangVoid? obj) "<VOID>")
		((LangLambda? obj) "<lambda>")
		(else (raise "Unknown object in fmtStackPrint"))))

(define (fmtDisplay obj)
	(cond
		((integer? obj) (number->string obj))
		((LangFloat? obj) (number->string (LangFloat-value obj)))
		((boolean? obj) (if obj "true" "false"))
		((LangSymbol? obj) (string-append "'" obj)) ; obj is a string
		((LangString? obj) (value obj))
		; OTHER PORTS USE fmtStackPrint here ... trying this out to see if i like it better ...
		((LangList? obj)
			(string-append 
				(dynvector-fold (lambda (i str obj) (string-append str " " (fmtDisplay obj))) "[" 
					(objlist obj)) " ]"))
		((LangNull? obj) "<null>")
		((LangVoid? obj) "<VOID>")
		((LangLambda? obj) "<lambda>")
		(else (raise "Unknown object in fmtStackPrint"))))

(define (test-printing)
	(print "--- STACK FORMAT: ---")
	(print (fmtStackPrint 123))
	(print (fmtStackPrint (make-LangFloat (/ 1.0 3.0))))
	(print (fmtStackPrint #t))
	(print (fmtStackPrint #f))
	(print (fmtStackPrint "abc"))
	(print (fmtStackPrint (make-LangNull)))
	(print (fmtStackPrint (make LangString 'value "abc")))
	(let ((lst (make LangList)))
		(push-back lst 111)
		(push-back lst 222)
		(push-back lst (make LangString 'value "hello world"))
		(push-back lst "a-long-symbol-here")
		(print "LIST: " (fmtStackPrint lst)))

	(print "--- DISPLAY FORMAT: ---")
	(print (fmtDisplay 123))
	(print (fmtDisplay (make-LangFloat (/ 1.0 3.0))))
	(print (fmtDisplay #t))
	(print (fmtDisplay #f))
	(print (fmtDisplay "abc"))
	(print (fmtDisplay (make-LangNull)))
	(print (fmtDisplay (make LangString 'value "abc")))
	(let ((lst (make LangList)))
		(push-back lst 111)
		(push-back lst 222)
		(push-back lst (make LangString 'value "hello world"))
		(push-back lst "a-long-symbol-here")
		(print "LIST: " (fmtDisplay lst))))

(test-printing)

;;;==================================================================================
;;; Deserializer
;;;==================================================================================
(import srfi-69) ; hash-tables
(import (chicken io))

(define (replace-escapes text)
	(cond ; recursively apply until no more escapes remain
		((string-contains text "%32") => (lambda (n) (replace-escapes (string-replace text " " n (+ n 3)))))
		((string-contains text "%10") => (lambda (n) (replace-escapes (string-replace text "\n" n (+ n 3)))))
		((string-contains text "%13") => (lambda (n) (replace-escapes (string-replace text "\r" n (+ n 3)))))
		((string-contains text "%37") => (lambda (n) (replace-escapes (string-replace text "%" n (+ n 3)))))
		(else text)))

(define (deserialize-stream intr fileIn)
	(let ((line (string-trim-both (read-line fileIn))))
		(if (not (eof-object? line))
			(begin
				(print "Line: " line)
				(case (string-ref line 0)
					((#\i) (string->number (string-drop line 2)))
					((#\f) (make-LangFloat (string->number (string-drop line 2))))
					((#\n) (make-LangNull))
					((#\s) 
						(if (>= (string-length line) 2) ; watch for empty string
							(make LangString 'value (replace-escapes (string-drop line 2)))
							(make LangString 'value "")))
					((#\y) (string-drop line 2)) ; verbii symbols are scheme strings
					((#\b)
						(if (equal? (string-drop line 2) "true")
							#t #f))
					((#\L)
						(let read-list ((nr (string->number (string-drop line 2))) (lst '()))
							(if (> nr 0) 
								(read-list (- nr 1) (append lst (list (deserialize-stream intr fileIn))))
								(begin
									; other ports don't do this (yet?!) -- remove VOIDs from list
									(set! lst (filter (lambda (obj) (not (LangVoid? obj))) lst))
									(list->LangList lst)))))
					((#\F) ; lambda - read list
						(let ((objlist (deserialize-stream intr fileIn)))
							(if (LangList? objlist)
								(make-LangLambda objlist)
								(raise "Expecting list after 'F' but got: " (fmtStackPrint objlist)))))
					((#\W) ; W name followed by list
						(let ((name (string-drop line 2))
								(objlist (deserialize-stream intr fileIn)))
							(if (LangList? objlist)
								(begin
									(print "DESERIALIZED WORD: " objlist)
									(hash-table-set! (slot intr 'WORDS) name objlist)
									(make-LangVoid))
								(raise (string-append "Expecting list after 'W' but got: " (fmtStackPrint objlist))))))
					(else
						(print "Unknown char: " (string-ref line 0))))
			)
			(make-LangVoid) ; eof
		)
	)
)

(define (test-deserialize)
	(let* ((fileIn (open-input-file "data.txt"))
			(result (deserialize-stream fileIn)))
		(print "Raw result: " result)
		(print "DISPLAY: " (fmtDisplay result))
		(print "STACK: " (fmtStackPrint result))
		(close-input-port fileIn))

	(print "WORDS:" (hash-table-keys WORDS))
	(hash-table-walk WORDS (lambda (key val) (print key ": " (fmtStackPrint val)))))

;(test-deserialize)

;;;==================================================================================
;;; Interpreter
;;;==================================================================================

(define STACK_SIZE (expt 2 10))
(define LOCALS_SIZE (expt 2 10))
(define HEAP_STARTSIZE (expt 2 16))

(define-class <Interpreter> ()
	(
		(OBJMEM accessor: OBJMEM initform: (make-dynvector (+ STACK_SIZE LOCALS_SIZE HEAP_STARTSIZE) 0))

		(SP_MIN accessor: SP_MIN initform: 0)
		(SP_EMPTY accessor: SP_EMPTY initform: STACK_SIZE)
		(SP accessor: SP initform: STACK_SIZE)
		
		(LP_MIN accessor: LP_MIN initform: STACK_SIZE)
		(LP_EMPTY accessor: LP_EMPTY initform: (+ STACK_SIZE LOCALS_SIZE))
		(LP accessor: LP initform: (+ STACK_SIZE LOCALS_SIZE))

		(HEAP_NEXTFREE accessor: HEAP_NEXTFREE initform: (+ STACK_SIZE LOCALS_SIZE))

		; WORDS[name: string] = LangList
		(WORDS (make-hash-table #:test equal?))

		(code '()) ; currently running code (LangList)
		(codepos -1)
		(callstack '())
	)
)

	(define-method (allocate (intr <Interpreter>) nr)
		(let ((addr (HEAP_NEXTFREE intr)))
			(set! (HEAP_NEXTFREE intr) (+ (HEAP_NEXTFREE intr) nr))
			addr))

	(define-method (push (intr <Interpreter>) obj)
		(if (<= (SP intr) (SP_MIN intr))
			(raise "Stack overflow!"))
		;(set! (SP intr) (- (SP intr) 1))
		(dec! (SP intr))
		(dynvector-set! (OBJMEM intr) (SP intr) obj))

	(define-method (pop (intr <Interpreter>))
		(if (>= (SP intr) (SP_EMPTY intr))
			(raise "Stack underflow!"))
		;(set! (SP intr) (+ (SP intr) 1))
		(inc! (SP intr))
		(dynvector-ref (OBJMEM intr) (- (SP intr) 1)))
		
	(define-method (reprStack (intr <Interpreter>))
		(let loop ((s "") (i (- (SP_EMPTY intr) 1)))
			(if (>= i (SP intr))
				(loop (string-append s " " (fmtStackPrint (dynvector-ref (OBJMEM intr) i)))
					(- i 1))
				s)))
			
	(define-method (code-call (intr <Interpreter>) (objlist LangList))
		(if (not (null? (slot intr 'code)))
			(begin
				(set! (slot intr 'callstack) (cons (list (slot intr 'code) (slot intr 'codepos)) (slot intr 'callstack)))
				(set! (slot intr 'code) objlist)
				(set! (slot intr 'codepos) 0))
			(raise "Call while not running!")))

	(define-method (code-return (intr <Interpreter>))
		(print "CODE-RETURN - CALLSTACK:" (slot intr 'callstack))
		(if (not (null? (slot intr 'callstack)))
			(begin
				;(print "CODE: " (caar (slot intr 'callstack)))
				;(print "POS: " (cadar (slot intr 'callstack)))
				(set! (slot intr 'code) (caar (slot intr 'callstack)))
				(set! (slot intr 'codepos) (cadar (slot intr 'callstack)))
				(set! (slot intr 'callstack) (cdr (slot intr 'callstack))))
			(raise "Return without call!")))

	(define-method (nextObj (intr <Interpreter>))
		(if (>= (slot intr 'codepos) (len (slot intr 'code)))
			(make-LangVoid)
			(begin
				(set! (slot intr 'codepos) (+ (slot intr 'codepos) 1))
				(get (slot intr 'code) (- (slot intr 'codepos) 1)))))

	(define-method (nextObjOrFail (intr <Interpreter>) where)
		(let ((obj (nextObj intr)))
			(if (LangVoid? obj)
				(raise (string-append "Unexpected end of input in " where))
				obj)))

	(define-method (peekObj (intr <Interpreter>))
		(if (>= (slot intr 'codepos) (len (slot intr 'code)))
			(make-LangVoid)
			(get (slot intr 'code) (slot intr 'codepos))))

	(define-method (prevObj (intr <Interpreter>))
		(if (== (slot intr 'codepos) 0)
			(make-LangVoid)
			(begin
				(set! (slot intr 'codepos) (- (slot intr 'codepos) 1))
				(get (slot intr 'code) (slot intr 'codepos)))))

	(define-method (prevObjOrFail (intr <Interpreter>) where)
		(let ((obj (prevObj intr)))
			(if (LangVoid? obj)
				(raise (string-append "Failed to find previous object in " where))
				obj)))
	
	(define-method (havePushedFrames (intr <Interpreter>))
		(not (null? (slot intr 'callstack))))

	(define-method (popTypeOrFail (intr <Interpreter>) test what where)
		(print "POP-TYPE-OR-FAIL, STACK: " (reprStack intr))
		(print "POPPING:" test)
		(let ((obj (pop intr)))
			(if (test obj)
				obj
				(raise (string-append "Expecting " what " in " where " but got: " (fmtStackPrint obj))))))

	; pop integer or fail
	(define-method (popInt (intr <Interpreter>) where)
		(popTypeOrFail intr integer? "integer" where))

	; pop a LangString (as string) or fail
	(define-method (popString (intr <Interpreter>) where)
		(value (popTypeOrFail intr LangString? "string" where)))
		
(define (nextSymbolOrFail intr where)
	(let ((obj (nextObjOrFail intr where)))
		(if (LangSymbol? obj)
			obj
			(raise (string-append "Expecting symbol in " where " but got " (fmtStackPrint obj))))))

;; Reader interface: reader-open-string, reader-next
(define READER_WORDLIST '())

(define (reader-open-string intr str)
	(set! READER_WORDLIST (string-tokenize str))
	(print "READER_WORDLIST: " READER_WORDLIST))

(define (reader-next intr)
	(if READER_WORDLIST
		(begin
			(push intr (car READER_WORDLIST))
			(set! READER_WORDLIST (cdr READER_WORDLIST)))
	
		(push intr (make-LangNull))))

; ( xn .. x1 N -- list of N items )
(define (builtin-make-list intr N)
	(let loop ((nr N) (lst '()))
		(if (> nr 0)
			(loop (- nr 1) (cons (pop intr) lst))
			(push intr (list->LangList lst)))))

; ( obj addr -- store obj @ addr)
(define (builtin-set intr obj addr)
	;(print "IN SET!, STACK:" (reprStack intr))
	(dynvector-set! (slot intr 'OBJMEM) addr obj))

(define (builtin-ref intr addr)
	;(print "IN SET!, STACK:" (reprStack intr))
	(push intr (dynvector-ref (slot intr 'OBJMEM) addr)))

; ( obj -- to local stack )
(define (builtin-to-local intr obj)
	(if (<= (LP intr) (LP_MIN intr))
		(raise "Locals overflow!")
		(begin
			(dec! (LP intr))
			(dynvector-set! (OBJMEM intr) (LP intr) obj))))

; TODO -- some of the above can be lambdas here instead
(define BUILTINS
	(list
		; each as: <function-name> <arg-list-typestr> <function>
		(list "int?" "*" (lambda (intr obj) (push intr (integer? obj))))
		(list "null?" "*" (lambda (intr obj) (push intr (LangNull? obj))))
		(list "reader-open-string" "s" reader-open-string)
		(list "make-list" "i" builtin-make-list)
		(list "set!" "*i" builtin-set)
		(list "SP" "" (lambda (intr) (push intr (slot intr 'SP))))
		(list ">L" "*" builtin-to-local)
		(list "reader-next" "" reader-next)
		(list "ref" "i" builtin-ref)
	))

(set! BUILTINS (alist->hash-table BUILTINS #:test equal?))

; typestr is list of type strings, like in serialize, e.g.: "ifs ..."
; also accepts '*' for "any object"
;
; returns popped arglist
(define (pop-typed-objs intr typestr where)
	(print "POP-TYPED-OBJS: typestr=" typestr)
	(let loop ((argtypes typestr) (args '()))
		(print "LOOP, typestr=" argtypes)
		;(print "CHAR=" (string-take argtypes 1))
		(if (> (string-length argtypes) 0)
			(loop (string-drop-right argtypes 1) (cons 
				(case (string-ref argtypes (- (string-length argtypes) 1))
					((#\i) (popTypeOrFail intr integer? "integer" where))
					((#\f) (popTypeOrFail intr LangFloat? "float" where))
					((#\s) (value (popTypeOrFail intr LangString? "string" where)))
					((#\y) (popTypeOrFail intr string? "symbol" where))
					((#\*) (pop intr))
					(else 
						(raise (string-append "Unknown argtype: " argtypes))))
				args))
			; return arglist
			args)))
					
(define-method (run (intr <Interpreter>) (objlist LangList)) 
	(if (not (null? (slot intr 'code)))
		(raise "Interpreter called recursively!"))
	(set! (slot intr 'code) objlist)
	(set! (slot intr 'codepos) 0)
	(let run-loop ((obj (nextObj intr)))
		(print "RUN OBJ: " (fmtStackPrint obj))
		(cond
			((LangVoid? obj)
				(print "RETURN OR EXIT:")
				; either return or exit
				(if (havePushedFrames intr)
					(begin
						(code-return intr)
						(run-loop (nextObj intr)))
					; else set self not running & exit
					(set! (slot intr 'code) '())
				))
			; literals get pushed
			((or (integer? obj) (LangFloat? obj) (LangString? obj) (LangLambda? obj))
				(push intr obj)
				(run-loop (nextObj intr)))
			; symbols do the most stuff ...
			((LangSymbol? obj) ; obj is a string
				(cond
					((equal? obj "return")
						; as above - return or exit
						(if (havePushedFrames intr)
							(begin
								(code-return intr)
								(run-loop (nextObj intr)))
							; else set self not running & exit
							(set! (slot intr 'code) '())
						))
					; @name -- jump target, ignore
					((equal? (string-take obj 1) "@")
						(run-loop (nextObj intr)))
					; var
					((equal? obj "var")
						(let* ((name (nextSymbolOrFail intr "var"))
								(count (nextObjOrFail intr "var,count")))
							(if (integer? count)
								(begin
									; NOTE different from other ports -- create WORD with name that
									; returns the start address -- doing it this way should allow this
									; code to eventually move to init.verb
									(hash-table-set! (slot intr 'WORDS) name 
											(list->LangList (list (allocate intr count))))
									(run-loop (nextObj intr)))
								(raise (string-append "Expecting int for count but got: " count)))))

					; builtin (native) functions
					((hash-table-exists? BUILTINS obj)
						(let* ((type-fn (hash-table-ref BUILTINS obj))
								(args (pop-typed-objs intr (car type-fn) obj)))

							(print "POP ARGLIST: " (car type-fn))
							(print "ARGS:" args)
							(print "RUN BUILTIN: " obj (cadr type-fn))
							(apply (cadr type-fn) (cons intr args)))
						(run-loop (nextObj intr)))
					; user-defined word
					((hash-table-exists? (slot intr 'WORDS) obj)
						; tail-call elimination
						(let ((next (peekObj intr)))
							(if (or (LangVoid? next) (eqv? next 'return))
								; end of list or return - don't need to come back here
								(if (havePushedFrames intr)
									(code-return))))
						(code-call intr (hash-table-ref (slot intr 'WORDS) obj))
						(run-loop (nextObj intr)))
					(else
						(raise (string-append "Unknown word: " (fmtStackPrint obj))))))
			(else
				(raise (string-append "Unknown word: " (fmtStackPrint obj)))))))

					

(set! intr (make <Interpreter>))
(print "SP NOW: " (SP intr))
(push intr 111)
(push intr 222)
(push intr 333)

(print "=>" (reprStack intr))

(print "POP: " (pop intr))
(print "POP: " (pop intr))

(print "=>" (reprStack intr))

(push intr "AAA")
(push intr "BBB")

(print "=>" (reprStack intr))

(print "POP: " (pop intr))

(print "POP: " (pop intr))
(print "POP: " (pop intr))

(print "=>" (reprStack intr))

(set! L1 (list->LangList '(1 2 3 4)))
(set! L2 (list->LangList '(5 6 7 8)))
(set! L3 (list->LangList '(9 10 11 12)))

(print (fmtStackPrint L1))
(print (fmtStackPrint L2))
(print (fmtStackPrint L3))

(set! (slot intr 'code) L1)
(print (fmtStackPrint (slot intr 'code)))
(code-call intr L2)
(print (fmtStackPrint (slot intr 'code)))
(code-call intr L3)
(print (fmtStackPrint (slot intr 'code)))
(code-return intr)
(print (fmtStackPrint (slot intr 'code)))
(code-return intr)
(print (fmtStackPrint (slot intr 'code)))
(set! (slot intr 'code) '())

(print (hash-table-ref BUILTINS "int?"))

(print "=>" (reprStack intr))
(run intr (list->LangList '(10 20 30)))
(print "=>" (reprStack intr))
(run intr (list->LangList '(40 50 60)))
(print "=>" (reprStack intr))
(run intr (list->LangList '("int?" 123)))
(print "=>" (reprStack intr))

(define (load-byte-compiled-file intr filename)
	(let* ((fileIn (open-input-file filename))
			(result (deserialize-stream intr fileIn)))
		(close-input-port fileIn)
		result))

(define (new-interpreter)
	(let ((intr (make <Interpreter>)))
		(load-byte-compiled-file intr "../lib/init.verb.b")
		(load-byte-compiled-file intr "../lib/compiler.verb.b")
		intr))
		
(define (byte-compile intr text)
	(print "BYTE-COMPILING text: " text)
	(push intr (make LangString 'value text))
	(run intr (hash-table-ref (slot intr 'WORDS) "byte-compile-string"))
	(print "STACK NOW: " (reprStack intr)))

(set! intr (new-interpreter))
(print "WORDS:" (hash-table-keys (slot intr 'WORDS)))
(hash-table-walk (slot intr 'WORDS) (lambda (key val) (print key ": " (fmtStackPrint val))))

(print (list->LangList (list 2048)))

(byte-compile intr "123 456 789")
