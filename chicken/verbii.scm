;;;==================================================================================
;;; verbii in chicken scheme
;;;
;;; all one long file for now since i haven't figured out chicken modules ...
;;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;==================================================================================

;; this is my first try using coops so ... may be some suboptimal stuff here ...

(import coops-primitive-objects)
(import srfi-13) ; string library
(import srfi-34) ; exceptions
(import (chicken format)) ; fprintf
(import srfi-1) ; list library
(import coops)
(import dyn-vector)
(import miscmacros) ; inc! dec! 
(import srfi-69) ; hash-tables
(import (chicken io))

; shorthand
(define slot slot-value)

; must comment these out when building standalone executable.
; uncomment them when running under interpreter.
; ... there has to be a better way ...
(load "langtypes.scm")
(load "deserializer.scm")

(import langtypes)
(import deserializer)

(define (test-printing)
	(print "--- STACK FORMAT: ---")
	(print (fmtStackPrint 123))
	(print (fmtStackPrint (make LangFloat 'value (/ 1.0 3.0))))
	(print (fmtStackPrint #t))
	(print (fmtStackPrint #f))
	(print (fmtStackPrint "abc"))
	(print (fmtStackPrint (make LangNull)))
	(print (fmtStackPrint (make LangString 'value "abc")))
	(let ((lst (make LangList)))
		(push-back lst 111)
		(push-back lst 222)
		(push-back lst (make LangString 'value "hello world"))
		(push-back lst "a-long-symbol-here")
		(print "LIST: " (fmtStackPrint lst)))

	(print "--- DISPLAY FORMAT: ---")
	(print (fmtDisplay 123))
	(print (fmtDisplay (make LangFloat 'value (/ 1.0 3.0))))
	(print (fmtDisplay #t))
	(print (fmtDisplay #f))
	(print (fmtDisplay "abc"))
	(print (fmtDisplay (make LangNull)))
	(print (fmtDisplay (make LangString 'value "abc")))
	(let ((lst (make LangList)))
		(push-back lst 111)
		(push-back lst 222)
		(push-back lst (make LangString 'value "hello world"))
		(push-back lst "a-long-symbol-here")
		(print "LIST: " (fmtDisplay lst))))

(test-printing)


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

		; heap will grow as needed, thanks to dynvector
		(HEAP_NEXTFREE accessor: HEAP_NEXTFREE initform: (+ STACK_SIZE LOCALS_SIZE))

		; WORDS[name: string] = LangList
		(WORDS accessor: WORDS initform: (make-hash-table #:test equal?))

		(code '()) ; currently running code (LangList)
		(codepos -1) ; next obj to run as index into code
		(callstack '()) ; frame pushed here on call (pushed to head), as (code,codepos)
	)
)

	(define-method (allocate (intr <Interpreter>) nr)
		(let ((addr (HEAP_NEXTFREE intr)))
			(set! (HEAP_NEXTFREE intr) (+ (HEAP_NEXTFREE intr) nr))
			addr))

	; get or set memory
	(define-method (memget (intr <Interpreter>) (addr <integer>))
		(dynvector-ref (OBJMEM intr) addr))

	(define-method (memset (intr <Interpreter>) (addr <integer>) obj)
		(dynvector-set! (OBJMEM intr) addr obj))

	(define-method (push (intr <Interpreter>) obj)
		(if (<= (SP intr) (SP_MIN intr))
			(raise "Stack overflow!"))
		;(set! (SP intr) (- (SP intr) 1))
		(dec! (SP intr))
		(memset intr (SP intr) obj))
		
	(define-method (pop (intr <Interpreter>))
		(if (>= (SP intr) (SP_EMPTY intr))
			(raise "Stack underflow!"))
		;(set! (SP intr) (+ (SP intr) 1))
		(inc! (SP intr))
		(memget intr (- (SP intr) 1)))
		
	(define-method (reprStack (intr <Interpreter>))
		(let loop ((s "") (i (- (SP_EMPTY intr) 1)))
			(if (>= i (SP intr))
				(loop (string-append s " " (fmtStackPrint (memget intr i)))
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
			(make LangVoid)
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
			(make LangVoid)
			(get (slot intr 'code) (slot intr 'codepos))))

	(define-method (prevObj (intr <Interpreter>))
		(if (== (slot intr 'codepos) 0)
			(make LangVoid)
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
	
		(push intr (make LangNull))))

; ( xn .. x1 N -- list of N items )
(define (builtin-make-list intr N)
	(let loop ((nr N) (lst '()))
		(if (> nr 0)
			(loop (- nr 1) (cons (pop intr) lst))
			(push intr (list->LangList lst)))))

; ( obj addr -- store obj @ addr)
(define (builtin-set intr obj addr)
	;(print "IN SET!, STACK:" (reprStack intr))
	(memset intr addr obj))
	
(define (builtin-ref intr addr)
	;(print "IN SET!, STACK:" (reprStack intr))
	(push intr (memget intr addr)))

; ( obj -- to local stack )
(define (builtin-to-local intr obj)
	(if (<= (LP intr) (LP_MIN intr))
		(raise "Locals overflow!"))
	(dec! (LP intr))
	(memset intr (LP intr) obj))
			
; ( -- obj from local stack )
(define (builtin-from-local intr)
	(if (>= (LP intr) (LP_EMPTY intr))
		(lang-error "Locals underflow!"))
	(push intr (memget intr (LP intr)))
	(inc! (LP intr)))
		
(define (builtin-equal intr A B)
	(cond
		((LangNull? A) (push intr (LangNull? B)))
		((integer? A)
			(cond
				((integer? B) (push intr (equal? A B)))
				((LangFloat? B) (push intr (equal? (exact->inexact A) (value B))))
				(else (push intr #f))))
		((LangFloat? A)
			(cond
				((integer? B) (push intr (equal? (value A) (exact->inexact B))))
				((LangFloat? B) (push intr (equal? (value A) (value B))))
				(else (push intr #f))))
		((LangString? A)
			(push intr (and (LangString? B) (equal? (value A) (value B)))))
		((string? A)
			(push intr (and (string? B) (equal? A B))))
		((boolean? A)
			(push intr (and (boolean? B) (equal? A B))))
		(else (lang-error "Don't know how to compare " A " and " B))))

(define (builtin-add intr A B)
	(cond
		((integer? A)
			(cond
				((integer? B) (push intr (+ A B)))
				((LangFloat? B) (push intr (make LangFloat 'value (+ A (value B)))))
				(else (lang-error "Don't know how to add " A " and " B))))
		((LangFloat? A)
			(cond
				((integer? B) (push intr (make LangFloat 'value (+ (value A) B))))
				((LangFloat? B) (push intr (make LangFloat 'value (+ (value A) (value B)))))
				(else (lang-error "Don't know how to add " A " and " B))))
		((LangString? A)
			(if (LangString? B)
				(make LangString 'value (string-append (value A) (value B)))
				(else (lang-error "Don't know how to add " A " and " B))))
		((string? A)
			(if (string? B)
				(string-append A B)
				(else (lang-error "Don't know how to add " A " and " B))))
		((LangList? A)
			(if (LangList? B)
				(push intr (make LangList 'objlist 
					(list->dynvector (append (dynvector->list (objlist A)) (dynvector->list (objlist B))))))
				(else (lang-error "Don't know how to add " A " and " B))))
		(else (lang-error "Don't know how to add " A " and " B))))

(define (is-sequence? obj)
	(or (LangString? obj) (LangSymbol? obj) (LangList? obj)))

(define (builtin-length intr obj)
	(if (is-sequence? obj)
		(push intr (len obj))
		(lang-error "Object does not support 'length': " obj)))

(define (builtin-slice intr obj index nr)
	; ported from c#
	(if (not (is-sequence? obj))
		(lang-error "Object does not support slicing: " obj))

	(let ((objsize (len obj)))
		; adjust index & nr for negative & out of bounds conditions
		(if (< index 0) (set! index (+ objsize index))) ; count from end
		(if (or (< index 0) (>= index objsize)) ; out of bounds - return empty object
			(cond
				((LangString? obj) (push intr (make LangString 'value "")))
				((LangSymbol? obj) (push intr "")) ; another reason not to use builtin symbols!
				((LangList? obj) (push intr (make LangList))))
			; else, i can make a valid slice - adjust nr as needed
			(begin
				; nr < 0 means "copy all, starting at index"
				(if (< nr 0) (set! nr (- objsize index)))
				; past end of object, truncate
				(if (> (+ index nr) objsize) (set! nr (- objsize index)))
				; make slice
				(cond
					((LangString? obj) 
						(push intr (make LangString 'value (string-copy (value obj) index (+ index nr)))))
					((LangSymbol? obj)
						(push intr (string-copy obj index (+ index nr))))
					((LangList? obj)
						(let ((newlist (make LangList)))
							(let loop ((i index) (n nr))
								(if (> n 0)
									(push-back newlist (get obj i))
									(loop (+ i 1) (- n 1))))
							(push intr newlist))))))))

(define (builtin-unmake intr obj)
	(cond
		((or (LangString? obj) (LangSymbol? obj))
			(string-for-each (lambda (c) (push intr (char->integer c))) (value obj))
			(push intr (len obj)))
		((LangList? obj)
			(dynvector-for-each (lambda (i o) (push intr o)) (objlist obj))
			(push intr (len obj)))
		((LangLambda? obj)
			; like other ports, push a copy
			(push intr (make LangLambda 'objlist (dynvector-copy (objlist obj)))))
		(else (lang-error "Don't know how to unmake object: " obj))))

; pop N objects and return as list in stack order
(define (pop-nr-objs N)
	(let loop ((nr N) (lst '()))
		(if (> nr 0)
			(loop (- nr 1) (cons (pop intr) lst))
			lst)))

(define (builtin-make-string intr NR)
	; pop list of integers into lst and make string
	(push intr (make LangString 'value 
		(apply string (map integer->char (pop-nr-objs NR))))))

; TODO -- some of the above can be lambdas here instead
(define BUILTINS
	(list
		; each as: <function-name> <arg-list-typestr> <function>
		(list "int?" "*" (lambda (intr obj) (push intr (integer? obj))))
		(list "null?" "*" (lambda (intr obj) (push intr (LangNull? obj))))
		(list "bool?" "*" (lambda (intr obj) (push intr (boolean? obj))))
		(list "list?" "*" (lambda (intr obj) (push intr (LangList? obj))))
		(list "string?" "*" (lambda (intr obj) (push intr (LangString? obj))))
		(list "symbol?" "*" (lambda (intr obj) (push intr (string? obj))))
		(list "reader-open-string" "s" reader-open-string)
		(list "make-list" "i" builtin-make-list)
		(list "set!" "*i" builtin-set)
		(list "SP" "" (lambda (intr) (push intr (SP intr))))
		(list "SP!" "i" (lambda (intr addr) (set! (SP intr) addr)))
		(list "LP" "" (lambda (intr) (push intr (LP intr))))
		(list ">L" "*" builtin-to-local)
		(list "L>" "" builtin-from-local)
		(list "reader-next" "" reader-next)
		(list "ref" "i" builtin-ref)
		(list "==" "**" builtin-equal)
		(list "+" "**" builtin-add)
		(list "slice" "*ii" builtin-slice)
		(list "unmake" "*" builtin-unmake)
		(list "make-string" "i" builtin-make-string)
		(list "length" "*" builtin-length)
	))

(set! BUILTINS (alist->hash-table BUILTINS #:test equal?))

; typestr is list of type strings, like in serialize, e.g.: "i", "f", "s", etc,
; concatenated into a string. use "*" for "any object"
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
					((#\f) (value (popTypeOrFail intr LangFloat? "float" where)))
					((#\s) (value (popTypeOrFail intr LangString? "string" where)))
					((#\y) (popTypeOrFail intr string? "symbol" where))
					((#\*) (pop intr))
					(else 
						(raise (string-append "Unknown argtype: " argtypes))))
				args))
			; return arglist
			args)))
					
; is str: '>>...'
(define (is-forward-jump? str) (equal? (string-take str 2) ">>"))
(define (is-backward-jump? str) (equal? (string-take str 2) "<<"))

(define lang-error (lambda args
	(raise (string-append (map fmtDisplay args)))))

(define-method (do-jump (intr <Interpreter>) target)
	(let ((movedir '()))
		(cond 
			((is-forward-jump? target) (set! movefn nextObjOrFail))
			((is-backward-jump? target) (set! movefn prevObjOrFail))
			(else (lang-error "Not a valid jump target: " target)))
		(let loop ((obj (movefn intr "do-jump")))
			(cond
				((LangVoid? obj) (lang-error "End of input looking for: " target))
				((and (LangSymbol? obj) (equal? (string-drop target 2) (string-drop obj 1)))
					; found it, stop
				)
				(else (loop (movefn intr "do-jump")))))))
				
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
					((equal? (string-ref obj 0) #\')
						; remove one level of quoting and push
						(push intr (string-drop obj 1))
						(run-loop (nextObj intr)))
					((equal? obj "return")
						; as above - return or exit
						(if (havePushedFrames intr)
							(begin
								(code-return intr)
								(run-loop (nextObj intr)))
							; else set self not running & exit
							(set! (slot intr 'code) '())
						))
					; if
					((equal? obj "if")
						(let ((target (nextSymbolOrFail intr "if"))
								(bval (popTypeOrFail intr boolean? "true|false" "if")))
							; this only repositions the reader
							(if bval (do-jump intr target))
							; else - keep running with next object
							(run-loop (nextObj intr))))
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
									(hash-table-set! (WORDS intr) name 
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
					((hash-table-exists? (WORDS intr) obj)
						; tail-call elimination
						(let ((next (peekObj intr)))
							(if (or (LangVoid? next) (eqv? next 'return))
								; end of list or return - don't need to come back here
								(if (havePushedFrames intr)
									(code-return intr))))
						(code-call intr (hash-table-ref (WORDS intr) obj))
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

; load a precompiled (.b) file into interpreter
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
	(run intr (hash-table-ref (WORDS intr) "byte-compile-string"))
	(print "STACK NOW: " (reprStack intr)))

(set! intr (new-interpreter))
(print "WORDS:" (hash-table-keys (WORDS intr)))
(hash-table-walk (WORDS intr) (lambda (key val) (print key ": " (fmtStackPrint val))))

(print (list->LangList (list 2048)))

(byte-compile intr "123 456 789")
