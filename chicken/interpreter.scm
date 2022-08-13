;;;==================================================================================
;;; Interpreter
;;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;==================================================================================

;; optimization settings (these are supposed to be global but not sure how they are
;; visible in different compilation units, so I'm including this header in all files
(declare (standard-bindings))
(declare (extended-bindings))

; module header
(declare (unit interpreter))

(module interpreter *
(import scheme)
(import (chicken base))
(import (chicken syntax))

; start of module code

; could probably trim some of these ...
(import srfi-13) ; string library
(import srfi-34) ; exceptions
(import (chicken format)) ; fprintf
(import srfi-1) ; list library
(import dyn-vector)
(import miscmacros) ; inc! dec! while
(import srfi-69) ; hash-tables
(import (chicken gc)) ; memory-statistics

(declare (uses langtypes))
(declare (uses errors))

(import langtypes)
(import errors)

(define STACK_SIZE (expt 2 16))
(define LOCALS_SIZE (expt 2 10))
(define HEAP_STARTSIZE (expt 2 16))

; populated from native.scm
(define BUILTINS (make-hash-table))
; populated from opcodes.scm
(define OPCODE-FUNCTIONS (make-vector 1))

(define-record-type Interpreter	
	(make-empty-Interpreter) ;; don't use this, use make-Interpreter, below
	interpreter?
	(OBJMEM intr-OBJMEM intr-OBJMEM-set!)

	(SP_MIN intr-SP_MIN intr-SP_MIN-set!)
	(SP_EMPTY intr-SP_EMPTY intr-SP_EMPTY-set!)
	(SP intr-SP intr-SP-set!)

	(LP_MIN intr-LP_MIN intr-LP_MIN-set!)
	(LP_EMPTY intr-LP_EMPTY intr-LP_EMPTY-set!)
	(LP intr-LP intr-LP-set!)
	; heap will grow as needed, thanks to dynvector
	(HEAP_NEXTFREE intr-HEAP_NEXTFREE intr-HEAP_NEXTFREE-set!)
	; WORDS[name: string] = List
	(_WORDS intr-WORDS intr-WORDS-set!)

	(code intr-code intr-code-set!) ; currently running code (List)
	(codepos intr-codepos intr-codepos-set!) ; next obj to run as index into code
	(framedata intr-framedata intr-framedata-set!) ; current CallFrameData or '()
	(callstack intr-callstack intr-callstack-set!) ; frame pushed here on call (pushed to head), as (code,codepos)

	; stats
	(callstack-depth callstack-depth callstack-depth-set!)
	(max-callstack max-callstack max-callstack-set!)
	(min-run-SP min-run-SP min-run-SP-set!)
	(min-run-LP min-run-LP min-run-LP-set!)
	(nr-tail-calls nr-tail-calls nr-tail-calls-set!))

(define (make-Interpreter)
	(let ((intr (make-empty-Interpreter)))
		(intr-OBJMEM-set! intr (make-dynvector (+ STACK_SIZE LOCALS_SIZE HEAP_STARTSIZE) 0))
		
		(intr-SP_MIN-set! intr 0)
		(intr-SP_EMPTY-set! intr STACK_SIZE)
		(intr-SP-set! intr (intr-SP_EMPTY intr))

		(intr-LP_MIN-set! intr STACK_SIZE)
		(intr-LP_EMPTY-set! intr (+ STACK_SIZE LOCALS_SIZE))
		(intr-LP-set! intr (intr-LP_EMPTY intr))

		(intr-HEAP_NEXTFREE-set! intr (+ STACK_SIZE LOCALS_SIZE))

		;(intr-WORDS-set! intr (make-hash-table #:test string=?))
		(intr-WORDS-set! intr (make-hash-table))

		(intr-code-set! intr '())
		(intr-codepos-set! intr 0)
		(intr-framedata-set! intr '())
		(intr-callstack-set! intr '())

		; stats
		(callstack-depth-set! intr 0)
		(max-callstack-set! intr 0)
		(min-run-SP-set! intr (intr-SP intr))
		(min-run-LP-set! intr (intr-LP intr))
		(nr-tail-calls-set! intr 0)

		intr))

; shorthand since usually i want the vector
(define (intr-code-list intr) (List-objlist (intr-code intr)))

(define (print-stats intr)
	(print "\n==== Runtime Stats ====")
	(print "* General:")
	(print "  Builtin words: " (length (hash-table-keys BUILTINS)))
	(print "  User-defined words: " (length (hash-table-keys (_WORDS intr))))
	(print "  Max stack depth: " (- (intr-SP_EMPTY intr) (min-run-SP intr)))
	(print "  Max locals depth: " (- (intr-LP_EMPTY intr) (min-run-LP intr)))
	(print "  Max callstack depth: " (max-callstack intr))
	(print "  Tail calls: " (nr-tail-calls intr))

	(let ((mstats (memory-statistics)))
		(print "* Chicken:")
		; really 2x this because of the copying gc, but only count the active half
		(print "  Heap size: " (vector-ref mstats 0))
		(print "  Memory in use: " (vector-ref mstats 1))
		(print "  Nursery (first heap gen) size: " (vector-ref mstats 2)))

	(print "* Notices:")
	(if (not (= (intr-SP intr) (intr-SP_EMPTY intr)))
		(print "  Stack is not empty! (" (- (intr-SP_EMPTY intr) (intr-SP intr)) " items)"))
	(if (not (= (intr-LP intr) (intr-LP_EMPTY intr)))
		(print "  Locals are not empty! (" (- (intr-LP_EMPTY intr) (intr-LP intr)) " items)")))

(define (allocate intr nr)
	(let ((addr (intr-HEAP_NEXTFREE intr)))
		(intr-HEAP_NEXTFREE-set! intr (+ (intr-HEAP_NEXTFREE intr) nr))
		addr))

; get or set memory
(define (memget intr addr)
	(dynvector-ref (intr-OBJMEM intr) addr))

(define (memset intr addr obj)
	(dynvector-set! (intr-OBJMEM intr) addr obj))

(define (push intr obj)
	(if (<= (intr-SP intr) (intr-SP_MIN intr))
		(lang-error 'push "Stack overflow"))
	;(set! (SP intr) (- (SP intr) 1))
	(intr-SP-set! intr (- (intr-SP intr) 1))
	(memset intr (intr-SP intr) obj)

	; stats
	(min-run-SP-set! intr (min (intr-SP intr) (min-run-SP intr))))
	
(define (push-int intr i)
	;(if (or (> i MAX_VINT) (< i MIN_VINT))
	;	(lang-error 'push-int "Integer overflow")
	;	(push intr i)))
	(push intr i))

(define (pop intr)
	(if (>= (intr-SP intr) (intr-SP_EMPTY intr))
		(lang-error 'pop "Stack underflow"))
	;(set! (SP intr) (+ (SP intr) 1))
	(intr-SP-set! intr (+ (intr-SP intr) 1))
	(memget intr (- (intr-SP intr) 1)))

(define (reprStack intr)	
	(let loop ((s "") (i (- (intr-SP_EMPTY intr) 1)))
		(if (>= i (intr-SP intr))
			(loop (string-append s " " (fmtStackPrint (memget intr i)))
				(- i 1))
			s)))

(define (_WORDS intr) (intr-WORDS intr))

(define (code-call intr objlist bound-lambda)		
	(if (not (null? (intr-code intr)))
		(begin
			(intr-callstack-set! intr 
				(cons (list (intr-code intr) (intr-codepos intr) (intr-framedata intr)) 
					(intr-callstack intr)))
			(intr-code-set! intr objlist)
			(intr-codepos-set! intr 0)
			(intr-framedata-set! intr (new-CallFrameData))
			(if (not (null? bound-lambda))
				(CallFrameData-outer-set! (intr-framedata intr) (BoundLambda-outer bound-lambda)))

			; stats
			(callstack-depth-set! intr (+ 1 (callstack-depth intr)))
			(max-callstack-set! intr (max (max-callstack intr) (callstack-depth intr))))
		(lang-error 'code-call "Call while not running!")))

(define (code-return intr)
	;(print "CODE-RETURN - CALLSTACK:" (slot intr 'callstack))
	(if (not (null? (intr-callstack intr)))
		(begin
			;(print "CODE: " (caar (slot intr 'callstack)))
			;(print "POS: " (cadar (slot intr 'callstack)))
			(intr-code-set! intr (caar (intr-callstack intr)))
			(intr-codepos-set! intr (cadar (intr-callstack intr)))
			(intr-framedata-set! intr (caddar (intr-callstack intr)))
			(intr-callstack-set! intr (cdr (intr-callstack intr)))
			; stats
			(callstack-depth-set! intr (- (callstack-depth intr) 1)))
		(lang-error 'code-return "Return without call!")))

(define (nextObj intr)
	(if (>= (intr-codepos intr) (dynvector-length (intr-code-list intr)))
		(make-Void)
		(begin
			(intr-codepos-set! intr (+ (intr-codepos intr) 1))
			(dynvector-ref (intr-code-list intr) (- (intr-codepos intr) 1)))))

(define (nextObjOrFail intr wheresym)
	(let ((obj (nextObj intr)))
		(if (Void? obj)
			(lang-error wheresym "Unexpected end of input")
			obj)))

(define (peekObj intr)
	(if (>= (intr-codepos intr) (dynvector-length (intr-code-list intr)))
		(make-Void)
		(dynvector-ref (intr-code-list intr) (intr-codepos intr))))

(define (prevObj intr)
	(if (= (intr-codepos intr) 0)
		(make-Void)
		(begin
			(intr-codepos-set! intr (- (intr-codepos intr) 1))
			(dynvector-ref (intr-code-list intr) (intr-codepos intr)))))

(define (prevObjOrFail intr wheresym)
	(let ((obj (prevObj intr)))
		(if (Void? obj)
			(lang-error wheresym "Failed to find previous object")
			obj)))
	
(define (havePushedFrames intr)
	(not (null? (intr-callstack intr))))

(define (popTypeOrFail intr test what wheresym)
	;(print "POP-TYPE-OR-FAIL, STACK: " (reprStack intr))
	;(print "POPPING:" test)
	(let ((obj (pop intr)))
		(if (test obj)
			obj
			(lang-error wheresym "Expecting " what " but got: " (fmtStackPrint obj)))))

	; pop integer or fail
	;(define-method (popInt (intr <Interpreter>) wheresym)
	;	(popTypeOrFail intr integer? "integer" wheresym))

	; pop a String (as string) or fail
	;(define-method (popString (intr <Interpreter>) wheresym)
	;	(value (popTypeOrFail intr String? "string" wheresym)))

(define (nextSymbolOrFail intr wheresym)
	(let ((obj (nextObjOrFail intr wheresym)))
		(if (Symbol? obj)
			obj
			(lang-error wheresym "Expecting symbol but got " (fmtStackPrint obj)))))

; typelist is a list of symbols giving argument types. symbols are same as in deserializer,
; i.e. 'i, 'f, 's, or '* meaning "any object"
;
; returns popped arglist
(define (pop-typed-objs intr typelist wheresym)
	;(print "POP-TYPED-OBJS " typelist)
	(let loop ((argtypes typelist) (args '()))
		;(print "LOOP, argtypes=" argtypes " args=" args)
		;(print "CHAR=" (string-take argtypes 1))
		(if (not (null? argtypes))
			(loop (cdr argtypes) (cons 
				(case (car argtypes)
					((i) (popTypeOrFail intr integer? "integer" wheresym))
					((b) (popTypeOrFail intr boolean? "boolean" wheresym))
					((f) (value (popTypeOrFail intr Float? "float" wheresym)))
					((s) (value (popTypeOrFail intr String? "string" wheresym)))
					((y) (popTypeOrFail intr string? "symbol" wheresym))
					((L) (popTypeOrFail intr List? "list" wheresym))
					((*) (pop intr))
					(else 
						(lang-error wheresym "Unknown argtype: " argtypes)))
				args))
			; return arglist
			args)))
					
; is str: '>>NAME'
(define (is-forward-jump? str) (and (>= (string-length str) 2) (string=? (string-take str 2) ">>")))
; is str: '<<NAME'
(define (is-backward-jump? str) (and (>= (string-length str) 2) (string=? (string-take str 2) "<<")))

(define (do-jump intr target)
	(let ((movefn '()))
		(cond 
			((is-forward-jump? target) (set! movefn nextObj))
			((is-backward-jump? target) (set! movefn prevObj))
			(else (lang-error 'do-jump "Not a valid jump target: " target)))
		(let loop ((obj (movefn intr)))
			(cond
				((Void? obj) (lang-error 'do-jump "No such jump: " target))
				((and (Symbol? obj) (string=? (string-drop target 2) (string-drop obj 1)))
					; found it, stop
				)
				(else (loop (movefn intr)))))))

(define (intr-has-word intr name)
	(hash-table-exists? (_WORDS intr) name))
			
(define (intr-define-word intr name objlist allow-overwrite)
	(if (and (intr-has-word intr name) (not allow-overwrite))
		(lang-error 'define-word "Trying to redefine name:" name)
		(hash-table-set! (_WORDS intr) name objlist)))

(define (intr-lookup-word intr name)
	(if (intr-has-word intr name)
		(hash-table-ref (_WORDS intr) name)
		'()))

(define (intr-lookup-word-or-fail intr name)
	(let ((objlist (intr-lookup-word intr name)))
		(if (null? objlist)
			(lang-error 'lookup-word-or-fail "Unknown word: " name)
			objlist)))

(define (intr-delete-word intr name)
	(if (hash-table-exists? (_WORDS intr) name)
		(hash-table-delete! (_WORDS intr) name)
		(lang-error 'delete-word "Trying to delete non-existent name: " name)))
		
(define (intr-run intr objlist)
	(if (not (null? (intr-code intr)))
		(lang-error 'intepreter "Interpreter called recursively!"))
	(intr-code-set! intr objlist)
	(intr-codepos-set! intr 0)
	(intr-framedata-set! intr '())
	(let ((exit-loop #f))
		(while (not exit-loop)
			(let ((obj (nextObj intr)))
				;(print "RUN OBJ: " (fmtStackPrint obj))
				(cond
					; symbols are the most common, so check for them first
					((Symbol? obj) ; obj is a string
						(cond
							((char=? (string-ref obj 0) #\')
								; remove one level of quoting and push
								(push intr (string-drop obj 1)))
							((string=? obj "return")
								; as above - return or exit
								(if (havePushedFrames intr)
									(code-return intr)
									; else set self not running & exit
									(begin
										(intr-code-set! intr '())
										(set! exit-loop #t))))
							; if
							((string=? obj "if")
								(let ((target (nextSymbolOrFail intr "if"))
										(bval (popTypeOrFail intr boolean? "true|false" 'if)))
									; this only repositions the reader
									(if bval (do-jump intr target))))
									; else - keep running with next object
									
							; call
							((string=? obj "call")
								; pop lambda or list and call
								(let ((L (pop intr)))
									(cond
										((Lambda? L)
											; TODO - tail call elimination??
											(code-call intr (Lambda-llist L) '()))
										((BoundLambda? L)
											(code-call intr (BoundLambda-llist L) L))
										(else
											(lang-error 'call "Expecting lambda or bound-lambda but got:" L)))))

							; builtin (native) functions
							((hash-table-exists? BUILTINS obj)
								;(print "CALL BUILTIN: " obj)
								(let* ((type-fn (hash-table-ref BUILTINS obj))
										(args (pop-typed-objs intr (car type-fn) (string->symbol obj))))

									;(print "POP ARGLIST: " (car type-fn))
									;(print "ARGS:" args)
									;(print "RUN BUILTIN: " obj (cadr type-fn))
									(apply (cadr type-fn) (cons intr args))))

							; user-defined word
							((hash-table-exists? (_WORDS intr) obj)
								;(print "CALL USERWORD: " obj)
								; tail-call elimination
								(let ((next (peekObj intr)))
									(if (or (Void? next) 
											(and (Symbol? next) (string=? next "return")))
										; end of list or return - don't need to come back here
										(if (havePushedFrames intr)
											(begin
												(code-return intr)
												(nr-tail-calls-set! intr (+ 1 (nr-tail-calls intr)))))))
								(code-call intr (hash-table-ref (_WORDS intr) obj) '()))
							
							; <<NAME and >>NAME
							((or (is-forward-jump? obj) (is-backward-jump? obj))
								(do-jump intr obj))

							; @name -- jump target, ignore
							((char=? (string-ref obj 0) #\@))
							
							(else
								(lang-error 'intepreter "Unknown word" obj))))

					((Opcode? obj)
						;(print "OPCODE:" (fmtStackPrint obj))
						((vector-ref OPCODE-FUNCTIONS (Opcode-code obj))
							intr (intr-framedata intr) (Opcode-A obj) (Opcode-B obj) (Opcode-C obj)))

					((Void? obj)
						;(print "RETURN OR EXIT:")
						; either return or exit
						(if (havePushedFrames intr)
							(code-return intr)
							; else set self not running & exit
							(begin
								(intr-code-set! intr '())
								(set! exit-loop #t))))

					; list literals are deepcopied (see DESIGN-NOTES.txt)
					((List? obj)
						(push intr (deepcopy obj)))

					; see c++ notes on pushing everything else
					(else 
						(push intr obj)))))))

) ; end of module