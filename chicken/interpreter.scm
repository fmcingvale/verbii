;;;==================================================================================
;;; Interpreter
;;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;==================================================================================

; module header
(module interpreter *
(import scheme)
(import (chicken base))
(import (chicken syntax))

; start of module code

; could probably trim some of these ...
(import coops-primitive-objects)
(import srfi-13) ; string library
(import srfi-34) ; exceptions
(import (chicken format)) ; fprintf
(import srfi-1) ; list library
(import coops)
(import dyn-vector)
(import miscmacros) ; inc! dec! 
(import srfi-69) ; hash-tables
; shorthand
(define slot slot-value)

(import langtypes)

(define STACK_SIZE (expt 2 10))
(define LOCALS_SIZE (expt 2 10))
(define HEAP_STARTSIZE (expt 2 16))

; populated from native.scm
(define BUILTINS (make-hash-table))

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
		(if (equal? (slot intr 'codepos) 0)
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
	(let ((movefn '()))
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

) ; end of module