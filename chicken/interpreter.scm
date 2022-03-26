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
(import errors)

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
			(lang-error 'push "Stack overflow!"))
		;(set! (SP intr) (- (SP intr) 1))
		(dec! (SP intr))
		(memset intr (SP intr) obj))
		
	(define-method (pop (intr <Interpreter>))
		(if (>= (SP intr) (SP_EMPTY intr))
			(lang-error 'pop "Stack underflow!"))
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
			(lang-error 'code-call "Call while not running!")))

	(define-method (code-return (intr <Interpreter>))
		;(print "CODE-RETURN - CALLSTACK:" (slot intr 'callstack))
		(if (not (null? (slot intr 'callstack)))
			(begin
				;(print "CODE: " (caar (slot intr 'callstack)))
				;(print "POS: " (cadar (slot intr 'callstack)))
				(set! (slot intr 'code) (caar (slot intr 'callstack)))
				(set! (slot intr 'codepos) (cadar (slot intr 'callstack)))
				(set! (slot intr 'callstack) (cdr (slot intr 'callstack))))
			(lang-error 'code-return "Return without call!")))

	(define-method (nextObj (intr <Interpreter>))
		(if (>= (slot intr 'codepos) (len (slot intr 'code)))
			(make LangVoid)
			(begin
				(set! (slot intr 'codepos) (+ (slot intr 'codepos) 1))
				(get (slot intr 'code) (- (slot intr 'codepos) 1)))))

	(define-method (nextObjOrFail (intr <Interpreter>) wheresym)
		(let ((obj (nextObj intr)))
			(if (LangVoid? obj)
				(lang-error wheresym "Unexpected end of input")
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

	(define-method (prevObjOrFail (intr <Interpreter>) wheresym)
		(let ((obj (prevObj intr)))
			(if (LangVoid? obj)
				(lang-error wheresym "Failed to find previous object")
				obj)))
	
	(define-method (havePushedFrames (intr <Interpreter>))
		(not (null? (slot intr 'callstack))))

	(define-method (popTypeOrFail (intr <Interpreter>) test what wheresym)
		;(print "POP-TYPE-OR-FAIL, STACK: " (reprStack intr))
		;(print "POPPING:" test)
		(let ((obj (pop intr)))
			(if (test obj)
				obj
				(lang-error wheresym "Expecting " what " but got: " (fmtStackPrint obj)))))

	; pop integer or fail
	;(define-method (popInt (intr <Interpreter>) wheresym)
	;	(popTypeOrFail intr integer? "integer" wheresym))

	; pop a LangString (as string) or fail
	;(define-method (popString (intr <Interpreter>) wheresym)
	;	(value (popTypeOrFail intr LangString? "string" wheresym)))

(define (nextSymbolOrFail intr wheresym)
	(let ((obj (nextObjOrFail intr wheresym)))
		(if (LangSymbol? obj)
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
					((f) (value (popTypeOrFail intr LangFloat? "float" wheresym)))
					((s) (value (popTypeOrFail intr LangString? "string" wheresym)))
					((y) (popTypeOrFail intr string? "symbol" wheresym))
					((L) (popTypeOrFail intr LangList? "list" wheresym))
					((*) (pop intr))
					(else 
						(lang-error wheresym "Unknown argtype: " argtypes)))
				args))
			; return arglist
			args)))
					
; is str: '>>NAME'
(define (is-forward-jump? str) (and (>= (len str) 2) (equal? (string-take str 2) ">>")))
; is str: '<<NAME'
(define (is-backward-jump? str) (and (>= (len str) 2) (equal? (string-take str 2) "<<")))

(define-method (do-jump (intr <Interpreter>) target)
	(let ((movefn '()))
		(cond 
			((is-forward-jump? target) (set! movefn nextObjOrFail))
			((is-backward-jump? target) (set! movefn prevObjOrFail))
			(else (lang-error 'do-jump "Not a valid jump target: " target)))
		(let loop ((obj (movefn intr 'do-jump)))
			(cond
				((LangVoid? obj) (lang-error 'do-jump "End of input looking for: " target))
				((and (LangSymbol? obj) (equal? (string-drop target 2) (string-drop obj 1)))
					; found it, stop
				)
				(else (loop (movefn intr 'do-jump)))))))
				
(define-method (run (intr <Interpreter>) (objlist LangList)) 
	(if (not (null? (slot intr 'code)))
		(lang-error 'intepreter "Interpreter called recursively!"))
	(set! (slot intr 'code) objlist)
	(set! (slot intr 'codepos) 0)
	(let run-loop ((obj (nextObj intr)))
		;(print "RUN OBJ: " (fmtStackPrint obj))
		(cond
			((LangVoid? obj)
				;(print "RETURN OR EXIT:")
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
								(bval (popTypeOrFail intr boolean? "true|false" 'if)))
							; this only repositions the reader
							(if bval (do-jump intr target))
							; else - keep running with next object
							(run-loop (nextObj intr))))
					; <<NAME and >>NAME
					((or (is-forward-jump? obj) (is-backward-jump? obj))
						(do-jump intr obj)
						(run-loop (nextObj intr)))

					; @name -- jump target, ignore
					((equal? (string-take obj 1) "@")
						(run-loop (nextObj intr)))
					; var
					; TODO -- this should pop count from stack instead of being syntax "var count"
					((equal? obj "var")
						(let* ((name (nextSymbolOrFail intr "var"))
								(count (nextObjOrFail intr 'var)))
							(if (integer? count)
								(begin
									; NOTE different from other ports -- create WORD with name that
									; returns the start address -- doing it this way should allow this
									; code to eventually move to init.verb
									(hash-table-set! (WORDS intr) name 
											(list->LangList (list (allocate intr count))))
									(run-loop (nextObj intr)))
								(lang-error 'var "Expecting int for count but got: " count))))
					; del
					; TODO -- this should pop symbol from stack instead of being syntax "del NAME"
					((equal? obj "del")
						(let ((name (nextSymbolOrFail intr "del")))
							(hash-table-delete! (WORDS intr) name)
							(run-loop (nextObj intr))))
					; call
					((equal? obj "call")
						; pop lambda and call
						(let ((L (popTypeOrFail intr LangLambda? "lambda" 'call)))
							; TODO - tail call elimination??
							(code-call intr (slot L 'llist))
							(run-loop (nextObj intr))))

					; builtin (native) functions
					((hash-table-exists? BUILTINS obj)
						;(print "CALL BUILTIN: " obj)
						(let* ((type-fn (hash-table-ref BUILTINS obj))
								(args (pop-typed-objs intr (car type-fn) (string->symbol obj))))

							;(print "POP ARGLIST: " (car type-fn))
							;(print "ARGS:" args)
							;(print "RUN BUILTIN: " obj (cadr type-fn))
							(apply (cadr type-fn) (cons intr args)))
						(run-loop (nextObj intr)))
					; user-defined word
					((hash-table-exists? (WORDS intr) obj)
						;(print "CALL USERWORD: " obj)
						; tail-call elimination
						(let ((next (peekObj intr)))
							(if (or (LangVoid? next) (eqv? next 'return))
								; end of list or return - don't need to come back here
								(if (havePushedFrames intr)
									(code-return intr))))
						(code-call intr (hash-table-ref (WORDS intr) obj))
						(run-loop (nextObj intr)))
					(else
						(lang-error 'intepreter "Unknown word:" obj))))
			(else
				(lang-error 'interpreter "Unknown word:" obj)))))

) ; end of module