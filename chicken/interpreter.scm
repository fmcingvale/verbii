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
(import miscmacros) ; inc! dec! 
(import srfi-69) ; hash-tables

(import langtypes)
(import errors)

(define STACK_SIZE (expt 2 10))
(define LOCALS_SIZE (expt 2 10))
(define HEAP_STARTSIZE (expt 2 16))

; populated from native.scm
(define BUILTINS (make-hash-table))

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
	; WORDS[name: string] = LangList
	(WORDS intr-WORDS intr-WORDS-set!)

	(code intr-code intr-code-set!) ; currently running code (LangList)
	(codepos intr-codepos intr-codepos-set!) ; next obj to run as index into code
	(callstack intr-callstack intr-callstack-set!)) ; frame pushed here on call (pushed to head), as (code,codepos)

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

		(intr-WORDS-set! intr (make-hash-table #:test string=?))

		(intr-code-set! intr '())
		(intr-codepos-set! intr 0)
		(intr-callstack-set! intr '())
		intr))

; shorthand since usually i want the vector
(define (intr-code-list intr) (LangList-objlist (intr-code intr)))

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
	(memset intr (intr-SP intr) obj))
	
(define (push-int intr i)
	(if (or (> i MAX_INT_31) (< i MIN_INT_31))
		(lang-error 'push-int "Integer overflow")
		(push intr i)))

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

(define (WORDS intr) (intr-WORDS intr))

(define (code-call intr objlist)		
	(if (not (null? (intr-code intr)))
		(begin
			(intr-callstack-set! intr (cons (list (intr-code intr) (intr-codepos intr)) (intr-callstack intr)))
			(intr-code-set! intr objlist)
			(intr-codepos-set! intr 0))
		(lang-error 'code-call "Call while not running!")))

(define (code-return intr)
	;(print "CODE-RETURN - CALLSTACK:" (slot intr 'callstack))
	(if (not (null? (intr-callstack intr)))
		(begin
			;(print "CODE: " (caar (slot intr 'callstack)))
			;(print "POS: " (cadar (slot intr 'callstack)))
			(intr-code-set! intr (caar (intr-callstack intr)))
			(intr-codepos-set! intr (cadar (intr-callstack intr)))
			(intr-callstack-set! intr (cdr (intr-callstack intr))))
		(lang-error 'code-return "Return without call!")))

(define (nextObj intr)
	(if (>= (intr-codepos intr) (dynvector-length (intr-code-list intr)))
		(make-LangVoid)
		(begin
			(intr-codepos-set! intr (+ (intr-codepos intr) 1))
			(dynvector-ref (intr-code-list intr) (- (intr-codepos intr) 1)))))

(define (nextObjOrFail intr wheresym)
	(let ((obj (nextObj intr)))
		(if (LangVoid? obj)
			(lang-error wheresym "Unexpected end of input")
			obj)))

(define (peekObj intr)
	(if (>= (intr-codepos intr) (dynvector-length (intr-code-list intr)))
		(make-LangVoid)
		(dynvector-ref (intr-code-list intr) (intr-codepos intr))))

(define (prevObj intr)
	(if (= (intr-codepos intr) 0)
		(make-LangVoid)
		(begin
			(intr-codepos-set! intr (- (intr-codepos intr) 1))
			(dynvector-ref (intr-code-list intr) (intr-codepos intr)))))

(define (prevObjOrFail intr wheresym)
	(let ((obj (prevObj intr)))
		(if (LangVoid? obj)
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
(define (is-forward-jump? str) (and (>= (string-length str) 2) (string=? (string-take str 2) ">>")))
; is str: '<<NAME'
(define (is-backward-jump? str) (and (>= (string-length str) 2) (string=? (string-take str 2) "<<")))

(define (do-jump intr target)
	(let ((movefn '()))
		(cond 
			((is-forward-jump? target) (set! movefn nextObjOrFail))
			((is-backward-jump? target) (set! movefn prevObjOrFail))
			(else (lang-error 'do-jump "Not a valid jump target: " target)))
		(let loop ((obj (movefn intr 'do-jump)))
			(cond
				((LangVoid? obj) (lang-error 'do-jump "End of input looking for: " target))
				((and (LangSymbol? obj) (string=? (string-drop target 2) (string-drop obj 1)))
					; found it, stop
				)
				(else (loop (movefn intr 'do-jump)))))))

(define (intr-has-word intr name)
	(hash-table-exists? (WORDS intr) name))
			
(define (intr-delete-word intr name)
	(if (hash-table-exists? (WORDS intr) name)
		(hash-table-delete! (WORDS intr) name)
		(lang-error 'delete-word "Trying to delete non-existent name: " name)))
		
(define (intr-run intr objlist)
	(if (not (null? (intr-code intr)))
		(lang-error 'intepreter "Interpreter called recursively!"))
	(intr-code-set! intr objlist)
	(intr-codepos-set! intr 0)
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
					(intr-code-set! intr '())
				))
			; literals get pushed
			((integer? obj) (push-int intr obj) (run-loop (nextObj intr)))
			((or (LangFloat? obj) (LangString? obj) (LangLambda? obj))
				(push intr obj)
				(run-loop (nextObj intr)))
			; symbols do the most stuff ...
			((LangSymbol? obj) ; obj is a string
				(cond
					((char=? (string-ref obj 0) #\')
						; remove one level of quoting and push
						(push intr (string-drop obj 1))
						(run-loop (nextObj intr)))
					((string=? obj "return")
						; as above - return or exit
						(if (havePushedFrames intr)
							(begin
								(code-return intr)
								(run-loop (nextObj intr)))
							; else set self not running & exit
							(intr-code-set! intr '())
						))
					; if
					((string=? obj "if")
						(let ((target (nextSymbolOrFail intr "if"))
								(bval (popTypeOrFail intr boolean? "true|false" 'if)))
							; this only repositions the reader
							(if bval (do-jump intr target))
							; else - keep running with next object
							(run-loop (nextObj intr))))
					; var
					; TODO -- this should pop count from stack instead of being syntax "var count"
					((string=? obj "var")
						(let* ((name (nextSymbolOrFail intr "var"))
								(count (nextObjOrFail intr 'var)))
							(if not (integer? count)
								(lang-error 'var "Expecting int for count but got: " count))
							(if (intr-has-word intr name)
								(lang-error 'var "Trying to redefine name: " name))

							; NOTE different from other ports -- create WORD with name that
							; returns the start address -- doing it this way should allow this
							; code to eventually move to init.verb
							(hash-table-set! (WORDS intr) name 
									(list->LangList (list (allocate intr count))))
							(run-loop (nextObj intr))))
					; del
					; TODO -- this should pop symbol from stack instead of being syntax "del NAME"
					((string=? obj "del")
						(let ((name (nextSymbolOrFail intr "del")))
							(intr-delete-word intr name)
							(run-loop (nextObj intr))))
					; call
					((string=? obj "call")
						; pop lambda or list and call
						(let ((L (pop intr)))
							(cond
								((LangLambda? L)
									; TODO - tail call elimination??
									(code-call intr (lambda-llist L))
									(run-loop (nextObj intr)))
								((LangList? L)
									(code-call intr L)
									(run-loop (nextObj intr)))
								(else
									(lang-error 'call "Expecting lambda or list but got:" L)))))

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
					
					; <<NAME and >>NAME
					((or (is-forward-jump? obj) (is-backward-jump? obj))
						(do-jump intr obj)
						(run-loop (nextObj intr)))

					; @name -- jump target, ignore
					((char=? (string-ref obj 0) #\@)
						(run-loop (nextObj intr)))
					
					(else
						(lang-error 'intepreter "Unknown word" obj))))
			(else
				(lang-error 'interpreter "Unknown word" obj)))))

) ; end of module