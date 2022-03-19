;;;==================================================================================
;;; verbii in chicken scheme
;;;
;;; all one long file for now since i haven't figured out chicken modules ...
;;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;==================================================================================

;; this is my first try using coops and even records, so ... may be some suboptimal stuff here ...

;(import coops)
;(import coops-primitive-objects)
(import srfi-13) ; string library
(import srfi-34) ; exceptions
(import (chicken format)) ; fprintf
(import srfi-1) ; list library
(import coops)
(import dyn-vector)

;;;==================================================================================
;;; langtypes.scm
;;;==================================================================================

; need a null type that is distinct from '() so that '() can be the empty list
; ---- ugh ... that's obsolete since I have LangList, but still LangNull is preferred to '()
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

; verbii lists are really arrays, so use a vector
(define-class LangList () 
	(
		(objlist accessor: objlist initform: (make-dynvector 0 0))
	))

(define-method (get (llist LangList) index)
	(if (or (< index 0) (>= index (dynvector-length (objlist llist))))
		(raise "Out of bounds in LangList"))
	(dynvector-ref (objlist llist) index))

(define-method (push-back (llist LangList) obj)
	(dynvector-set! (objlist llist) (dynvector-length (objlist llist)) obj))

(define-method (len (llist LangList))
	(dynvector-length (objlist llist)))

(define (LangList? obj) (subclass? (class-of obj) LangList))

;(define (LangNull? obj) (subclass? (class-of obj) LangNull))

(define (fmtStackPrint obj)
	(cond
		((integer? obj) (number->string obj))
		((LangFloat? obj) (string-append "#" (number->string (LangFloat-value obj))))
		((boolean? obj) (if obj "true" "false"))
		((symbol? obj) (symbol->string obj))
		((string? obj) (string-append "\"" obj "\""))
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
		((symbol? obj) (string-append "'" (symbol->string obj)))
		((string? obj) obj)
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
	(print (fmtStackPrint 'abc))
	(print (fmtStackPrint (make-LangNull)))
	(print (fmtStackPrint "abc"))
	(let ((lst (make LangList)))
		(push-back lst 111)
		(push-back lst 222)
		(push-back lst "hello world")
		(push-back lst 'a-long-symbol-here)
		(print "LIST: " (fmtStackPrint lst)))

	(print "--- DISPLAY FORMAT: ---")
	(print (fmtDisplay 123))
	(print (fmtDisplay (make-LangFloat (/ 1.0 3.0))))
	(print (fmtDisplay #t))
	(print (fmtDisplay #f))
	(print (fmtDisplay 'abc))
	(print (fmtDisplay (make-LangNull)))
	(print (fmtDisplay "abc"))
	(let ((lst (make LangList)))
		(push-back lst 111)
		(push-back lst 222)
		(push-back lst "hello world")
		(push-back lst 'a-long-symbol-here)
		(print "LIST: " (fmtDisplay lst))))

(test-printing)

;;;==================================================================================
;;; Deserializer
;;;==================================================================================
(import srfi-69) ; hash-tables
(import (chicken io))

(define WORDS (make-hash-table))

(define (replace-escapes text)
	(cond ; recursively apply until no more escapes remain
		((string-contains text "%32") => (lambda (n) (replace-escapes (string-replace text " " n (+ n 3)))))
		((string-contains text "%10") => (lambda (n) (replace-escapes (string-replace text "\n" n (+ n 3)))))
		((string-contains text "%13") => (lambda (n) (replace-escapes (string-replace text "\r" n (+ n 3)))))
		((string-contains text "%37") => (lambda (n) (replace-escapes (string-replace text "%" n (+ n 3)))))
		(else text)))

(define (deserialize-stream fileIn)
	(let ((line (string-trim-both (read-line fileIn))))
		(if (not (eof-object? line))
			(begin
				(print "Line: " line)
				(case (string-ref line 0)
					((#\i) (string->number (string-drop line 2)))
					((#\f) (make-LangFloat (string->number (string-drop line 2))))
					((#\n) (make-LangNull))
					((#\s) (replace-escapes (string-drop line 2)))
					((#\y) (string->symbol (string-drop line 2)))
					((#\b)
						(if (equal? (string-drop line 2) "true")
							#t #f))
					((#\L)
						(let read-list ((nr (string->number (string-drop line 2))) (lst '()))
							(if (> nr 0) 
								(read-list (- nr 1) (append lst (list (deserialize-stream fileIn))))
								(begin
									; other ports don't do this (yet?!) -- remove VOIDs from list
									(set! lst (filter (lambda (obj) (not (LangVoid? obj))) lst))
									(make LangList 'objlist (list->dynvector lst))))))
					((#\F) ; lambda - read list
						(let ((objlist (deserialize-stream fileIn)))
							(if (LangList? objlist)
								(make-LangLambda objlist)
								(raise "Expecting list after 'F' but got: " (fmtStackPrint objlist)))))
					((#\W) ; W name followed by list
						(let ((name (string-drop line 2))
								(objlist (deserialize-stream fileIn)))
							(if (LangList? objlist)
								(begin
									(hash-table-set! WORDS name objlist)
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

(define slot slot-value)

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

		(code '())
		(codepos -1)
		(callstack '())
	)
)

(define-method (push (intr <Interpreter>) obj)
	(if (<= (SP intr) (SP_MIN intr))
		(raise "Stack overflow!"))
	(set! (SP intr) (- (SP intr) 1))
	(dynvector-set! (OBJMEM intr) (SP intr) obj))

(define-method (pop (intr <Interpreter>))
	(if (>= (SP intr) (SP_EMPTY intr))
		(raise "Stack underflow!"))
	(set! (SP intr) (+ (SP intr) 1))
	(dynvector-ref (OBJMEM intr) (- (SP intr) 1)))
	
(define-method (reprStack (intr <Interpreter>))
	(let loop ((s "") (i (- (SP_EMPTY intr) 1)))
		(if (>= i (SP intr))
			(loop (string-append s " " (fmtStackPrint (dynvector-ref (OBJMEM intr) i)))
				(- i 1))
			s)))
		
(define-method (code-call (intr <Interpreter>) objlist)
	(if (not (null? (slot intr 'code)))
		(begin
			(set! (slot intr 'callstack) (cons (list (slot intr 'code) (slot intr 'codepos)) (slot intr 'callstack)))
			(set! (slot intr 'code) objlist)
			(set! (slot intr 'codepos) 0))
		(raise "Call while not running!")))

(define-method (code-return (intr <Interpreter>))
	(if (not (null? (slot intr 'callstack)))
		(begin
			(set! (slot intr 'code) (caar (slot intr 'callstack)))
			(set! (slot intr 'codepos) (cdar (slot intr 'callstack)))
			(set! (slot intr 'callstack) (cdr (slot intr 'callstack))))
		(raise "Return without call!")))

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

(set! L1 (make LangList 'objlist (list->dynvector '(1 2 3 4))))
(set! L2 (make LangList 'objlist (list->dynvector '(5 6 7 8))))
(set! L3 (make LangList 'objlist (list->dynvector '(9 10 11 12))))

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

