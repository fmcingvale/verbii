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
(import (chicken condition)) ; exception object
(import (chicken format)) ; fprintf
(import srfi-1) ; list library
(import coops)
(import dyn-vector)
(import miscmacros) ; inc! dec! 
(import srfi-69) ; hash-tables
(import (chicken io))

; shorthand
(define slot slot-value)

(import (chicken platform))
(cond-expand
	(compiling #t)
	; call load only when running as a script, not when compiled
	; (if called in the compiled version, it appears to run the .scm instead
	; of the compiled versions)
	(else
		;(print "LOADING .SCM FILES")
		(load "langtypes.scm")
		(load "errors.scm")
		(load "deserializer.scm")
		(load "interpreter.scm")
		(load "native.scm")))

(import langtypes)
(import errors)
(import deserializer)
(import native)
(import interpreter)

(define (test-printing)
	(print "--- STACK FORMAT: ---")
	(print (fmtStackPrint 123))
	(print (fmtStackPrint (make-lang-float (/ 1.0 3.0))))
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
	(print (fmtDisplay (make-lang-float (/ 1.0 3.0))))
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

;(test-printing)


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


(define (intr-tests)
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
)

; load a precompiled (.b) file into interpreter
(define (load-byte-compiled-file intr filename)
	(let* ((fileIn (open-input-file filename))
			(result (deserialize-stream intr fileIn)))
		(close-input-port fileIn)
		result))

(define (new-interpreter)
	(let ((intr (make <Interpreter>)))
		(load-byte-compiled-file intr "../lib/init.verb.b")
		(run intr (hash-table-ref (WORDS intr) "__main__"))
		(load-byte-compiled-file intr "../lib/compiler.verb.b")
		; do NOT run __main__ for compiler since that would run command-line compiler
		intr))
		
(define (byte-compile intr text)
	;(print "BYTE-COMPILING text: " text)
	(push intr (make LangString 'value text))
	(run intr (hash-table-ref (WORDS intr) "byte-compile-string"))
	;(print "STACK NOW: " (reprStack intr)))
)

;(set! intr (new-interpreter))
;(print "WORDS:" (hash-table-keys (WORDS intr)))
;(hash-table-walk (WORDS intr) (lambda (key val) (print key ": " (fmtStackPrint val))))

;(print (list->LangList (list 2048)))

;(byte-compile intr "123 456 789")
;(print "COMPILED TO: " (fmtStackPrint (hash-table-ref (WORDS intr) "__main__")))
;(run intr (hash-table-ref (WORDS intr) "__main__"))
;(print "STACK AFTER RUN: " (reprStack intr))

;(import (chicken process signal))
;(import breadline)
; from example code in breadline docs
;(set-signal-handler! signal/int
;	(lambda _
;		(cleanup-after-signal!)
;		(reset-after-signal!)
;		; I added this ...
;		;(print "Use quit to exit")))
;		(reset-terminal!)
;		(exit 1)))
		
;(on-exit reset-terminal!)

(import simple-exceptions) ; with-exn-handler

; compile and run text - if OK returns null, else returns error message (string)
(define (safe-compile-and-run intr text)
	(handle-exceptions exn
		(cond
			; for verbii (lang-error) return string, caller can handle
			(((exception-of? 'lang-error) exn) (message exn))
			; for scheme errors (i.e. bugs in the interpreter, not user code,
			; re-raise the error to show the full traceback)
			(else (abort exn)))
		;(print "Compile ...")
		(byte-compile intr text)
		(pop intr) ; pop list of compiled words, don't need
		;(print "Run ...")
		(run intr (hash-table-ref (WORDS intr) "__main__"))
		; ran OK if we made it here, return null
		'()))

; for debugging - does same as safe-compile-and-run but doesn't catch exceptions
(define (unsafe-compile-and-run intr text)
	(byte-compile intr text)
	(pop intr) ; pop list of compiled words, don't need
	(run intr (hash-table-ref (WORDS intr) "__main__"))
	'())

; normally this should be set to the safe version, but to get tracebacks on scheme
; errors, set it to the unsafe version instead
(define compile-and-run safe-compile-and-run)
;(define compile-and-run unsafe-compile-and-run)

(define (repl)
	(let ((intr (new-interpreter)))
		(let repl-loop ()
			(display ">> ")
			(let ((line (read-line)))
				(cond
					((equal? line "quit")) ; stop looping
					((equal? line ",q")) ; shorthand for 'quit'
					(else
						(let ((result (compile-and-run intr line)))
							(if (null? result)
								; no errors, print stack and continue
								(begin
									(print "=>" (reprStack intr))
									(repl-loop))
								; print error and restart interpreter
								(begin
									(print "** ERROR ** " result)
									(set! intr (new-interpreter))
									(repl-loop))))))))))

(import (chicken file posix))

(define (run-program filename)
	(let* ((intr (new-interpreter))
			(fileIn (file-open filename open/rdonly))
			(text (car (file-read fileIn (file-size fileIn)))))
		(file-close fileIn)
		;(print "READ FILE: " text)
		(let ((result (compile-and-run intr text)))
			(if (not (null? result))
				(print "** ERROR ** " result)))))

; does text contain only whitespace?
(define (blank-string? text)
	(equal? 0 (string-length (string-trim-both text))))

; like a non-interactive repl that runs a line at a time, printing either the
; stack or error message that occurred, restarting the interpreter on errors			
(define (run-test filename)
	(with-input-from-file filename (lambda ()
		(let ((intr (new-interpreter)))
			(let run-loop ((line (read-line)))
				(if (not (eof-object? line))
					(if (blank-string? line)
						(run-loop (read-line))
					(begin
			
						(print ">> " line)
						(let ((result (compile-and-run intr line)))
							(if (null? result)
								; no errors, print stack and continue
								(begin
									(print "=>" (reprStack intr))
									(run-loop (read-line)))
								; else print error and restart interpreter
								(begin
									(print result)
									(set! intr (new-interpreter))
									(run-loop (read-line)))))))))))))

(import srfi-193)
(import (chicken file))

;(print "COMMAND LINE: " (command-line))
;(repl)

(define (main)
	(let ((filename #f)
			(test-mode #f)
			(script-args #f))
		(for-each (lambda (arg)
			; once i get '--' all the remaining args go to script-args
			(if script-args
				(set! script-args (append script-args (list arg)))
				; else process as normal
				(cond
					((equal? arg "-test") (set! test-mode #t))
					((equal? arg "--") (set! script-args '())) ; rest go to script-args
					(else
						(if (and (file-exists? arg) (not filename))
							(set! filename arg)
							(begin
								(print "Unknown arg: " arg)
								(exit 1))))))) (cdr (command-line)))
		;(print "Parsed args:")
		;(print "Filename: " filename)
		;(print "test mode: " test-mode)
		;(print "script args: " script-args)
		; decide what to do based on args
		(cond
			((not filename) (repl))
			((and filename test-mode) (run-test filename))
			((and filename (not test-mode)) (run-program filename))
			(else (print "NOT IMPLEMENTED YET")))))


(main)




