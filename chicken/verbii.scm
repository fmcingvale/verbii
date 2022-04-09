;;;==================================================================================
;;; verbii in chicken scheme
;;;
;;; all one long file for now since i haven't figured out chicken modules ...
;;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;==================================================================================

;; optimization settings (these are supposed to be global but not sure how they are
;; visible in different compilation units, so I'm including this header in all files
(declare (standard-bindings))
(declare (extended-bindings))

;; this is my first try using coops so ... may be some suboptimal stuff here ...

;(import coops-primitive-objects)
(import srfi-13) ; string library
(import srfi-34) ; exceptions
(import (chicken condition)) ; exception object
(import (chicken format)) ; fprintf
(import srfi-1) ; list library
;(import coops)
(import dyn-vector)
(import miscmacros) ; inc! dec! 
(import srfi-69) ; hash-tables
(import (chicken io))

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
		(load "interpreter.scm")
		(load "deserializer.scm")
		(load "native.scm")))

(import langtypes)
(import errors)
(import interpreter)
(import deserializer)
(import native)

(define SHOW_RUNTIME_STATS #f)

; load a precompiled (.b) file into interpreter
(define (load-byte-compiled-file intr filename)
	(let* ((fileIn (open-input-file filename))
			(result (deserialize-stream intr fileIn)))
		(close-input-port fileIn)
		result))

(define (make-loaded-interpreter)
	(let ((intr (make-Interpreter)))
		(load-byte-compiled-file intr "../lib/init.verb.b")
		(intr-run intr (hash-table-ref (WORDS intr) "__main__"))
		(load-byte-compiled-file intr "../lib/compiler.verb.b")
		(intr-run intr (hash-table-ref (WORDS intr) "__main__"))
		
		; delete __main__ so I don't inadvertently reuse it later
		(intr-delete-word intr "__main__")

		intr))
		
(define (byte-compile-and-load intr text)
	;(print "BYTE-COMPILING text: " text)
	(push intr (make-LangString text))
	(intr-run intr (hash-table-ref (WORDS intr) "compile-and-load-string"))
	;(print "STACK NOW: " (reprStack intr)))
)

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
		(byte-compile-and-load intr text)
		;(print "Run ...")
		(intr-run intr (hash-table-ref (WORDS intr) "__main__"))
		; delete __main__ once run
		(intr-delete-word intr "__main__")
		; ran OK if we made it here, return null
		'()))

; for debugging - does same as safe-compile-and-run but doesn't catch exceptions
(define (unsafe-compile-and-run intr text)
	(byte-compile-and-load intr text)
	(intr-run intr (hash-table-ref (WORDS intr) "__main__"))
	; delete __main__ once run
	(intr-delete-word intr "__main__")
	'())

; normally this should be set to the safe version, but to get tracebacks on scheme
; errors, set it to the unsafe version instead
(define compile-and-run safe-compile-and-run)
;(define compile-and-run unsafe-compile-and-run)

(define (repl)
	(print "Verbii running on Chicken " (chicken-version))
	(let ((intr (make-loaded-interpreter)))
		(let repl-loop ()
			(display ">> ")
			(let ((line (read-line)))
				(cond
					((or (string=? line "quit") ; stop looping
						(string=? line ",q"))
						(if SHOW_RUNTIME_STATS (print-stats intr)))
					(else
						(let ((result (compile-and-run intr line)))
							(if (null? result)
								; no errors, print stack and continue
								(begin
									(print "=>" (reprStack intr))
									(repl-loop))
								; print error and restart interpreter
								(begin
									(print result)
									(set! intr (make-loaded-interpreter))
									(repl-loop))))))))))

(import (chicken file posix))

(define (run-program filename)
	(let* ((intr (make-loaded-interpreter))
			(fileIn (file-open filename open/rdonly))
			(text (car (file-read fileIn (file-size fileIn)))))
		(file-close fileIn)
		;(print "READ FILE: " text)
		(let ((result (compile-and-run intr text)))
			(if (not (null? result))
				(print result))
			(if SHOW_RUNTIME_STATS (print-stats intr)))))

; does text contain only whitespace?
(define (blank-string? text)
	(= 0 (string-length (string-trim-both text))))

; like a non-interactive repl that runs a line at a time, printing either the
; stack or error message that occurred, restarting the interpreter on errors			
(define (run-test filename)
	(with-input-from-file filename (lambda ()
		(let ((intr (make-loaded-interpreter)))
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
									(set! intr (make-loaded-interpreter))
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
					((string=? arg "-test") (set! test-mode #t))
					((string=? arg "-stats") (set! SHOW_RUNTIME_STATS #t))
					((string=? arg "--") (set! script-args '())) ; rest go to script-args
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




