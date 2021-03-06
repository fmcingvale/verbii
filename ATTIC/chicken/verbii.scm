;;;==================================================================================
;;; verbii repl
;;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;==================================================================================

;; optimization settings (these are supposed to be global but not sure how they are
;; visible in different compilation units, so I'm including this header in all files
(declare (standard-bindings))
(declare (extended-bindings))

(import srfi-13) ; string library
(import srfi-34) ; exceptions
(import (chicken condition)) ; exception object
(import (chicken format)) ; fprintf
(import srfi-1) ; list library
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

(import (chicken file posix))

(define (readfile filename)
	(let* ((fileIn (file-open filename open/rdonly))
			(text (car (file-read fileIn (file-size fileIn)))))
		(file-close fileIn)
		text))

(define (compile-and-load intr text allow_overwrite)
	(set! ALLOW_OVERWRITING_WORDS allow_overwrite)
	(push intr (make-String text))
	(intr-run intr (intr-lookup-word-or-fail intr "compile-and-load-string"))
	(set! ALLOW_OVERWRITING_WORDS #f))

; compile & run, with NO exception handling
(define (compile-and-run intr text allow_overwrite)
	(compile-and-load intr text allow_overwrite)
	(intr-run intr (intr-lookup-word-or-fail intr "__main__"))
	; delete __main__ once run
	(intr-delete-word intr "__main__")
	'())

; load and run precompiled (.b) file
(define (deserialize-and-run intr filename)
	(let ((fileIn (open-input-file filename)))
		(deserialize-stream intr fileIn)
		; run main to set any vars
		(intr-run intr (intr-lookup-word-or-fail intr "__main__"))
		(intr-delete-word intr "__main__")
		(close-input-port fileIn)))

(define (make-loaded-interpreter verbose)
	(let ((intr (make-Interpreter)))
		(deserialize-and-run intr "../lib/init.verb.b")
		(deserialize-and-run intr "../lib/compiler.verb.b")
		
		; load patches (currently, not run, just loaded for words)
		(let ((buf (readfile "../lib/patches.verb")))
			; if file is large, print a message since this will take a bit ...
			(if (and (> (string-length buf) 1000) verbose)
				(print "Patching ..."))
			(compile-and-run intr buf #t) ; allow overwriting words
			)
		
		intr))
		
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
		(compile-and-run intr text #f)
		; ran OK if we made it here, return null
		'()))

; normally this should be set to the safe version, but to get tracebacks on scheme
; errors, set it to the unsafe version instead

(define (repl)
	(print "Verbii running on Chicken " (chicken-version))
	(let ((intr (make-loaded-interpreter #t)))
		(let repl-loop ()
			(display ">> ")
			(let ((line (read-line)))
				(cond
					((or (string=? line "quit") ; stop looping
						(string=? line ",q"))
						(if SHOW_RUNTIME_STATS (print-stats intr)))
					(else
						(let ((result (safe-compile-and-run intr line)))
							(if (null? result)
								; no errors, print stack and continue
								(begin
									(print "=>" (reprStack intr))
									(repl-loop))
								; print error and restart interpreter
								(begin
									(print result)
									(set! intr (make-loaded-interpreter #t))
									(repl-loop))))))))))

(define (run-program filename)
	(let* ((intr (make-loaded-interpreter #f))
			(text (readfile filename)))
		;(print "READ FILE: " text)
		(let ((result (safe-compile-and-run intr text)))
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
		(let ((intr (make-loaded-interpreter #f)))
			(let run-loop ((line (read-line)))
				(if (not (eof-object? line))
					(if (blank-string? line)
						(run-loop (read-line))
					(begin
			
						(print ">> " line)
						(let ((result (safe-compile-and-run intr line)))
							(if (null? result)
								; no errors, print stack and continue
								(begin
									(print "=>" (reprStack intr))
									(run-loop (read-line)))
								; else print error and restart interpreter
								(begin
									(print result)
									(set! intr (make-loaded-interpreter #f))
									(run-loop (read-line)))))))))))))

(import srfi-193)
(import (chicken file))

;(print "COMMAND LINE: " (command-line))
;(repl)

(define (main)
	(let ((filename #f)
			(test-mode #f))
		(set! NATIVE_CMDLINE_ARGS '())
		(for-each (lambda (arg)
			; once i get '--' all the remaining args go to NATIVE_CMDLINE_ARGS
			(if (not (null? NATIVE_CMDLINE_ARGS))
				(llist-push-back NATIVE_CMDLINE_ARGS (make-String arg))
				; else process as normal
				(cond
					((string=? arg "-test") (set! test-mode #t))
					((string=? arg "-stats") (set! SHOW_RUNTIME_STATS #t))
					((string=? arg "--") (set! NATIVE_CMDLINE_ARGS (new-lang-list))) ; rest go to NATIVE_CMDLINE_ARGS
					(else
						(if (and (file-exists? arg) (not filename))
							(set! filename arg)
							(begin
								(print "Unknown arg: " arg)
								(exit 1))))))) (cdr (command-line)))

		; decide what to do based on args
		(if (null? NATIVE_CMDLINE_ARGS)
			(set! NATIVE_CMDLINE_ARGS (new-lang-list)))

		(cond
			((not filename) (repl))
			((and filename test-mode) (run-test filename))
			((and filename (not test-mode)) (run-program filename))
			(else (print "NOT IMPLEMENTED YET")))))

(main)




