;;;==================================================================================
;;; Verbii frontend -- only loads and runs boot.verb.b
;;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;==================================================================================

;; optimization settings (these are supposed to be global but not sure how they are
;; visible in different compilation units, so I'm including this header in all files
(declare (standard-bindings))
(declare (extended-bindings))

;(import srfi-13) ; string library
;(import srfi-34) ; exceptions
;(import (chicken condition)) ; exception object
;(import (chicken format)) ; fprintf
;(import srfi-1) ; list library
;(import dyn-vector)
;(import miscmacros) ; inc! dec! 
;(import srfi-69) ; hash-tables
;(import (chicken io))

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
(define BOOTFILE "../lib/boot.verb.b")

(import (chicken file posix))

; load and run precompiled (.b) file
(define (deserialize-and-run intr filename)
	(let ((fileIn (open-input-file filename)))
		(deserialize-stream intr fileIn)
		; run main to set any vars
		(let ((code (intr-lookup-word-or-fail intr "__main__")))
			; delete __main__ *before* running
			(intr-delete-word intr "__main__")
			(intr-run intr code)
			(close-input-port fileIn))))
		
(import simple-exceptions) ; with-exn-handler

(import (chicken process-context))
(import simple-loops)

;(import srfi-193) ; command line ... is this not in a base chicken library??
;(import (chicken file))

;(print "COMMAND LINE: " (command-line))
;(repl)

(define (main)
	(let ((cmdline-args (new-lang-list)))
		(let ((rest-to-script #f))
			(for-each (lambda (arg)
				; once i get '--' all the remaining args go to NATIVE_CMDLINE_ARGS
				(if rest-to-script
					(llist-push-back cmdline-args (make-String arg))
					; else process as normal
					(cond
						((string=? arg "-stats") (set! SHOW_RUNTIME_STATS #t))
						((string=? arg "--") 
							; the '--' stays in the list passed to boot.verb
							(llist-push-back cmdline-args (make-String arg))
							(set! rest-to-script #t)) ; rest go to NATIVE_CMDLINE_ARGS
						(else
							; anything else goes to script
							(llist-push-back cmdline-args (make-String arg)))))) (cdr (argv))))

		(let ((done #f))
			(do-while (not done)
			(let ((intr (make-Interpreter)))
				(handle-exceptions exn
					(cond
						; for verbii (lang-error) print string (usercode error)
						(((exception-of? 'lang-error) exn) 
							; TODO -- stack trace
							(print (message exn))
							; stop or continue based on EXIT_ON_EXCEPTION
							(set! done EXIT_ON_EXCEPTION))
						; for scheme errors (i.e. bugs in the interpreter, not user code,
						; re-raise the error to show the full traceback - don't try and continue)
						(else (abort exn)))
					;(print "Compile ...")
					;(print "About to run ...")
					; boot.verb expects cmdline args on top of stack
					(push intr cmdline-args)
					(deserialize-and-run intr BOOTFILE)
					; if i made it here, then the above ran OK, so exit now
					(if SHOW_RUNTIME_STATS
						(print-stats intr))
					(set! done #t)))))))
								
(main)




