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

(declare (uses langtypes))
(declare (uses errors))
(declare (uses interpreter))
(declare (uses deserializer))
(declare (uses native))

(import langtypes)
(import errors)
(import interpreter)
(import deserializer)
(import native)

(define SHOW_RUNTIME_STATS #f)
(define NO_CATCH_EXCEPTIONS #f)

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
(import miscmacros)

;(import srfi-193) ; command line ... is this not in a base chicken library??
;(import (chicken file))

;(print "COMMAND LINE: " (command-line))
;(repl)

(define (main)
	(let ((cmdline-args (new-lang-list))
			(current-arg (cdr (argv)))
			(BOOTFILE ""))
		(let ((rest-to-script #f))
			(while (not (null? current-arg))
				; once i get '--' all the remaining args go to NATIVE_CMDLINE_ARGS
				(if rest-to-script
					(llist-push-back cmdline-args (make-String (car current-arg)))
					; else process as normal
					(cond
						((string=? (car current-arg) "-stats") (set! SHOW_RUNTIME_STATS #t))
						((string=? (car current-arg) "-libdir")
							(if (null? (cdr current-arg))
								(begin
									(print "Missing path after -libdir")
									(exit 1)))

							(let ((name (car (cdr current-arg))))
								(if (and (not (equal? (string-ref name (- (string-length name) 1)) #\\))
										(not (equal? (string-ref name (- (string-length name) 1)) #\/)))
									(begin
										(print "-libdir path must end with \\ or /")
										(exit 1)))
								(set! name (string-append name "boot.verb.b"))
								(if (regular-file? name)
									(set! BOOTFILE name)))

							; push -libdir path to script args as well
							(llist-push-back cmdline-args (make-String (car current-arg)))
							(llist-push-back cmdline-args (make-String (car (cdr current-arg))))
							(set! current-arg (cdr current-arg)))
						((string=? (car current-arg) "-noexn") (set! NO_CATCH_EXCEPTIONS #t))
						((string=? (car current-arg) "--") 
							; the '--' stays in the list passed to boot.verb
							(llist-push-back cmdline-args (make-String (car current-arg)))
							(set! rest-to-script #t)) ; rest go to NATIVE_CMDLINE_ARGS
						(else
							; anything else goes to script
							(llist-push-back cmdline-args (make-String (car current-arg))))))
					(set! current-arg (cdr current-arg))))

		(if (equal? (string-length BOOTFILE) 0)
			(begin
				(print "Unable to find boot.verb.b -- maybe you need to pass -libdir or set VERBII_BOOT?")
				(exit 1)))

		(if NO_CATCH_EXCEPTIONS
			; when things are really broken, run with -noexn to see raw crash from 
			; chicken, instead of catching exceptions
			(let ((intr (make-Interpreter)))
				(push intr cmdline-args)
				(deserialize-and-run intr BOOTFILE))
			; else, in the normal case, run with exception catching
			(let ((done #f))
				(while (not done)
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
						(set! done #t))))))))
									
(main)




