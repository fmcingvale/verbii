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
(import (chicken format)) ; fprintf
(import srfi-1) ; list library
(import coops)
(import dyn-vector)
(import miscmacros) ; inc! dec! 
(import srfi-69) ; hash-tables
(import (chicken io))

; shorthand
(define slot slot-value)

; must comment these out when building standalone executable.
; uncomment them when running under interpreter.
; ... there has to be a better way ...
(load "langtypes.scm")
(load "deserializer.scm")
(load "interpreter.scm")
(load "native.scm")


(import langtypes)
(import deserializer)
(import native)
(import interpreter)

(define (test-printing)
	(print "--- STACK FORMAT: ---")
	(print (fmtStackPrint 123))
	(print (fmtStackPrint (make LangFloat 'value (/ 1.0 3.0))))
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
	(print (fmtDisplay (make LangFloat 'value (/ 1.0 3.0))))
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

(test-printing)


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

; load a precompiled (.b) file into interpreter
(define (load-byte-compiled-file intr filename)
	(let* ((fileIn (open-input-file filename))
			(result (deserialize-stream intr fileIn)))
		(close-input-port fileIn)
		result))

(define (new-interpreter)
	(let ((intr (make <Interpreter>)))
		(load-byte-compiled-file intr "../lib/init.verb.b")
		(load-byte-compiled-file intr "../lib/compiler.verb.b")
		intr))
		
(define (byte-compile intr text)
	(print "BYTE-COMPILING text: " text)
	(push intr (make LangString 'value text))
	(run intr (hash-table-ref (WORDS intr) "byte-compile-string"))
	(print "STACK NOW: " (reprStack intr)))

(set! intr (new-interpreter))
(print "WORDS:" (hash-table-keys (WORDS intr)))
(hash-table-walk (WORDS intr) (lambda (key val) (print key ": " (fmtStackPrint val))))

(print (list->LangList (list 2048)))

(byte-compile intr "123 456 789")
(print "COMPILED TO: " (fmtStackPrint (hash-table-ref (WORDS intr) "__main__")))
(run intr (hash-table-ref (WORDS intr) "__main__"))
(print "STACK AFTER RUN: " (reprStack intr))
