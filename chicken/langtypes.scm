;;;==================================================================================
;;; langtypes.scm
;;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;
;;; Type mappings:
;;;
;;;	verbii		Lua
;;;	------		---
;;;	null		LangNull (class) ** maybe change this to '() ?? **
;;; void		LangVoid (class)
;;;	int			integer
;;;	float		LangFloat (class)
;;;	boolean		boolean
;;;	symbol		string	** like Python & Lua ports, also see below **
;;;	string		LangString (class)
;;;	lambda		LangLambda
;;;==================================================================================

; module header
(module langtypes *
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
; shorthand
(define slot slot-value)

(import simple-exceptions)

; define local version of lang-error, to avoid circular import. use same tags so that
; exception handlers will see it as a lang-error
(define langtype-error (lambda (wheresym . args)
	(raise ((make-exception 
		(string-append "[" (symbol->string wheresym) "] " 
			(string-join (map fmtDisplay args) " ")) 'lang-error) 'lang-error))))

; like the Python port, plain strings are used as symbols, and LangString is used
; for strings. i think this will be better to avoid at least two corner cases: (1) symbols
; starting with ' are treated specially by scheme and (2) i'm not sure it is possible to
; create empty symbols in scheme, which are needed in places. i also don't see a fast
; way to get a substring of a symbol, which is also needed a lot.

; add some methods to strings for consistency with other objects
(define-method (len (str <string>)) (string-length str))
(define-method (get (str <string>) index) (string-ref str index))
(define-method (value (str <string>)) str)
(define LangSymbol? string?)

; need a null type that is distinct from '() so that '() can be the empty list
; ---- ugh ... that's obsolete since I have LangList, but I still prefer LangNull to '()

(define-class LangNull ())
	
	(define (LangNull? obj) (subclass? (class-of obj) LangNull))

; and a void type distinct from null & '()
(define-class LangVoid ())
	
	(define (LangVoid? obj) (subclass? (class-of obj) LangVoid))

(define-method (value (i <integer>)) i)

; in scheme a number like 123.0 tests true as an integer; in verbii it is a float, so cannot
; use scheme floats directly (at least I haven't figure out a way)
(define-class LangFloat ()
	((value accessor: value initform: 0)))
	
	(define (LangFloat? obj) (subclass? (class-of obj) LangFloat))

; NOTE - use this factory function to make LangFloats instead of calling 'make' directly
(define (make-lang-float value) 
	; ensure i really have a float value
	(make LangFloat 'value (exact->inexact value)))

; for code that doesn't care if its int or float and will just use (value obj)
(define (is-numeric? obj) (or (integer? obj) (LangFloat? obj)))

(define-class LangLambda ()
	((llist accessor: llist initform: (make LangList))))

	(define (LangLambda? obj) (subclass? (class-of obj) LangLambda))

; verbii string type
(define-class LangString ()
	((value initform: "" accessor: value)))

	(define (LangString? obj) (subclass? (class-of obj) LangString))

	; get i'th char
	(define-method (get (str LangString) index) 
		(string-ref (value str) index)) 
	; sequences get 'len'
	(define-method (len (str LangString)) 
		(string-length (value str)))

; verbii lists are really arrays, so use a vector
(define-class LangList () (
		(objlist accessor: objlist initform: (make-dynvector 0 0)) ))

	; get i'th object
	(define-method (get (llist LangList) index)
		(if (or (< index 0) (>= index (dynvector-length (objlist llist))))
			(langtype-error 'LangList-get "Out of bounds in LangList")
			(dynvector-ref (objlist llist) index)))

	; append an object
	(define-method (push-back (llist LangList) obj)
		(dynvector-set! (objlist llist) (dynvector-length (objlist llist)) obj))

	; sequences get len
	(define-method (len (llist LangList))
		(dynvector-length (objlist llist)))

	(define (LangList? obj) (subclass? (class-of obj) LangList))

	(define (list->LangList lst) 
		;(print "LANGLIST FROM LIST: " lst)
		(make LangList 'objlist (list->dynvector lst)))

;(define (LangNull? obj) (subclass? (class-of obj) LangNull))

;; see c++ comments for display vs. stack format
(define (fmtStackPrint obj)
	;(print "FMT_STACK_PRINT:" obj)
	(cond
		((integer? obj) (number->string obj))
		((LangFloat? obj) 
			(let ((s (string-append "#" (number->string (value obj)))))
				(if (and (> (string-length s) 2) (equal? (string-take-right s 2) ".0"))
					(string-drop-right s 2)
					s)))
		((boolean? obj) (if obj "<true>" "<false>"))
		((LangSymbol? obj) obj) ; obj is a string
		((LangString? obj) (string-append "\"" (value obj) "\""))
		((LangList? obj)
			(string-append 
				(dynvector-fold (lambda (i str obj) (string-append str " " (fmtStackPrint obj))) "[" 
					(objlist obj)) " ]"))
		((LangNull? obj) "<null>")
		((LangVoid? obj) "<VOID>")
		((LangLambda? obj) "<lambda>")
		; for sanity, don't use lang-error (or langtype-error) just in case they switch to
		; using fmtStackPrint. this would be an obvious bug that needs to be fixed, so just
		; print message and exit
		(else (print "FATAL ERROR: Unknown object in fmtStackPrint: " obj) (exit 1))))

;; see c++ comments for display vs. stack format
(define (fmtDisplay obj)
	(cond
		((integer? obj) (number->string obj))
		((LangFloat? obj) 
			; for consistency with other ports, if number string ends with ".0", remove it
			(let ((s (number->string (value obj))))
				(if (and (> (string-length s) 2) (equal? (string-take-right s 2) ".0"))
					(string-drop-right s 2)
					s)))
		((boolean? obj) (if obj "true" "false"))
		((LangSymbol? obj) (string-append "'" obj)) ; obj is a string
		((LangString? obj) (value obj))
		; OTHER PORTS USE fmtStackPrint here ... trying this out to see if i like it better ...
		((LangList? obj)
			(string-append 
				(dynvector-fold (lambda (i str obj) (string-append str " " (fmtDisplay obj))) "[" 
					(objlist obj)) " ]"))
		((LangNull? obj) "<null>")
		((LangVoid? obj) "<VOID>")
		((LangLambda? obj) "<lambda>")
		; like above, but lang-error/langtype-error DOES us this so it would be a loop to
		; use those here
		(else 
			(print "FATAL ERROR: Unknown object in fmtDisplay: " obj) (exit 1))))

) ; end of module