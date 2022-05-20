;;;==================================================================================
;;; langtypes.scm
;;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;
;;; Type mappings:
;;;
;;;	verbii		Lua
;;;	------		---
;;;	null		Null (class) ** maybe change this to '() ?? **
;;; void		Void (class)
;;;	int			integer
;;;	float		Float (class)
;;;	boolean		boolean
;;;	symbol		string	** like Python & Lua ports, also see below **
;;;	string		String (class)
;;;	lambda		Lambda
;;;==================================================================================

;; optimization settings (these are supposed to be global but not sure how they are
;; visible in different compilation units, so I'm including this header in all files
(declare (standard-bindings))
(declare (extended-bindings))

; module header
(module langtypes *
(import scheme)
(import (chicken base))
(import (chicken syntax))

; start of module code

; could probably trim some of these ...
;(import coops-primitive-objects)
(import srfi-13) ; string library
(import srfi-34) ; exceptions
(import (chicken format)) ; fprintf
(import srfi-1) ; list library
(import srfi-69) ; hash tables
;(import coops)
(import dyn-vector)
(import miscmacros) ; inc! dec!
(import (chicken sort)) ; sort! 
; shorthand
;(define slot slot-value)

(import simple-exceptions)

; define local version of lang-error, to avoid circular import. use same tags so that
; exception handlers will see it as a lang-error
(define langtype-error (lambda (wheresym . args)
	(raise ((make-exception 
		(string-append "[" (symbol->string wheresym) "] " 
			(string-join (map fmtDisplay args) " ")) 'lang-error) 'lang-error))))

; like the Python port, plain strings are used as symbols, and String is used
; for strings. i think this will be better to avoid at least two corner cases: (1) symbols
; starting with ' are treated specially by scheme and (2) i'm not sure it is possible to
; create empty symbols in scheme, which are needed in places. i also don't see a fast
; way to get a substring of a symbol, which is also needed a lot.

; add some methods to strings for consistency with other objects
;(define-method (len (str <string>)) (string-length str))
;(define-method (get (str <string>) index) (string-ref str index))
;(define-method (value (str <string>)) str)
(define Symbol? string?)

; need a null type that is distinct from '() so that '() can be the empty list
; ---- ugh ... that's obsolete since I have List, but I still prefer Null to '()

(define-record Null)
;(define-class Null ())
	
;	(define (Null? obj) (subclass? (class-of obj) Null))

; and a void type distinct from null & '()
;(define-class Void ())
	
	;(define (Void? obj) (subclass? (class-of obj) Void))
(define-record Void)

; (define-method (value (i <integer>)) i)

; see c++ notes on integer limits
(define MAX_VINT 9007199254740991)
(define MIN_VINT -9007199254740991)

; in scheme a number like 123.0 tests true as an integer; in verbii it is a float, so cannot
; use scheme floats directly (at least I haven't figured out a way)
(define-record Float value)
;(define-class Float ()
;	((value accessor: value initform: 0)))
	
	;(define (Float? obj) (subclass? (class-of obj) Float))

; NOTE - use this factory function to make Floats instead of calling 'make' directly
(define (make-lang-float value) 
	; ensure i really have a float value
	;(make Float 'value (exact->inexact value)))
	(make-Float (exact->inexact value)))

; for code that doesn't care if its int or float and will just use (value obj)
(define (is-numeric? obj) (or (integer? obj) (Float? obj)))
(define (as-numeric obj)
	(cond
		((integer? obj) obj)
		((Float? obj) (Float-value obj))
		; can't use lang-error here, but this is an internal error anyways
		(else
			(error "FATAL ERROR: Not a numeric value in as-numeric")
			(exit 1))))
			
; more generic version to get data portion of object
;
; should use type-specific getter when possible, this is just for code that needs
; to be generic
(define (value obj)
	(cond
		((integer? obj) obj)
		((string? obj) obj)
		((Float? obj) (Float-value obj))
		((String? obj) (String-value obj))))

; dictionary
(define-record Dict table)
;(define (new-Dict) (make-Dict (make-hash-table #:test string=?)))
(define (new-Dict) (make-Dict (make-hash-table)))

; holds a List as llist
(define-record Lambda llist)
(define (lambda-llist obj) (Lambda-llist obj))

; holds a List and an arbitrary object
(define-record Closure llist state)

;(define-class Lambda ()
;	((llist accessor: llist initform: (make List))))

;	(define (Lambda? obj) (subclass? (class-of obj) Lambda))

; verbii string type
(define-record String value)
;(define-class String ()
;	((value initform: "" accessor: value)))

;	(define (String? obj) (subclass? (class-of obj) String))

	; get i'th char
	;(define-method (get (str String) index) 
	;	(string-ref (value str) index)) 
	; sequences get 'len'
	;(define-method (len (str String)) 
	;	(string-length (value str)))

; verbii lists are really arrays, so use a vector
(define-record List objlist)

(define (new-lang-list) (make-List (make-dynvector 0 0)))

;(define-class List () (
;		(objlist accessor: objlist initform: (make-dynvector 0 0)) ))

; get i'th object
(define (llist-get llist index)
	(if (or (< index 0) (>= index (dynvector-length (List-objlist llist))))
		(langtype-error 'List-get "Out of bounds in List")
		(dynvector-ref (List-objlist llist) index)))

; append an object
(define (llist-push-back llist obj)
	(dynvector-set! (List-objlist llist) (dynvector-length (List-objlist llist)) obj))

	; sequences get len
	;(define-method (len (llist List))
	;	(dynvector-length (objlist llist)))

	;(define (List? obj) (subclass? (class-of obj) List))

	(define (list->List lst) 
		;(print "LANGLIST FROM LIST: " lst)
		;(make List 'objlist (list->dynvector lst)))
		(make-List (list->dynvector lst)))

;(define (Null? obj) (subclass? (class-of obj) Null))

; a generic "length" operator, but better to use type-specific when possible
(define (len obj)
	(cond
		((string? obj) (string-length obj))
		((String? obj) (string-length (String-value obj)))
		((List? obj) (dynvector-length (List-objlist obj)))
		((Dict? obj) (hash-table-size (Dict-table obj)))
		(else
			(error "Bad object in len")
			(exit 1))))

(define (fmtStackPrintObjlist objlist open-delim close-delim)
	(string-append 
		(dynvector-fold (lambda (i str obj) (string-append str " " (fmtStackPrint obj))) open-delim
			objlist) " " close-delim))

;; see c++ comments for display vs. stack format
(define (fmtStackPrint obj)
	;(print "FMT_STACK_PRINT:" obj)
	(cond
		((integer? obj) (number->string obj))
		((Float? obj) 
			(let ((s (string-append "#" (number->string (value obj)))))
				(if (and (> (string-length s) 2) (string=? (string-take-right s 2) ".0"))
					(string-drop-right s 2)
					s)))
		((boolean? obj) (if obj "<true>" "<false>"))
		((Symbol? obj) (string-append "'" obj)) ; obj is a string
		((String? obj) (string-append "\"" (value obj) "\""))
		((List? obj)
			(fmtStackPrintObjlist (List-objlist obj) "[" "]"))
		((Closure? obj)
			(string-append "<" 
				(fmtStackPrintObjlist (List-objlist (Closure-llist obj)) "{" "}")
				" :: " (fmtStackPrint (Closure-state obj)) ">"))
		((Null? obj) "<null>")
		((Void? obj) "<VOID>")
		((Lambda? obj)
			(string-append "<"
				(fmtStackPrintObjlist (List-objlist (lambda-llist obj)) "{" "}")
				">"))
		((Dict? obj)
			(let* ((u-keys (hash-table-keys (Dict-table obj)))
					(keys (sort u-keys string<?)))
				(string-append "{ "
					(fold (lambda (key str)
						(string-append str "\"" (value key) "\" => " 
							(fmtStackPrint (hash-table-ref (Dict-table obj) (value key))) " "))
							"" keys)
					"}")))

		; for sanity, don't use lang-error (or langtype-error) just in case they switch to
		; using fmtStackPrint. this would be an obvious bug that needs to be fixed, so just
		; print message and exit
		(else (print "FATAL ERROR: Unknown object in fmtStackPrint: " obj) (exit 1))))

;; see c++ comments for display vs. stack format
(define (fmtDisplayObjlist objlist open-delim close-delim)
	(string-append 
		(dynvector-fold (lambda (i str obj) (string-append str " " (fmtDisplay obj))) open-delim
			objlist) " " close-delim))

(define (fmtDisplay obj)
	(cond
		((integer? obj) (number->string obj))
		((Float? obj) 
			; for consistency with other ports, if number string ends with ".0", remove it
			(let ((s (number->string (value obj))))
				(if (and (> (string-length s) 2) (string=? (string-take-right s 2) ".0"))
					(string-drop-right s 2)
					s)))
		((boolean? obj) (if obj "true" "false"))
		((Symbol? obj) obj) ; obj is a string
		((String? obj) (value obj))
		((List? obj)
			(string-append 
				(dynvector-fold (lambda (i str obj) (string-append str " " (fmtDisplay obj))) "[" 
					(List-objlist obj)) " ]"))
		((Closure? obj)
			(string-append "<" 
				(fmtDisplayObjlist (List-objlist (Closure-llist obj)) "{" "}")
				" :: " (fmtDisplay (Closure-state obj)) ">"))
		((Null? obj) "<null>")
		((Void? obj) "<VOID>")
		((Lambda? obj)
			(string-append "<"
				(fmtDisplayObjlist (List-objlist (lambda-llist obj)) "{" "}")
				">"))
		((Dict? obj)
			(let* ((u-keys (hash-table-keys (Dict-table obj)))
					(keys (sort u-keys string<?)))
				(string-append "{ "
					(fold (lambda (key str)
						(string-append str "\"" key "\" => " 
							(fmtDisplay (hash-table-ref (Dict-table obj) key)) " "))
							"" keys)
					"}")))
		; like above, but lang-error/langtype-error DOES us this so it would be a loop to
		; use those here
		(else 
			(print "FATAL ERROR: Unknown object in fmtDisplay: " obj) (exit 1))))

; see c++ & DESIGN-NOTES.txt
(define (deepcopy obj)
	(cond
		((or (integer? obj)
			(Float? obj)
			(boolean? obj)
			(Symbol? obj)
			(String? obj)
			(Closure? obj)
			(Null? obj)
			(Void? obj)
			(Lambda? obj))
			obj)
		((List? obj)
			(let ((newlist (new-lang-list)))
				(dynvector-for-each 
					(lambda (i elem) (dynvector-set! (List-objlist newlist) i (deepcopy elem)))
						(List-objlist obj))
				newlist))
		((Dict? obj)
			(let ((newdict (new-Dict)))
				(hash-table-walk (Dict-table obj)
					(lambda (k v)
						(hash-table-set! (Dict-table newdict) k (deepcopy v))))
				newdict))
		(else 
			(print "Don't know how to deepcopy object:" (fmtStackPrint obj) ) (exit 1))))

) ; end of module