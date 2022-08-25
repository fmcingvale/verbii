;;;==================================================================================
;;; langtypes.scm
;;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;
;;; Type mappings:
;;;
;;;	verbii		Chicken
;;;	------		---
;;;	null		Null (class) 
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

(declare (unit langtypes))

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

; need a verbii null object distinct from '() -- much like in the Python port, there
; are places where '() is more natural to use on the scheme side, so having a Null object
; makes it clear that those instances are not verbii nulls.
(define-record Null)

; ... and a void type distint from null -- see notes in c++/langtypes.hpp
(define-record Void)

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

(define (parse-bool text)
	(cond
		((string=? text "true") #t)
		((string=? text "false") #f)
		(else (langtype-error 'parse-bool "Bad boolean literal: " text))))

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

; makes:
;	(make-Opcode code A B C)
;	(Opcode? obj)
;	(Opcode-code obj)
;	(Opcode-code-set! obj)
;	.. etc .. for A,B,C
(import (chicken bitwise))

(define-record Opcode code A B C)

; I really want these to be in opcodes.scm but had so much trouble
; with mutual dependency issues I gave in and put these here ...

; keep these synced with c++
(define OPCODE-FRAME-GET 0)
(define OPCODE-FRAME-SET 1)
(define OPCODE-JUMP-FORW 2)
(define OPCODE-JUMP-BACK 3)

(define OPCODE-NAME-TO-CODE
	(alist->hash-table
		`(	("FRAME-GET" . ,OPCODE-FRAME-GET)
			("FRAME-SET" . ,OPCODE-FRAME-SET)
			("JUMP-FORW" . ,OPCODE-JUMP-FORW)
			("JUMP-BACK" . ,OPCODE-JUMP-BACK))))

(define OPCODE-CODE-TO-NAME
	(alist->hash-table
		`(	(,OPCODE-FRAME-GET . "FRAME-GET")
			(,OPCODE-FRAME-SET . "FRAME-SET")
			(,OPCODE-JUMP-FORW . "JUMP-FORW")
			(,OPCODE-JUMP-BACK . "JUMP-BACK"))))

(define (opcode-pack code A B C)
	(if (> C #x000fffff)
		(langtype-error 'opcode-pack "C > 20 bits in opcode_pack()")
		(bitwise-ior code (arithmetic-shift A 8) (arithmetic-shift B 16)
			(arithmetic-shift C 32))))

; returns (code A B C)
(define (opcode-unpack packed)
	(list
		(bitwise-and packed #x00ff)
		(bitwise-and (arithmetic-shift packed -8)  #x000000ff)
		(bitwise-and (arithmetic-shift packed -16) #x0000ffff)
		(bitwise-and (arithmetic-shift packed -32) #x000fffff)))

(define (opcode-name-to-code name)
	(if (hash-table-exists? OPCODE-NAME-TO-CODE name)
		(hash-table-ref OPCODE-NAME-TO-CODE name)
		(langtype-error 'opcode-name-to-code "No such opcode name: " name)))

(define (opcode-code-to-name code)
	(if (hash-table-exists? OPCODE-CODE-TO-NAME code)
		(hash-table-ref OPCODE-CODE-TO-NAME code)
		(langtype-error 'opcode-code-to-name "No such opcode number: " code)))

; *** SYNC THIS WITH C++ ***
(define MAX-CALLFRAME-SLOTS 32)

; DON'T USE THIS DIRECTLY -- use new-CallFrameData
(define-record CallFrameData outer data)
(define (new-CallFrameData)
	(make-CallFrameData '() (make-vector MAX-CALLFRAME-SLOTS (make-Void))))

(define (CallFrameData-findFrameUp frame levels)
	(let loop ((current frame)(nr levels))
		(if (null? frame)
			(langtype-error 'findFrameUp "Bad level number in findFrameUp"))

		(if (> nr 0)
			(loop (CallFrameData-outer current) (- nr 1))
			current)))

(define (CallFrameData-getFrameObj frame levels index)
	(if (or (< index 0) (>= index MAX-CALLFRAME-SLOTS))
		(langtype-error 'getFrameObj "Bad index in getFrameObj"))

	(let ((found (CallFrameData-findFrameUp frame levels)))
		(vector-ref (CallFrameData-data found) index)))

(define (CallFrameData-setFrameObj frame levels index obj)
	(if (or (< index 0) (>= index MAX-CALLFRAME-SLOTS))
		(langtype-error 'getFrameObj "Bad index in setFrameObj"))

	(let ((found (CallFrameData-findFrameUp frame levels)))
		(vector-set! (CallFrameData-data found) index obj)))

; holds a List as llist
(define-record Lambda llist)

; holds a List (llist) and a CallFrameData (outer)
(define-record BoundLambda llist outer)

; verbii string type
(define-record String value)

; verbii lists are really arrays, so use a vector
(define-record List objlist)

(define (new-lang-list) (make-List (make-dynvector 0 0)))

; get i'th object
(define (llist-get llist index)
	(if (or (< index 0) (>= index (dynvector-length (List-objlist llist))))
		(langtype-error 'List-get "Out of bounds in List")
		(dynvector-ref (List-objlist llist) index)))

; append an object
(define (llist-push-back llist obj)
	(dynvector-set! (List-objlist llist) (dynvector-length (List-objlist llist)) obj))

	(define (list->List lst) 
		;(print "LANGLIST FROM LIST: " lst)
		;(make List 'objlist (list->dynvector lst)))
		(make-List (list->dynvector lst)))

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
		((Null? obj) "<null>")
		((Void? obj) "<*void*>")
		((Lambda? obj)
			(fmtStackPrintObjlist (List-objlist (Lambda-llist obj)) "{" "}"))
		((BoundLambda? obj)
			(string-append "<bound " 
				(fmtStackPrintObjlist (List-objlist (BoundLambda-llist obj)) "{" "}") ">"))
		((Opcode? obj)
			(string-append "#op( " (opcode-code-to-name (Opcode-code obj))
				" " (fmtDisplay (Opcode-A obj)) " " (fmtDisplay (Opcode-B obj))
				" " (fmtDisplay (Opcode-C obj)) " )"))
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
		((Null? obj) "<null>")
		((Void? obj) "<*void*>")
		((Lambda? obj)
			(fmtDisplayObjlist (List-objlist (Lambda-llist obj)) "{" "}"))
		((BoundLambda? obj)
			(string-append "<bound "
				(fmtDisplayObjlist (List-objlist (BoundLambda-llist obj)) "{" "}") ">"))
		((Opcode? obj) (fmtStackPrint obj))
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
			(Opcode? obj)
			(Null? obj)
			(Void? obj)
			(Lambda? obj)
			(BoundLambda? obj))
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
