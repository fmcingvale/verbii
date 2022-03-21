;;;==================================================================================
;;; native.scm - verbii builtin functions
;;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;==================================================================================

; module header
(module native *
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
(import srfi-69) ; hash-tables
; shorthand
(define slot slot-value)

(import langtypes)
(import interpreter)

;; Reader interface: reader-open-string, reader-next
(define READER_WORDLIST '())

(define (reader-open-string intr str)
	(set! READER_WORDLIST (string-tokenize str))
	(print "READER_WORDLIST: " READER_WORDLIST))

(define (reader-next intr)
	(if READER_WORDLIST
		(begin
			(push intr (car READER_WORDLIST))
			(set! READER_WORDLIST (cdr READER_WORDLIST)))
	
		(push intr (make LangNull))))

; ( xn .. x1 N -- list of N items )
(define (builtin-make-list intr N)
	(let loop ((nr N) (lst '()))
		(if (> nr 0)
			(loop (- nr 1) (cons (pop intr) lst))
			(push intr (list->LangList lst)))))

; ( obj addr -- store obj @ addr)
(define (builtin-set intr obj addr)
	;(print "IN SET!, STACK:" (reprStack intr))
	(memset intr addr obj))
	
(define (builtin-ref intr addr)
	;(print "IN SET!, STACK:" (reprStack intr))
	(push intr (memget intr addr)))

; ( obj -- to local stack )
(define (builtin-to-local intr obj)
	(if (<= (LP intr) (LP_MIN intr))
		(raise "Locals overflow!"))
	(dec! (LP intr))
	(memset intr (LP intr) obj))
			
; ( -- obj from local stack )
(define (builtin-from-local intr)
	(if (>= (LP intr) (LP_EMPTY intr))
		(lang-error "Locals underflow!"))
	(push intr (memget intr (LP intr)))
	(inc! (LP intr)))
		
(define (builtin-equal intr A B)
	(cond
		((LangNull? A) (push intr (LangNull? B)))
		((integer? A)
			(cond
				((integer? B) (push intr (equal? A B)))
				((LangFloat? B) (push intr (equal? (exact->inexact A) (value B))))
				(else (push intr #f))))
		((LangFloat? A)
			(cond
				((integer? B) (push intr (equal? (value A) (exact->inexact B))))
				((LangFloat? B) (push intr (equal? (value A) (value B))))
				(else (push intr #f))))
		((LangString? A)
			(push intr (and (LangString? B) (equal? (value A) (value B)))))
		((string? A)
			(push intr (and (string? B) (equal? A B))))
		((boolean? A)
			(push intr (and (boolean? B) (equal? A B))))
		(else (lang-error "Don't know how to compare " A " and " B))))

(define (builtin-add intr A B)
	(cond
		((integer? A)
			(cond
				((integer? B) (push intr (+ A B)))
				((LangFloat? B) (push intr (make LangFloat 'value (+ A (value B)))))
				(else (lang-error "Don't know how to add " A " and " B))))
		((LangFloat? A)
			(cond
				((integer? B) (push intr (make LangFloat 'value (+ (value A) B))))
				((LangFloat? B) (push intr (make LangFloat 'value (+ (value A) (value B)))))
				(else (lang-error "Don't know how to add " A " and " B))))
		((LangString? A)
			(if (LangString? B)
				(make LangString 'value (string-append (value A) (value B)))
				(lang-error "Don't know how to add " A " and " B)))
		((string? A)
			(if (string? B)
				(string-append A B)
				(lang-error "Don't know how to add " A " and " B)))
		((LangList? A)
			(if (LangList? B)
				(push intr (make LangList 'objlist 
					(list->dynvector (append (dynvector->list (objlist A)) (dynvector->list (objlist B))))))
				(lang-error "Don't know how to add " A " and " B)))
		(else (lang-error "Don't know how to add " A " and " B))))

(define (is-sequence? obj)
	(or (LangString? obj) (LangSymbol? obj) (LangList? obj)))

(define (builtin-length intr obj)
	(if (is-sequence? obj)
		(push intr (len obj))
		(lang-error "Object does not support 'length': " obj)))

(define (builtin-slice intr obj index nr)
	; ported from c#
	(if (not (is-sequence? obj))
		(lang-error "Object does not support slicing: " obj))

	(let ((objsize (len obj)))
		; adjust index & nr for negative & out of bounds conditions
		(if (< index 0) (set! index (+ objsize index))) ; count from end
		(if (or (< index 0) (>= index objsize)) ; out of bounds - return empty object
			(cond
				((LangString? obj) (push intr (make LangString 'value "")))
				((LangSymbol? obj) (push intr "")) ; another reason not to use builtin symbols!
				((LangList? obj) (push intr (make LangList))))
			; else, i can make a valid slice - adjust nr as needed
			(begin
				; nr < 0 means "copy all, starting at index"
				(if (< nr 0) (set! nr (- objsize index)))
				; past end of object, truncate
				(if (> (+ index nr) objsize) (set! nr (- objsize index)))
				; make slice
				(cond
					((LangString? obj) 
						(push intr (make LangString 'value (string-copy (value obj) index (+ index nr)))))
					((LangSymbol? obj)
						(push intr (string-copy obj index (+ index nr))))
					((LangList? obj)
						(let ((newlist (make LangList)))
							(let loop ((i index) (n nr))
								(if (> n 0)
									(push-back newlist (get obj i))
									(loop (+ i 1) (- n 1))))
							(push intr newlist))))))))

(define (builtin-unmake intr obj)
	(cond
		((or (LangString? obj) (LangSymbol? obj))
			(string-for-each (lambda (c) (push intr (char->integer c))) (value obj))
			(push intr (len obj)))
		((LangList? obj)
			(dynvector-for-each (lambda (i o) (push intr o)) (objlist obj))
			(push intr (len obj)))
		((LangLambda? obj)
			; like other ports, push a copy
			(push intr (make LangLambda 'objlist (dynvector-copy (objlist obj)))))
		(else (lang-error "Don't know how to unmake object: " obj))))

; pop N objects and return as list in stack order
(define (pop-nr-objs intr N)
	(let loop ((nr N) (lst '()))
		(if (> nr 0)
			(loop (- nr 1) (cons (pop intr) lst))
			lst)))

(define (builtin-make-string intr NR)
	; pop list of integers into lst and make string
	(push intr (make LangString 'value 
		(apply string (map integer->char (pop-nr-objs intr NR))))))

; TODO -- some of the above can be lambdas here instead
(define N_BUILTINS
	(list
		; each as: <function-name> <arg-list-typestr> <function>
		(list "int?" "*" (lambda (intr obj) (push intr (integer? obj))))
		(list "null?" "*" (lambda (intr obj) (push intr (LangNull? obj))))
		(list "bool?" "*" (lambda (intr obj) (push intr (boolean? obj))))
		(list "list?" "*" (lambda (intr obj) (push intr (LangList? obj))))
		(list "string?" "*" (lambda (intr obj) (push intr (LangString? obj))))
		(list "symbol?" "*" (lambda (intr obj) (push intr (string? obj))))
		(list "reader-open-string" "s" reader-open-string)
		(list "make-list" "i" builtin-make-list)
		(list "set!" "*i" builtin-set)
		(list "SP" "" (lambda (intr) (push intr (SP intr))))
		(list "SP!" "i" (lambda (intr addr) (set! (SP intr) addr)))
		(list "LP" "" (lambda (intr) (push intr (LP intr))))
		(list ">L" "*" builtin-to-local)
		(list "L>" "" builtin-from-local)
		(list "reader-next" "" reader-next)
		(list "ref" "i" builtin-ref)
		(list "==" "**" builtin-equal)
		(list "+" "**" builtin-add)
		(list "slice" "*ii" builtin-slice)
		(list "unmake" "*" builtin-unmake)
		(list "make-string" "i" builtin-make-string)
		(list "length" "*" builtin-length)
	))

(set! BUILTINS (alist->hash-table N_BUILTINS #:test equal?))

) ; end of modules
