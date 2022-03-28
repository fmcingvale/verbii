;;;==================================================================================
;;; native.scm - verbii builtin functions
;;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;==================================================================================

;; optimization settings (these are supposed to be global but not sure how they are
;; visible in different compilation units, so I'm including this header in all files
(declare (standard-bindings))
(declare (extended-bindings))

; module header
(module native *
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
;(import coops)
(import dyn-vector)
(import miscmacros) ; inc! dec! 
(import srfi-69) ; hash-tables
(import (chicken flonum)) ; flonum-print-precision

(import langtypes)
(import errors)
(import interpreter)

;; Reader interface: reader-open-string, reader-next
(define READER_WORDLIST '())

(define (reader-open-string intr str)
	(set! READER_WORDLIST (string-tokenize str))
	;(print "READER_WORDLIST: " READER_WORDLIST))
)

(define (reader-next intr)
	;(print "READER-NEXT FROM: " READER_WORDLIST)
	(if (not (null? READER_WORDLIST))
		(begin
			(push intr (car READER_WORDLIST))
			(set! READER_WORDLIST (cdr READER_WORDLIST)))
	
		(push intr (make-LangNull))))

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
	(if (<= (intr-LP intr) (intr-LP_MIN intr))
		(lang-error '>L "Locals overflow!"))
	(intr-LP-set! intr (- (intr-LP intr) 1))
	(memset intr (intr-LP intr) obj))
			
; ( -- obj from local stack )
(define (builtin-from-local intr)
	(if (>= (intr-LP intr) (intr-LP_EMPTY intr))
		(lang-error 'L> "Locals underflow!"))
	(push intr (memget intr (intr-LP intr)))
	(intr-LP-set! intr (+ (intr-LP intr) 1)))
		
(define (builtin-equal intr A B)
	(cond
		((LangNull? A) (push intr (LangNull? B)))
		((integer? A)
			(cond
				((integer? B) (push intr (= A B)))
				((LangFloat? B) (push intr (= A (value B))))
				(else (push intr #f))))
		((LangFloat? A)
			(cond
				((integer? B) (push intr (= (value A) B)))
				((LangFloat? B) (push intr (= (value A) (value B))))
				(else (push intr #f))))
		((LangString? A)
			(push intr (and (LangString? B) (string=? (value A) (value B)))))
		((string? A)
			(push intr (and (string? B) (string=? A B))))
		((boolean? A)
			(push intr (and (boolean? B) (eq? A B))))
		((LangLambda? A) (push intr #f)) ; lambdas never equal to anything else even themselves
		(else (lang-error '== "Don't know how to compare " A " and " B))))

(define (builtin-greater intr A B)
	; unlike above, both A and B have to be same (or comparable) types
	(cond
		((and (is-numeric? A) (is-numeric? B)) (push intr (> (value A) (value B))))
		((and (LangString? A) (LangString? B)) (push intr (string> (value A) (value B))))
		((and (LangSymbol? A) (LangSymbol? B)) (push intr (string> A B)))
		(else (lang-error '> "Don't know how to compare " A " and " B))))

(define (builtin-add intr A B)
	(cond
		((integer? A)
			(cond
				((integer? B) (push-int intr (+ A B)))
				((LangFloat? B) (push intr (make-lang-float (+ A (value B)))))
				(else (lang-error '+ "Don't know how to add " A " and " B))))
		((LangFloat? A)
			(cond
				((integer? B) (push intr (make-lang-float (+ (value A) B))))
				((LangFloat? B) (push intr (make-lang-float (+ (value A) (value B)))))
				(else (lang-error '+ "Don't know how to add " A " and " B))))
		((LangString? A)
			(if (LangString? B)
				(push intr (make-LangString (string-append (value A) (value B))))
				(lang-error '+ "Don't know how to add " A " and " B)))
		((string? A)
			(if (string? B)
				(push intr (string-append A B))
				(lang-error '+ "Don't know how to add " A " and " B)))
		((LangList? A)
			(if (LangList? B)
				(push intr (make-LangList 
					(list->dynvector (append (dynvector->list (LangList-objlist A)) (dynvector->list (LangList-objlist B))))))
				(lang-error '+ "Don't know how to add " A " and " B)))
		(else (lang-error '+ "Don't know how to add " A " and " B))))

(define (builtin-subtract intr A B)
	; only hande ints and floats here - preserve ints when possible
	(cond
		((integer? A)
			(cond
				((integer? B) (push-int intr (- A B)))
				((LangFloat? B) (push intr (make-lang-float (- A (value B)))))
				(else (lang-error '- "Don't know how to subtract " A " and " B))))
		((LangFloat? A)
			(cond
				((integer? B) (push intr (make-lang-float (- (value A) B))))
				((LangFloat? B) (push intr (make-lang-float (- (value A) (value B)))))
				(else (lang-error '- "Don't know how to subtract " A " and " B))))
		(else (lang-error '- "Don't know how to subtract " A " and " B))))
		
(define (builtin-multiply intr A B)
	; like subtract, only hande ints and floats here - preserve ints when possible
	(cond
		((integer? A)
			(cond
				((integer? B) (push-int intr (* A B)))
				((LangFloat? B) (push intr (make-lang-float (* A (value B)))))
				(else (lang-error '* "Don't know how to multiply " A " and " B))))
		((LangFloat? A)
			(cond
				((integer? B) (push intr (make-lang-float (* (value A) B))))
				((LangFloat? B) (push intr (make-lang-float (* (value A) (value B)))))
				(else (lang-error '* "Don't know how to multiply " A " and " B))))
		(else (lang-error '* "Don't know how to multiply " A " and " B))))
		
(define (builtin-divide intr A B)
	; results is ALWAYS float here
	(if (and (is-numeric? A) (is-numeric? B))
		(if (= (value B) 0)
			(lang-error '/ "Divide by zero")
			(push intr (make-lang-float (/ (value A) (value B)))))
		(lang-error '/ "Don't know how to divide " A " and " B)))
		
;; see C++ version for extensive comments on divmod
(define (builtin-divmod intr a b)
	(if (= b 0)
		(lang-error '/mod "Divide by zero"))

	(let* ((quot (floor (/ (abs a) (abs b))))
			(samesign (or (and (< a 0) (< b 0)) (and (>= a 0) (>= b 0))))
			(mod 0))
		(if samesign
			(set! mod (- a (* quot b)))
			(begin
				(set! mod (+ a (* quot b)))
				(set! quot (- 0 quot))))
		(push-int intr mod)
		(push-int intr quot)))

(define (is-sequence? obj)
	(or (LangString? obj) (LangSymbol? obj) (LangList? obj)))

(define (builtin-length intr obj)
	(if (is-sequence? obj)
		(push-int intr (len obj))
		(lang-error 'length "Object does not support 'length': " obj)))

(define (builtin-slice intr obj index nr)
	; ported from c#
	(if (not (is-sequence? obj))
		(lang-error 'slice "Object does not support slicing: " obj))

	(let ((objsize (len obj)))
		; adjust index & nr for negative & out of bounds conditions
		(if (< index 0) (set! index (+ objsize index))) ; count from end
		(if (or (< index 0) (>= index objsize)) ; out of bounds - return empty object
			(cond
				((LangString? obj) (push intr (make-LangString "")))
				((LangSymbol? obj) (push intr "")) ; another reason not to use builtin symbols!
				((LangList? obj) (push intr (new-lang-list))))
			; else, i can make a valid slice - adjust nr as needed
			(begin
				; nr < 0 means "copy all, starting at index"
				(if (< nr 0) (set! nr (- objsize index)))
				; past end of object, truncate
				(if (> (+ index nr) objsize) (set! nr (- objsize index)))
				; make slice
				(cond
					((LangString? obj) 
						(push intr (make-LangString (string-copy (value obj) index (+ index nr)))))
					((LangSymbol? obj)
						(push intr (string-copy obj index (+ index nr))))
					((LangList? obj)
						(let ((newlist (new-lang-list)))
							(let loop ((i index) (n nr))
								(if (> n 0)
									(begin
										(llist-push-back newlist (dynvector-ref (LangList-objlist obj) i))
										(loop (+ i 1) (- n 1)))))
							(push intr newlist))))))))

(define (builtin-unmake intr obj)
	(cond
		((or (LangString? obj) (LangSymbol? obj))
			(string-for-each (lambda (c) (push-int intr (char->integer c))) (value obj))
			(push-int intr (len obj)))
		((LangList? obj)
			(dynvector-for-each (lambda (i o) (push intr o)) (LangList-objlist obj))
			(push-int intr (len obj)))
		((LangLambda? obj)
			; like other ports, push a copy
			(push intr (make-LangList (dynvector-copy (LangList-objlist (lambda-llist obj))))))
		(else (lang-error 'unmake "Don't know how to unmake object: " obj))))

; pop N objects and return as list in stack order
(define (pop-nr-objs intr N)
	(let loop ((nr N) (lst '()))
		(if (> nr 0)
			(loop (- nr 1) (cons (pop intr) lst))
			lst)))

(define (builtin-make-string intr NR)
	; pop list of integers into lst and make string
	(push intr (make-LangString (apply string (map integer->char (pop-nr-objs intr NR))))))

(define (builtin-make-symbol intr NR)
	; pop list of integers into lst and make symbol
	(push intr (apply string (map integer->char (pop-nr-objs intr NR)))))

(define (builtin-make-word intr name)
	(let ((llist (popTypeOrFail intr LangList? "symbol" "make-word")))
		(if (intr-has-word intr name)
			(lang-error 'make-word "Trying to redefine name: " name))
		(hash-table-set! (WORDS intr) name llist)))

(define (builtin-append intr llist obj)
	; modify in place and push back on stack
	(llist-push-back llist obj)
	(push intr llist))

(define (LangString-or-Symbol? o) (or (LangString? o) (LangSymbol? o)))

(define (builtin-dumpword intr name)
	(if (not (hash-table-exists? (WORDS intr) name))
		(lang-error '.dumpword "No such word: " name))
	; like other ports, make a copy of list
	(let ((newlist (new-lang-list)))
		(dynvector-for-each 
			(lambda (i o) (llist-push-back newlist o)) 
				(LangList-objlist (hash-table-ref (WORDS intr) name)))
		(push intr newlist)))

; TODO -- some of the above can be lambdas here instead
(define N_BUILTINS
	(list
		; each as: <function-name> <arg-list-typestr> <function>
		(list "int?" 	(list '*) (lambda (intr obj) (push intr (integer? obj))))
		(list "float?" 	(list '*)  (lambda (intr obj) (push intr (LangFloat? obj))))
		(list "null?" 	(list '*)  (lambda (intr obj) (push intr (LangNull? obj))))
		(list "bool?" 	(list '*)  (lambda (intr obj) (push intr (boolean? obj))))
		(list "list?" 	(list '*)  (lambda (intr obj) (push intr (LangList? obj))))
		(list "string?" (list '*)  (lambda (intr obj) (push intr (LangString? obj))))
		(list "symbol?" (list '*)  (lambda (intr obj) (push intr (string? obj))))
		(list "null" 	'() (lambda (intr) (push intr (make-LangNull))))
		(list "reader-open-string" (list 's) reader-open-string)
		(list "make-list" (list 'i) builtin-make-list)
		(list "set!" 	(reverse (list '* 'i)) builtin-set)
		(list "SP" 		'() (lambda (intr) (push-int intr (intr-SP intr))))
		(list "SP!" 	(list 'i) (lambda (intr addr) (intr-SP-set! intr addr)))
		(list "LP" 		'() (lambda (intr) (push-int intr (intr-LP intr))))
		(list "LP!" 	(list 'i) (lambda (intr addr) (intr-LP-set! intr addr)))
		(list ">L" 		(list '*)  builtin-to-local)
		(list "L>" 		'() builtin-from-local)
		(list "reader-next" '() reader-next)
		(list "ref" 		(list 'i) builtin-ref)
		(list "==" 			(reverse (list '* '*)) builtin-equal)
		(list ">" 			(reverse (list '* '*)) builtin-greater)
		(list "+" 			(reverse (list '* '*)) builtin-add)
		(list "-" 			(reverse (list '* '*)) builtin-subtract)
		(list "*" 			(reverse (list '* '*)) builtin-multiply)
		(list "/" 			(reverse (list '* '*)) builtin-divide)
		(list "/mod" 		(reverse (list 'i 'i)) builtin-divmod)
		(list "slice" 		(reverse (list '* 'i 'i)) builtin-slice)
		(list "unmake"		(list '*)  builtin-unmake)
		(list "make-string" (list 'i) builtin-make-string)
		(list "make-word" 	(list 'y) builtin-make-word)
		(list "length" 		(list '*)  builtin-length)
		(list "append"	 	(reverse (list 'L '*)) builtin-append)
		(list "parse-int"	'() (lambda (intr) 
			(push-int intr (string->number (value 
				(popTypeOrFail intr LangString-or-Symbol? "string|symbol" "parse-int"))))))
		(list "parse-float" '() (lambda (intr) 
			(push intr (make-lang-float (string->number (value 
				(popTypeOrFail intr LangString-or-Symbol? "string|symbol" "parse-float")))))))
		(list "str" 		(list '*)  (lambda (intr obj) (push intr (make-LangString (fmtDisplay obj)))))
		(list "repr" 		(list '*)  (lambda (intr obj) (push intr (make-LangString (fmtStackPrint obj)))))
		(list "puts" 		(list 's) (lambda (intr obj) (display (value obj))))
		(list ".c" 			(list 'i) (lambda (intr obj) (display (integer->char obj))))
		(list "make-lambda" (list 'L) (lambda (intr llist) (push intr (make-LangLambda llist))))
		(list "make-symbol" (list 'i) builtin-make-symbol)
		(list ".dumpword" 	(list 'y) builtin-dumpword)
		(list "f.setprec" 	(list 'i) (lambda (intr i) (flonum-print-precision i)))
		(list "error"		(list 's) (lambda (intr s) (lang-error 'unknown s)))
	))

(set! BUILTINS (alist->hash-table N_BUILTINS #:test string=?))

) ; end of modules
