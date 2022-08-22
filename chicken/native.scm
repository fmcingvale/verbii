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
(declare (unit native))

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
(import (chicken time)) ; current-process-milliseconds

(declare (uses langtypes))
(declare (uses errors))
(declare (uses interpreter))
(declare (uses deserializer))

(define ALLOW_OVERWRITING_WORDS #f)
(define EXIT_ON_EXCEPTION #t)
(define STACKTRACK_ON_EXCEPTION #t)

(define FP_STDOUT '()) ; null for normal stdout, non-null for file

(define STARTUP_TIME (current-process-milliseconds))

; ( xn .. x1 N -- list of N items )
(define (builtin-make-list intr N)
	(let loop ((nr N) (lst '()))
		(if (> nr 0)
			(loop (- nr 1) (cons (pop intr) lst))
			(push intr (list->List lst)))))

; ( obj addr -- store obj @ addr)
(define (builtin-set intr obj addr)
	;(print "IN SET!, STACK:" (reprStack intr))
	(memset intr addr obj))
	
(define (builtin-ref intr addr)
	;(print "IN SET!, STACK:" (reprStack intr))
	(push intr (memget intr addr)))

(define (test-equal-lists A B)
	(let loop ((i 0))
		(if (>= i (dynvector-length (List-objlist A)))
			#t
			(if (not (test-equal (dynvector-ref (List-objlist A) i)
								(dynvector-ref (List-objlist B) i)))
				#f 
				(loop (+ i 1))))))

; moved this out of line since its a long function
(import continuations)

(define (compare-dicts A B)
	(catch return
		(if (or (not (Dict? A)) (not (Dict? B)))
			(throw return #f))

		(if (not (equal? (len A) (len B)))
			(throw return #f))

		(hash-table-walk (Dict-table A)
			(lambda (k v)
				; if B is missing key, then not equal
				(if (not (hash-table-exists? (Dict-table B) k))
					(throw return #f))

				; if A[k] != B[k] then not equal
				(if (not (test-equal v (hash-table-ref (Dict-table B) k)))
					(throw return #f))))

		; all entries equal
		#t))

(define (test-equal A B)
	(cond
		((Null? A) (Null? B))
		((integer? A)
			(cond
				((integer? B) (= A B))
				((Float? B) (= A (value B)))
				(else #f)))
		((Float? A)
			(cond
				((integer? B) (= (value A) B))
				((Float? B) (= (value A) (value B)))
				(else #f)))
		((String? A)
			(and (String? B) (string=? (value A) (value B))))
		((string? A)
			(and (string? B) (string=? A B)))
		((boolean? A)
			(and (boolean? B) (eq? A B)))
		((Lambda? A) #f) ; lambdas never equal to anything else even themselves
		((BoundLambda? A) #f) ; same as lambdas
		((Void? A) (Void? B))
		((List? A)
			(cond
				((not (List? B)) #f)
				((not (= (dynvector-length (List-objlist A)) (dynvector-length (List-objlist B)))) #f)
				(else (test-equal-lists A B))))
		((Dict? A) (compare-dicts A B))	
		((Opcode? A)
			(if (not (Opcode? B))
				#f 
				(and 
					(equal? (Opcode-code A) (Opcode-code B))
					(equal? (Opcode-A A) (Opcode-A B))
					(equal? (Opcode-B A) (Opcode-B B))
					(equal? (Opcode-C A) (Opcode-C B)))))
		(else (lang-error '== "Don't know how to compare " A " and " B))))

(define (builtin-equal intr A B)
	(push intr (test-equal A B)))

(define (test-greater A B)
	; unlike above, both A and B have to be same (or comparable) types
	(cond
		((and (is-numeric? A) (is-numeric? B)) (> (value A) (value B)))
		((and (String? A) (String? B)) (string> (value A) (value B)))
		((and (Symbol? A) (Symbol? B)) (string> A B))
		((and (List? A) (List? B))
			; see c++ notes
			(let ((nr (max (len A) (len B))))
				(let loop ((i 0))
					(if (< i nr)
						(cond
							((test-greater (dynvector-ref (List-objlist A) i) (dynvector-ref (List-objlist B) i))
								; A>B -> result is greater
								#t)
							((not (test-equal (dynvector-ref (List-objlist A) i) (dynvector-ref (List-objlist B) i)))
								; ! A>B and !A==B, so A<B -> result is less
								#f)
							(else
								; A==B so continue to next element
								(loop (+ 1 i))))
						; first nr elements are equal, determine result based on length:
						(cond
							((> (len A) (len B))
								; elements up to here are equal, but A is longer, so is greater
								#t)
							(else
								#f)))))) ; B is longer OR A==B, in either case, result is false
		(else (lang-error '> "Don't know how to compare " A " and " B))))

(define (builtin-greater intr A B)
	(push intr (test-greater A B)))

(define (builtin-add intr A B)
	(cond
		((integer? A)
			(cond
				((integer? B) (push-int intr (+ A B)))
				((Float? B) (push intr (make-lang-float (+ A (value B)))))
				(else (lang-error '+ "Don't know how to add " A " and " B))))
		((Float? A)
			(cond
				((integer? B) (push intr (make-lang-float (+ (value A) B))))
				((Float? B) (push intr (make-lang-float (+ (value A) (value B)))))
				(else (lang-error '+ "Don't know how to add " A " and " B))))
		((String? A)
			(if (String? B)
				(push intr (make-String (string-append (value A) (value B))))
				(lang-error '+ "Don't know how to add " A " and " B)))
		((string? A)
			(if (string? B)
				(push intr (string-append A B))
				(lang-error '+ "Don't know how to add " A " and " B)))
		((List? A)
			(if (List? B)
				(push intr (make-List 
					(list->dynvector (append (dynvector->list (List-objlist A)) (dynvector->list (List-objlist B))))))
				(lang-error '+ "Don't know how to add " A " and " B)))
		(else (lang-error '+ "Don't know how to add " A " and " B))))

(define (builtin-subtract intr A B)
	; only hande ints and floats here - preserve ints when possible
	(cond
		((integer? A)
			(cond
				((integer? B) (push-int intr (- A B)))
				((Float? B) (push intr (make-lang-float (- A (value B)))))
				(else (lang-error '- "Don't know how to subtract " A " and " B))))
		((Float? A)
			(cond
				((integer? B) (push intr (make-lang-float (- (value A) B))))
				((Float? B) (push intr (make-lang-float (- (value A) (value B)))))
				(else (lang-error '- "Don't know how to subtract " A " and " B))))
		(else (lang-error '- "Don't know how to subtract " A " and " B))))
		
(define (builtin-multiply intr A B)
	; like subtract, only hande ints and floats here - preserve ints when possible
	(cond
		((integer? A)
			(cond
				((integer? B) (push-int intr (* A B)))
				((Float? B) (push intr (make-lang-float (* A (value B)))))
				(else (lang-error '* "Don't know how to multiply " A " and " B))))
		((Float? A)
			(cond
				((integer? B) (push intr (make-lang-float (* (value A) B))))
				((Float? B) (push intr (make-lang-float (* (value A) (value B)))))
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
	(or (String? obj) (Symbol? obj) (List? obj)))

(define (builtin-length intr obj)
	(cond
		((is-sequence? obj)
			(push-int intr (len obj)))
		((Dict? obj)
			(push-int intr (hash-table-size (Dict-table obj))))
		(else
			(lang-error 'length "Object does not support 'length': " obj))))

(define (builtin-slice intr obj index nr)
	; ported from c#
	(if (not (is-sequence? obj))
		(lang-error 'slice "Object does not support slicing: " obj))

	(let ((objsize (len obj)))
		; adjust index & nr for negative & out of bounds conditions
		(if (< index 0) (set! index (+ objsize index))) ; count from end
		(if (or (< index 0) (>= index objsize)) ; out of bounds - return empty object
			(cond
				((String? obj) (push intr (make-String "")))
				((Symbol? obj) (push intr "")) ; another reason not to use builtin symbols!
				((List? obj) (push intr (new-lang-list))))
			; else, i can make a valid slice - adjust nr as needed
			(begin
				; nr < 0 means "copy all, starting at index"
				(if (< nr 0) (set! nr (- objsize index)))
				; past end of object, truncate
				(if (> (+ index nr) objsize) (set! nr (- objsize index)))
				; make slice
				(cond
					; TODO -- (substring) would be better than string-copy probably
					((String? obj) 
						(push intr (make-String (string-copy (value obj) index (+ index nr)))))
					((Symbol? obj)
						(push intr (string-copy obj index (+ index nr))))
					((List? obj)
						(let ((newlist (new-lang-list)))
							(let loop ((i index) (n nr))
								(if (> n 0)
									(begin
										(llist-push-back newlist (dynvector-ref (List-objlist obj) i))
										(loop (+ i 1) (- n 1)))))
							(push intr newlist))))))))

(define (builtin-unmake intr obj)
	(cond
		((or (String? obj) (Symbol? obj))
			(string-for-each (lambda (c) (push-int intr (char->integer c))) (value obj))
			(push-int intr (len obj)))
		((List? obj)
			(dynvector-for-each (lambda (i o) (push intr o)) (List-objlist obj))
			(push-int intr (len obj)))
		((Lambda? obj)
			; like other ports, push a deepcopy of objlist
			(push intr (deepcopy (Lambda-llist obj))))
		(else (lang-error 'unmake "Don't know how to unmake object: " obj))))

; pop N objects and return as list in stack order
(define (pop-nr-objs intr N)
	(let loop ((nr N) (lst '()))
		(if (> nr 0)
			(loop (- nr 1) (cons (pop intr) lst))
			lst)))

(define (popFloatOrInt intr wheresym)
	(let ((obj (pop intr)))
		(cond
			((integer? obj) obj)
			((Float? obj) (value obj))
			(else (lang-error wheresym "Expecting float or int but got: " obj)))))

(define (popInt intr wheresym)
	(let ((obj (pop intr)))
		(if (integer? obj)
			obj 
			(lang-error wheresym "Expecting integer but got: " obj))))

(define (popStringOrSymbol intr wheresym)
	(let ((obj (pop intr)))
		(cond
			((String? obj) obj)
			((string? obj) obj)
			(else (lang-error wheresym "Expecting string or symbol but got: " obj)))))

(define (popList intr wheresym)
	(let ((obj (pop intr)))
		(if (List? obj)
			obj 
			(lang-error wheresym "Expecting list but got: " obj))))

(define (popLambda intr wheresym)
	(let ((obj (pop intr)))
		(if (Lambda? obj)
			obj 
			(lang-error wheresym "Expecting lambda but got: " obj))))

(define (popOpcode intr wheresym)
	(let ((obj (pop intr)))
		(if (Opcode? obj)
			obj 
			(lang-error wheresym "Expecting opcode but got: " obj))))

(define (builtin-make-string intr NR)
	; pop list of integers into lst and make string
	(push intr (make-String (apply string (map integer->char (pop-nr-objs intr NR))))))

(define (builtin-make-symbol intr NR)
	; pop list of integers into lst and make symbol
	(push intr (apply string (map integer->char (pop-nr-objs intr NR)))))

(define (builtin-make-word intr name)
	(let ((llist (popTypeOrFail intr List? "symbol" "make-word")))
		(intr-define-word intr name llist ALLOW_OVERWRITING_WORDS)))
		
(define (builtin-append intr llist obj)
	; modify in place and push back on stack
	(llist-push-back llist obj)
	(push intr llist))

(define (builtin-extend intr)
	(let* ((src (popList intr 'extend))
			(dest (popList intr 'extend)))
		(dynvector-for-each (lambda (i elem) (llist-push-back dest elem)) (List-objlist src))
		(push intr dest)))

(define (builtin-wordlist intr)
	(let ((newlist (new-lang-list)))
		(hash-table-walk (intr-WORDS intr)
			(lambda (k v)
				(llist-push-back newlist k)))
		(push intr newlist)))

(define (String-or-Symbol? o) (or (String? o) (Symbol? o)))
	
(define (builtin-dumpword intr name)
	(if (not (intr-has-word intr name))
		(lang-error '.dumpword "No such word: " name))
	; like other ports, make a copy of list
	;(let ((newlist (new-lang-list)))
	;	(dynvector-for-each 
	;		(lambda (i o) (llist-push-back newlist o)) 
	;			(List-objlist (intr-lookup-word intr name)))
	;	(push intr newlist)))
	(push intr (deepcopy (intr-lookup-word intr name))))

(import (chicken file posix))

(define (builtin-file-read intr filename)
	(let* ((fileIn (file-open filename open/rdonly))
			(text (car (file-read fileIn (file-size fileIn)))))
		(push intr (make-String text))))

(define (builtin-put intr dest index obj)
	(cond
		((List? dest)
			(if (not (integer? index))
				(lang-error 'put "Index must be an integer"))
			(if (< index 0)
				(set! index (+ index (len dest)))) ; handle negative index
			(if (or (< index 0) (>= index (len dest)))
				(lang-error 'put "Index out of range in put")
				(begin
					(dynvector-set! (List-objlist dest) index obj)
					(push intr dest))))
		((Dict? dest)
			(if (not (String? index))
				(lang-error 'put "Key must be string in put")
				(begin
					(hash-table-set! (Dict-table dest) (value index) obj)
					(push intr dest))))
		(else
			(lang-error 'put "Object does not support put:" (fmtStackPrint dest)))))

(define (builtin-get intr obj index)
	(cond
		((Symbol? obj)
			(if (not (integer? index))
				(lang-error 'get "Index must be an integer"))
			(if (< index 0)
				(set! index (+ index (len obj)))) ; negative index adjustment
			(if (or (< index 0) (>= index (len obj)))
				(push intr (make-Void)) ; out of bounds == void
				(push intr (substring obj index (+ index 1)))))
		
		((String? obj)
			(if (not (integer? index))
				(lang-error 'get "Index must be an integer"))
			(if (< index 0)
				(set! index (+ index (len obj)))) ; negative index adjustment
			(if (or (< index 0) (>= index (len obj)))
				(push intr (make-Void)) ; out of bounds == void
				(push intr (make-String (substring (value obj) index (+ index 1))))))
		
		((List? obj)
			(if (not (integer? index))
				(lang-error 'get "Index must be an integer"))
			(if (< index 0)
				(set! index (+ index (len obj)))) ; negative index adjustment
			(if (or (< index 0) (>= index (len obj)))
				(push intr (make-Void)) ; out of bounds == void
				(push intr (dynvector-ref (List-objlist obj) index))))
					
		((Dict? obj)
			(if (not (String? index))
				(lang-error 'get "Key must be string in get")
				(if (not (hash-table-exists? (Dict-table obj) (value index)))
					(push intr (make-Void)) ; return void when missing key
					(push intr (hash-table-ref (Dict-table obj) (value index))))))
					
		(else
			(lang-error 'put "Object does not support put:" (fmtStackPrint obj)))))

(import (chicken io))

(define (builtin-prompt intr prompt)
	(display prompt)
	(let ((line (read-line)))
		(cond
			((string? line)
				(push intr (make-String line)))
			(else (push intr (make-Void))))))
	
(define (builtin-floor intr)
	(let ((obj (pop intr)))
		(cond
			((integer? obj) (push intr obj))
			((Float? obj) (push intr (inexact->exact (floor (value obj)))))
			(else (lang-error 'floor "Floor expects number but got:" obj)))))

(define (builtin-keys intr)
	(let ((dict (popTypeOrFail intr Dict? "dict" 'keys))
			(newlist (new-lang-list)))
		(hash-table-walk (Dict-table dict)
			(lambda (k v)
				(llist-push-back newlist (make-String k))))
		(push intr newlist)))

(define (builtin-make-opcode intr)
	(let* (	(C (popInt intr 'make-opcode))
			(B (popInt intr 'make-opcode))
			(A (popInt intr 'make-opcode))
			(name (popStringOrSymbol intr 'make-opcode)))

		(if (or (< A 0) (> A 255))
			(lang-error 'make-opcode "A must be [0-255] in make-opcode, got: " A))

		(if (or (< B 0) (> B 65535))
			(lang-error 'make-opcode "B must be [0-65535] in make-opcode, got: " B))

		(if (or (< C 0) (> C #x000fffff))
			(lang-error 'make-opcode "C must be [0-1048575] in make-opcode, got: " C))

		(push intr (make-Opcode (opcode-name-to-code name) A B C))))
		
(define (builtin-opcode-packed intr)
	(let ((op (popOpcode intr 'opcode-packed)))
		(push intr (opcode-pack (Opcode-code op) (Opcode-A op) (Opcode-B op) (Opcode-C op)))))

(define (builtin-bind-lambda intr)
	(let ((_lambda (popLambda intr 'bind-lambda)))
		(push intr (make-BoundLambda (Lambda-llist _lambda) (intr-framedata intr)))))

(define (builtin-pow intr)
	(let* ((y (popFloatOrInt intr 'pow))
			(x (popFloatOrInt intr 'pow)))
		(push intr (make-Float (expt x y)))))

(define (builtin-atan2 intr)
	(let* ((x (popFloatOrInt intr 'atan2))
			(y (popFloatOrInt intr 'atan2)))
		(print "ATAN" y x)
		(push intr (make-Float (atan y x)))))

(import (chicken bitwise))
(import (chicken time))
(import (chicken time posix))
(import (chicken file))
(import (chicken platform))
(import filepath)
(import (chicken process-context))

; TODO -- some of the above can be lambdas here instead
(define N_BUILTINS
	(list
		; each as: <function-name> <arg-list-typestr> <function>
		(list "int?" 	(list '*) (lambda (intr obj) (push intr (integer? obj))))
		(list "float?" 	(list '*)  (lambda (intr obj) (push intr (Float? obj))))
		(list "null?" 	(list '*)  (lambda (intr obj) (push intr (Null? obj))))
		(list "void?" 	(list '*)  (lambda (intr obj) (push intr (Void? obj))))
		(list "bool?" 	(list '*)  (lambda (intr obj) (push intr (boolean? obj))))
		(list "list?" 	(list '*)  (lambda (intr obj) (push intr (List? obj))))
		(list "string?" (list '*)  (lambda (intr obj) (push intr (String? obj))))
		(list "symbol?" (list '*)  (lambda (intr obj) (push intr (string? obj))))
		(list "lambda?" (list '*)  (lambda (intr obj) (push intr (Lambda? obj))))
		(list "bound-lambda?" (list '*)  (lambda (intr obj) (push intr (BoundLambda? obj))))
		(list "opcode?" (list '*)  (lambda (intr obj) (push intr (Opcode? obj))))
		(list "void" 	'() (lambda (intr) (push intr (make-Void))))
		(list "make-list" (list 'i) builtin-make-list)
		(list "set!" 	(reverse (list '* 'i)) builtin-set)
		(list "SP" 		'() (lambda (intr) (push-int intr (intr-SP intr))))
		(list "SP!" 	(list 'i) (lambda (intr addr) 
							(intr-SP-set! intr addr)
							(min-run-SP-set! intr (min (intr-SP intr) (min-run-SP intr)))))
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
		(list "extend"	 	'() builtin-extend)
		(list "parse-int"	'() (lambda (intr) 
			(push-int intr (string->number (value 
				(popTypeOrFail intr String-or-Symbol? "string|symbol" "parse-int"))))))
		(list "parse-float" '() (lambda (intr) 
			(push intr (make-lang-float (string->number (value 
				(popTypeOrFail intr String-or-Symbol? "string|symbol" "parse-float")))))))
		(list "str" 		(list '*)  (lambda (intr obj) (push intr (make-String (fmtDisplay obj)))))
		(list "repr" 		(list '*)  (lambda (intr obj) (push intr (make-String (fmtStackPrint obj)))))
		(list "puts" 		(list 's) 
			(lambda (intr obj) 
				(if (null? FP_STDOUT)
					(display (value obj)) ; write to normal output port
					(file-write FP_STDOUT (value obj)))))
		(list ".c" 			(list 'i) 
			(lambda (intr obj) 
				(if (null? FP_STDOUT)
					(display (integer->char obj)) ; write to normal output port
					(file-write FP_STDOUT (string (integer->char obj))))))
		(list "open-as-stdout" (list '*)
			(lambda (intr filename)
				(if (not (null? FP_STDOUT))
					(begin
						(file-close FP_STDOUT)
						(set! FP_STDOUT '())))
				(cond
					((Void? filename)) ; nothing, already reset to stdout above
					((String? filename)
						(set! FP_STDOUT (file-open (value filename) (+ open/wronly open/creat open/trunc))))
					(else
						(lang-error 'open-as-stdout "Expecting filename or obj but got:" filename)))))
				
		; must deepcopy list - see DESIGN-NOTES.md
		(list "make-lambda" (list 'L) (lambda (intr llist) (push intr (make-Lambda (deepcopy llist)))))
		(list "make-symbol" (list 'i) builtin-make-symbol)
		(list ".dumpword" 	(list 'y) builtin-dumpword)
		(list ".wordlist"   '() builtin-wordlist)
		(list "f.setprec" 	(list 'i) (lambda (intr i) (flonum-print-precision i)))
		(list "error"		(list 's) (lambda (intr s) (lang-error 'unknown s)))
		; as above, must deepcopy list
		(list "put" (reverse (list '* '* '*)) builtin-put)
		(list "get" (reverse (list '* '*)) builtin-get)
		(list "deepcopy" (list '*) (lambda (intr obj) (push intr (deepcopy obj))))
		(list "alloc" (list 'i) (lambda (intr nr) (push intr (allocate intr nr))))
		(list ",,del" (list 'y) (lambda (intr name) (intr-delete-word intr name)))
		(list "bit-and" (list 'i 'i) (lambda (intr a b) (push intr (bitwise-and (bitwise-and a b) #xffffffff))))
		(list "bit-or" (list 'i 'i) (lambda (intr a b) (push intr (bitwise-and (bitwise-ior a b) #xffffffff))))
		(list "bit-xor" (list 'i 'i) (lambda (intr a b) (push intr (bitwise-and (bitwise-xor a b) #xffffffff))))
		(list "bit-not" (list 'i) (lambda (intr a) (push intr (bitwise-and (bitwise-not a) #xffffffff))))
		(list "bit-shl" (list 'i 'i) (lambda (intr a n) (push intr (bitwise-and (arithmetic-shift a n) #xffffffff))))
		(list "bit-shr" (list 'i 'i) (lambda (intr a n) (push intr (bitwise-and (arithmetic-shift a (- 0 n)) #xffffffff))))

		(list "run-time" '() 
			(lambda (intr) (push intr (make-lang-float (/ (- (current-process-milliseconds) STARTUP_TIME) 1000.0)))))
		(list ",,new-dict" '() (lambda (intr) (push intr (new-Dict))))

		(list "file-exists?" (list 's) (lambda (intr name) (push intr (regular-file? name))))
		(list "file-mtime" (list 's) 
			(lambda (intr name)
				(if (regular-file? name)
					(push intr (vector-ref (file-stat name) 8))
					(lang-error 'file-mtime "No such file" name))))
		(list "deserialize" (list 's)
			(lambda (intr filename)
				(let ((fileIn (open-input-file filename)))
					(deserialize-stream intr fileIn))))
		(list "set-allow-overwrite-words" (list 'b) (lambda (intr b) (set! ALLOW_OVERWRITING_WORDS b)))
		(list "set-exit-on-exception" (list 'b) (lambda (intr b) (set! EXIT_ON_EXCEPTION b)))
		; stacktraces don't work yet on chicken though ...
		(list "set-stacktrace-on-exception" (list 'b) (lambda (intr b) (set! STACKTRACK_ON_EXCEPTION b)))
		(list "prompt" (list 's) builtin-prompt)

		(list "time-string" '() (lambda (intr) (push intr 
			(make-String (time->string (seconds->local-time (current-seconds)) "%Y-%m-%d %H:%M:%S")))))
		(list "floor" '() builtin-floor)

		(list "file-write" (list 's 's)
			(lambda (intr filename text)
				(let ((F (file-open filename (+ open/wronly open/creat open/trunc))))
					(file-write F text)
					(file-close F))))

		(list "file-append" (list 's 's)
			(lambda (intr filename text)
				(let ((F (file-open filename (+ open/wronly open/append open/creat))))
					(file-write F text)
					(file-close F))))
		(list "file-read"   (list 's) builtin-file-read)
		(list "file-delete" (list 's) 
			(lambda (intr filename)
				(if (regular-file? filename)
					(delete-file filename))))

		(list "sys-platform" '()
			(lambda (intr) (push intr (make-String (string-append "Chicken " (chicken-version))))))

		(list "depth" '()
			(lambda (intr) (push intr (- (intr-SP_EMPTY intr) (intr-SP intr)))))

		(list "keys" '() builtin-keys)

		(list "sin" '() (lambda (intr) (push intr (make-Float (sin (popFloatOrInt intr 'sin))))))
		(list "cos" '() (lambda (intr) (push intr (make-Float (cos (popFloatOrInt intr 'cos))))))
		(list "tan" '() (lambda (intr) (push intr (make-Float (tan (popFloatOrInt intr 'cos))))))
		(list "asin" '() (lambda (intr) (push intr (make-Float (asin (popFloatOrInt intr 'sin))))))
		(list "acos" '() (lambda (intr) (push intr (make-Float (acos (popFloatOrInt intr 'cos))))))
		(list "atan2" '() builtin-atan2)
		(list "sqrt" '() (lambda (intr) (push intr (make-Float (sqrt (popFloatOrInt intr 'sqrt))))))
		(list "log" '() (lambda (intr) (push intr (make-Float (log (popFloatOrInt intr 'log))))))
		(list "exp" '() (lambda (intr) (push intr (make-Float (exp (popFloatOrInt intr 'log))))))
		(list "pow" '() builtin-pow)

		(list "make-opcode" '() builtin-make-opcode)
		(list "opcode-packed" '() builtin-opcode-packed)
		(list "bind-lambda" '() builtin-bind-lambda)

		(list "file-pathsep" '() (lambda (intr) (push intr (make-String (make-string 1 (filepath:path-separator))))))
		(list "os-getcwd" '()  (lambda (intr) (push intr (make-String (current-directory)))))
	))

(set! BUILTINS (alist->hash-table N_BUILTINS #:test string=?))

; ================================================================
; Opcodes
;
; This is NOT an ideal place to define these, however, after having a lot
; of trouble with module imports & mututal dependencies, I gave up
; and moved these here ... would be nice to split these back into
; opcodes.scm later
; ================================================================

; opcode implementations
(define (opcode-FRAME-GET intr framedata levels index _unused)
	(if (null? framedata)
		(lang-error "FRAME-GET called with null frame"))

	(push intr (CallFrameData-getFrameObj framedata levels index)))

(define (opcode-FRAME-SET intr framedata levels index _unused)
	(if (null? framedata)
		(lang-error "FRAME-SET called with null frame"))

	(let ((obj (pop intr)))
		(CallFrameData-setFrameObj framedata levels index obj)))

(define (do-opcode-JUMP intr offset)
	(if (null? (intr-code intr))
		(lang-error "JUMP called with no code running?"))

	(let ((pos (+ (intr-codepos intr) offset)))
		(if (or (< pos 0) (>= pos (intr-code-length intr)))
			(lang-error "JUMP out of bounds")
			(intr-codepos-set! intr pos))))

(define (opcode-JUMP-FORW intr framedata A B offset)
	(do-opcode-JUMP intr offset))

(define (opcode-JUMP-BACK intr framedata A B offset)
	(do-opcode-JUMP intr (- 0 offset)))

; defined in interpreter.scm
; order must be same as numeric order of opcodes
(set! OPCODE-FUNCTIONS
	(list->vector
		(list opcode-FRAME-GET opcode-FRAME-SET opcode-JUMP-FORW opcode-JUMP-BACK)))
