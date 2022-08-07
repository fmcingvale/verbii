;;;==================================================================================
;;; Deserializer
;;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;==================================================================================

;; optimization settings (these are supposed to be global but not sure how they are
;; visible in different compilation units, so I'm including this header in all files
(declare (standard-bindings))
(declare (extended-bindings))

; module header
(module deserializer *
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
(import (chicken io))

(import interpreter)
(import langtypes)
(import errors)

(define (replace-escapes text)
	(cond ; recursively apply until no more escapes remain
		((string-contains text "%32") => (lambda (n) (replace-escapes (string-replace text " " n (+ n 3)))))
		((string-contains text "%09") => (lambda (n) (replace-escapes (string-replace text "\t" n (+ n 3)))))
		((string-contains text "%10") => (lambda (n) (replace-escapes (string-replace text "\n" n (+ n 3)))))
		((string-contains text "%13") => (lambda (n) (replace-escapes (string-replace text "\r" n (+ n 3)))))
		((string-contains text "%37") => (lambda (n) (replace-escapes (string-replace text "%" n (+ n 3)))))
		(else text)))

(define (deserialize-stream intr fileIn)
	(let ((line (read-line fileIn)))
		(if (not (eof-object? line))
			(begin
				(set! line (string-trim-both line))
				;(print "Line: " line)
				(case (string-ref line 0)
					((#\i) (string->number (string-drop line 2)))
					((#\f) (make-lang-float (string->number (string-drop line 2))))
					((#\b) (parse-bool (string-drop line 2)))
					((#\n) (make-Null))
					((#\o) 
						(let ((unpacked (opcode-unpack (string->number (string-drop line 2)))))
							(apply make-Opcode unpacked)))
					((#\s) 
						(if (>= (string-length line) 2) ; watch for empty string
							(make-String (replace-escapes (string-drop line 2)))
							(make-String "")))
					((#\y) (string-drop line 2)) ; verbii symbols are scheme strings
					((#\L)
						(let read-list ((nr (string->number (string-drop line 2))) (lst '()))
							(if (> nr 0) 
								(read-list (- nr 1) (append lst (list (deserialize-stream intr fileIn))))
								(begin
									; other ports don't do this (yet?!) -- remove VOIDs from list
									(set! lst (filter (lambda (obj) (not (Void? obj))) lst))
									(list->List lst)))))
					((#\F) ; lambda - read list
						(let ((llist (deserialize-stream intr fileIn)))
							(if (List? llist)
								(make-Lambda llist)
								(lang-error 'deserializer-list 
									"Expecting list after 'F' but got: " llist))))
					((#\W) ; W name followed by list
						(let ((name (string-drop line 2))
								(llist (deserialize-stream intr fileIn)))
							(if (List? llist)
								(begin
									;(print "DESERIALIZED WORD: " llist)
									(intr-define-word intr name llist #f)
									(make-Void))
								(lang-error 'deserializer-word 
									"Expecting list after 'W' but got: " llist))))
					(else
						(print "Unknown char: " (string-ref line 0))))
			)
			(make-Void) ; eof
		)
	)
)

) ; end of module