;;;==================================================================================
;;; Deserializer
;;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;==================================================================================

; module header
(module deserializer *
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
(import (chicken io))

(import langtypes)

(define (replace-escapes text)
	(cond ; recursively apply until no more escapes remain
		((string-contains text "%32") => (lambda (n) (replace-escapes (string-replace text " " n (+ n 3)))))
		((string-contains text "%10") => (lambda (n) (replace-escapes (string-replace text "\n" n (+ n 3)))))
		((string-contains text "%13") => (lambda (n) (replace-escapes (string-replace text "\r" n (+ n 3)))))
		((string-contains text "%37") => (lambda (n) (replace-escapes (string-replace text "%" n (+ n 3)))))
		(else text)))

(define (deserialize-stream intr fileIn)
	(let ((line (string-trim-both (read-line fileIn))))
		(if (not (eof-object? line))
			(begin
				(print "Line: " line)
				(case (string-ref line 0)
					((#\i) (string->number (string-drop line 2)))
					((#\f) (make LangFloat 'value (string->number (string-drop line 2))))
					((#\n) (make LangNull))
					((#\s) 
						(if (>= (string-length line) 2) ; watch for empty string
							(make LangString 'value (replace-escapes (string-drop line 2)))
							(make LangString 'value "")))
					((#\y) (string-drop line 2)) ; verbii symbols are scheme strings
					((#\b)
						(if (equal? (string-drop line 2) "true")
							#t #f))
					((#\L)
						(let read-list ((nr (string->number (string-drop line 2))) (lst '()))
							(if (> nr 0) 
								(read-list (- nr 1) (append lst (list (deserialize-stream intr fileIn))))
								(begin
									; other ports don't do this (yet?!) -- remove VOIDs from list
									(set! lst (filter (lambda (obj) (not (LangVoid? obj))) lst))
									(list->LangList lst)))))
					((#\F) ; lambda - read list
						(let ((llist (deserialize-stream intr fileIn)))
							(if (LangList? llist)
								(make LangLambda 'llist llist)
								(raise "Expecting list after 'F' but got: " (fmtStackPrint objlist)))))
					((#\W) ; W name followed by list
						(let ((name (string-drop line 2))
								(llist (deserialize-stream intr fileIn)))
							(if (LangList? llist)
								(begin
									(print "DESERIALIZED WORD: " llist)
									(hash-table-set! (slot intr 'WORDS) name llist)
									(make LangVoid))
								(raise (string-append "Expecting list after 'W' but got: " (fmtStackPrint objlist))))))
					(else
						(print "Unknown char: " (string-ref line 0))))
			)
			(make LangVoid) ; eof
		)
	)
)

) ; end of module