;;;==================================================================================
;; 
;; Exceptions
;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;==================================================================================

;; optimization settings (these are supposed to be global but not sure how they are
;; visible in different compilation units, so I'm including this header in all files
(declare (standard-bindings))
(declare (extended-bindings))

; module header
(declare (unit errors))

(module errors *
(import scheme)
(import (chicken base))
(import (chicken syntax))
(import srfi-13)

; start of module code

(declare (uses langtypes))

(import langtypes)
(import simple-exceptions)

(define lang-error (lambda (wheresym msg . args)
	(raise ((make-exception 
		(string-append "*** " msg " "
			(string-join (map 
				(lambda (obj)
					(cond
						((string? obj) obj)
						(else (fmtDisplay obj)))
					;(fmtStackPrint obj)
						) 
						args) " ") " ***") 'lang-error) 'lang-error))))

) ; end of module
