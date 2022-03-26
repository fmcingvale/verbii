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
(module errors *
(import scheme)
(import (chicken base))
(import (chicken syntax))
(import srfi-13)

; start of module code

(import langtypes)
(import simple-exceptions)

(define lang-error (lambda (wheresym . args)
	(raise ((make-exception 
		(string-append "[" (symbol->string wheresym) "] " 
			(string-join (map fmtDisplay args) " ")) 'lang-error) 'lang-error))))

) ; end of module
