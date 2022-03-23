;;;==================================================================================
;; 
;; Exceptions
;;
;;; Copyright (c) 2022 Frank McIngvale, see LICENSE
;;;==================================================================================

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
