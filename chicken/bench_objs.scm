
; try benchmarking some different ways of structuring data
;
; results:
;
; == interpreted, using 2 million objects ==
; (define-record) 8.2 seconds
; (define-record-type) 8.2 seconds
; (coops) 16.0 seconds
;
; So, interpreted, coops takes about 2x longer than define-record-*
;
; == compiled with -O3, 2 million objects ==
; (define-record)		0.9 seconds
; (define-recrd-type)	1.0 seconds
; (coops)				9.1 seconds
;
; So when compiled, coops takes 10x as long as define-record-*
; define-record-* gets about 10x speedup when compiled; coops gets < 2x
;

(define-record spoint x y z)

;(import srfi-9) ; define-record-type
; -- included in (chicken base)
(define-record-type rpoint
	(make-rpoint x y z)
	rpoint?
	(x rpoint-x set-rpoint-x!)
	(y rpoint-y set-rpoint-y!)
	(z rpoint-z set-rpoint-z!))

;(import srfi-43) ; vectors
; -- included in vector-lib
(import vector-lib)
(define NR_POINTS 2000000)

; test with (define-record) style
(define SPOINTS (make-vector NR_POINTS 0))

(define (test-with-spoints)
	(let fill-loop ((i 0))
		(if (< i NR_POINTS)
			(begin
				(vector-set! SPOINTS i (make-spoint i (* 2 i) (* 3 i)))
				(fill-loop (+ i 1)))))

	(vector-for-each (lambda (i pt) 
		(spoint-x-set! pt (+ (spoint-y pt) (spoint-z pt)))
		(spoint-y-set! pt (* (spoint-x pt) (spoint-z pt)))
		(spoint-z-set! pt (- (spoint-y pt) (spoint-x pt)))) SPOINTS))

; test with (define-record-type) style
(define RPOINTS (make-vector NR_POINTS 0))

(define (test-with-rpoints)
	(let fill-loop ((i 0))
		(if (< i NR_POINTS)
			(begin
				(vector-set! RPOINTS i (make-rpoint i (* 2 i) (* 3 i)))
				(fill-loop (+ i 1)))))

	(vector-for-each (lambda (i pt) 
		(set-rpoint-x! pt (+ (rpoint-y pt) (rpoint-z pt)))
		(set-rpoint-y! pt (* (rpoint-x pt) (rpoint-z pt)))
		(set-rpoint-z! pt (- (rpoint-y pt) (rpoint-x pt)))) RPOINTS))

; define with coops object
(import coops)

(define-class opoint () (
	(x accessor: OX initform: 0)
	(y accessor: OY initform: 0)
	(z accessor: OZ initform: 0)))

	(define OPOINTS (make-vector NR_POINTS 0))

(define (test-with-opoints)
	(let fill-loop ((i 0))
		(if (< i NR_POINTS)
			(begin
				(vector-set! OPOINTS i (make opoint 'x i 'y (* 2 i) 'z (* 3 i)))
				(fill-loop (+ i 1)))))

	(vector-for-each (lambda (i pt) 
		(set! (OX pt) (+ (OY pt) (OZ pt)))
		(set! (OY pt) (* (OX pt) (OZ pt)))
		(set! (OZ pt) (- (OY pt) (OX pt)))) OPOINTS))

; check timings
(print "(define-record)")
(time (test-with-spoints))
(print "(define-record-type)")
(time (test-with-rpoints))
(print "(coops)")
(time (test-with-opoints))



