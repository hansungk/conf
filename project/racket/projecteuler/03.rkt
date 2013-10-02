#lang racket

(define number 600851475143)

(define (largest-prime-factor n)
  (define (h r p)
	(cond [(>= r p) p]
		  [(zero? (remainder p r)) (h r (/ p r))]
		  [else (h (add1 r) p)]))
  (h 2 n))

(largest-prime-factor number)
