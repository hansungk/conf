#lang racket

(define (is-palindrome? n)
  (= n (reverse-number n)))

(define (digits n)
  (define (h tens d)
	(if (>= (quotient n tens) 1)
	  (h (* 10 tens) (add1 d))
	  d))
  (h 1 0))

(define (reverse-number n)
  (define (h tens np r)
	(if (tens < 1) r
	  (h (/ tens 10) (remainder np tens) (+ (* 10 r) (quotient np tens)))))
  (h (expt 10 (sub1 (digits n))) n 0))

(reverse-number 4321)
