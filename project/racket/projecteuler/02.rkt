#lang racket

(define (h i j val)
  (if (> val 4000000)
	val
	(h j (+ i j) (if (even? i) (+ i val) val))))

(h 1 2 0)
