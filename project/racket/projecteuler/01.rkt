#lang racket

; (for*/list ([x (range 1 100)] [y (range 1 100)] [z (in-range 100)] #:when (= (+ (* x x) (* y y)) (* z z))) (list x y z)) 
;(apply + (for/list ([x (range 1 1000)] #:when (or (= (remainder x 3) 0) (= (remainder x 5) 0))) x))
(apply + (filter (lambda (x) (or (zero? (remainder x 3)) (zero? (remainder x 5)))) (range 1000)))
