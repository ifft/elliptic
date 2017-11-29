#lang racket
(require rackunit "elliptic.rkt")
(check-eq? (pow_exp 1 5 0) 1)
(check-eq? (pow_exp 1 5 1) 5)
(check-eq? (pow_exp 1 5 2) 25)
(check-eq? (pow_exp 1 5 3) 125)
(check-eq? (pow_exp 1 5 4) (* 5 125))

;(check-eq? (pow_exp 5 0) 1)
;(check-eq? (pow_exp 5 1) 5)
;(check-eq? (pow_exp 5 2) 25)
;(check-eq? (pow_exp 5 3) 125)
;(check-eq? (pow_exp 5 4) (* 5 125))
