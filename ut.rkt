#lang racket
(require rackunit "elliptic.rkt")

;;;;; pow ;;;;;
(check-eq? (pow 5 0) 1)
(check-eq? (pow 5 1) 5)
(check-eq? (pow 5 2) 25)
(check-eq? (pow 5 3) 125)
(check-eq? (pow 5 4) (* 5 125))

;;;;; pow-p ;;;;;
(check-eq? (pow-p 5 2 17) 8)

;;;;; factor-2 ;;;;;
(define (get-q q s) q)
(define (get-s q s) s)

(check-eq? (call-with-values (lambda () (factor-2 12)) get-q) 2)
(check-eq? (call-with-values (lambda () (factor-2 12)) get-s) 3)
(check-eq? (call-with-values (lambda () (factor-2 bc_p)) get-q) 0)
(check-eq? (call-with-values (lambda () (factor-2 bc_p)) get-s) bc_p)

;;;;; find-non-residue ;;;;;
(check-eq? (find-non-residue 17) 3)
(check-eq? (find-non-residue 19) 2)
(check-eq? (find-non-residue 23) 5)
;(check-eq? (find-non-residue 17) )
;(check-eq? (find-non-residue 17) )
;(check-eq? (find-non-residue 17) )

;;;;; assumption of pow/mod ;;;;;
(display "!") (newline)
(define p_probe '(17 19 23 101 103))
(for-each (lambda (x) (check-eq? (modulo (pow 3 x) x) (pow-p 3 x x))) p_probe)

;bug fixed in pow_exp
(check-equal? (pow-p 12 6 13) 1)


;(check-eq? (pow_bitcoin 5 0) 1)
;(check-eq? (pow_bitcoin 5 1) 5)
;(check-eq? (pow_bitcoin 5 2) 25)
;(check-eq? (pow_bitcoin 5 3) 125)
;(check-eq? (pow_bitcoin 5 4) (* 5 125))
;
;(check-eq? (pow_exp 5 0) 1)
;(check-eq? (pow_exp 5 1) 5)
;(check-eq? (pow_exp 5 2) 25)
;(check-eq? (pow_exp 5 3) 125)
;(check-eq? (pow_exp 5 4) (* 5 125))
