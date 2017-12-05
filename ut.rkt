#lang racket
(require rackunit "elliptic.rkt")

;;;;; pow ;;;;;
(check-equal? (pow 5 0) 1)
(check-equal? (pow 5 1) 5)
(check-equal? (pow 5 2) 25)
(check-equal? (pow 5 3) 125)
(check-equal? (pow 5 4) (* 5 125))

;;;;; pow-p ;;;;;
(check-equal? (pow-p 5 2 17) 8)

;;;;; factor-2 ;;;;;
(let-values ([(s q) (factor-2 12)])
(check-equal? 2 s)
(check-equal? 3 q)
)
(let-values ([(s q) (factor-2 bc_p)])
(check-equal? 0 s)
(check-equal? bc_p q)
)

;;;;; find-non-residue ;;;;;
(check-equal? (find-non-residue 17) 3)
(check-equal? (find-non-residue 19) 2)
(check-equal? (find-non-residue 23) 5)
;(check-equal? (find-non-residue 17) )
;(check-equal? (find-non-residue 17) )
;(check-equal? (find-non-residue 17) )

;;;;; assumption of pow/mod ;;;;;
(display "!") (newline)
(define p_probe '(17 19 23 101 103))
(for-each (lambda (x) (check-equal? (modulo (pow 3 x) x) (pow-p 3 x x))) p_probe)

;bug fixed in pow_exp
(check-equal? (pow-p 12 6 13) 1)


; Tonelly-Shranks
(check-equal? (mod-sqr 8 17) 12)

; some tests generated for Tonelly-Shranks
(define primes '(13 17 19 23 101 103))

; square it then root it
(for-each 
  (check-equal? (mod-sqr (pow-p j 2 i) i) j)
  primes
  )


(check-equal? (mod-sqr j i) (pow-p j 2 i))

(for-each (lambda (x)
           (for-each (lambda (x)))
          )
 primes)

;(check-equal? (pow_bitcoin 5 0) 1)
;(check-equal? (pow_bitcoin 5 1) 5)
;(check-equal? (pow_bitcoin 5 2) 25)
;(check-equal? (pow_bitcoin 5 3) 125)
;(check-equal? (pow_bitcoin 5 4) (* 5 125))
;
;(check-equal? (pow_exp 5 0) 1)
;(check-equal? (pow_exp 5 1) 5)
;(check-equal? (pow_exp 5 2) 25)
;(check-equal? (pow_exp 5 3) 125)
;(check-equal? (pow_exp 5 4) (* 5 125))
