#lang racket
(require rackunit "elliptic.rkt")
(parameterize ([debug (if (getenv "DEBUG") #t #f)])
(define primes '(13 17 19 23 101 103))

;;;;; crash test ;;;;;
(x)
(x)
(x)

;;;;; add-point 1 ;;;;;
; TODO find test cases elsewhere
(check-equal? (point-x (add-point bc_G bitcoin-curve)) 89565891926547004231252920425935692360644145829622209833684329913297188986597)
(check-equal? (point-y (add-point bc_G bitcoin-curve)) 12158399299693830322967808612713398636155367887041628176798871954788371653930)
;;;;; add-point 2 ;;;;;
; TODO find test cases elsewhere

;;;;; binary mul ;;;;;
(for ([a (in-range 1 10)]
      )
     (for ([b (in-range 1 10)])
      (dprintf "~ax~a = ~a~n" a b (binary-mul a b))
      (check-equal? (binary-mul a b) (* a b))
          )
     )

;;;;; inverse-of ;;;;;
(for-each (lambda (p)
            (for ([i (in-range 1 p)])
             (begin
             (dprintf "inverse-of ~a ~a~n" i p)
                 (check-equal? (mul-p i (inverse-of i p) p) 1
                               )
             )
                 )
            )
          primes
          )

;;;;; euclid++ ;;;;;
(let-values (
             [(a b c) (euclid++ 240 46)]
             )
            (check-equal? a 2)
            (check-equal? b -9)
            (check-equal? c 47)
            )

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

;;;;; assumption of pow/mod ;;;;;

;bug fixed in pow_exp
(check-equal? (pow-p 12 6 13) 1)


; Tonelly-Shranks
(check-equal? (sqrt-p 8 17) '(5 12))

; some tests generated for Tonelly-Shranks

; square it then root it
(for-each 
  (lambda (p)
    (let loop ([i 2])
      (when (< i p)
        (begin
          (check-equal? (sqrt-p (pow-p i 2 p) p) (sort `(,i ,(- p i)) <))
          (loop (add1 i))
          )
        )
      )
    )
  primes
  )

; calc-y
(check-equal? (car (calc-y bc_G_x bitcoin-curve)) bc_G_y)

;;;;; scalar-mul ;;;;;
;mul with 0
(check-equal? (scalar-mul bc_G 0 bitcoin-curve) (point 0 0))
;mul up-to 128, check with repeated addition operator

)
