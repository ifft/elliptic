#lang racket
(require rackunit "elliptic-arith.rkt")
(require "utility.rkt")
(require "bitcoin-curve.rkt")
(require "modulo-arith.rkt")
(parameterize ([debug (if (getenv "DEBUG") #t #f)])
(define primes '(13 17 19 23 101 103))

;;;;; negate ;;;;;
;(check-equal? (negate bc_G bitcoin-curve) (point 0 0))

;;;;; crash test ;;;;;
;(x)
;(x)
;(x)

;;;;; add-point 1 ;;;;;
; TODO find test cases elsewhere
(check-equal? (point-x (add-point bc_G bitcoin-curve)) 89565891926547004231252920425935692360644145829622209833684329913297188986597)
(check-equal? (point-y (add-point bc_G bitcoin-curve)) 12158399299693830322967808612713398636155367887041628176798871954788371653930)

; calc-y
(check-equal? (car (calc-y bc_G_x bitcoin-curve)) bc_G_y)

;;;;; scalar-mul ;;;;;
;mul with 0
(check-equal? (scalar-mul bc_G 0 bitcoin-curve) (point 0 0))

;check scalar-mul against repeated additions
(for ([i (in-range 1 33)])
     (let ([rep
             (let repeated-add ([result bc_G]
                                [n i]
                                [firstrun? #t]
                                )
               (cond
                 ((equal? n 1) result)
                 (else (repeated-add
                         (if firstrun?
                           (add-point result bitcoin-curve)
                           (add-point result bc_G bitcoin-curve)
                           )
                         (sub1 n)
                         #f
                         )
                       )
                 )
               )])
       (printf "checking scalar-mul ~a/~a~n" i 32)
       (check-equal? (scalar-mul bc_G i bitcoin-curve) rep)
       )
     )

)

;;;;; check if common secret could be calculated ;;;;;
(with-helper-funcs
  bitcoin-curve
  (let (
        [alicepriv 36281381264312944114660412731460996624433877351060966911175032316224544298925]
        [bobpriv 109377126302409292064350767041684127893334715379768434484720197907651178515954]
        )
    (let (
          [alicepub (* bc_G alicepriv)] 
          [bobpub   (* bc_G bobpriv)]
          )
      (check-equal? (* bobpub alicepriv) (* alicepub bobpriv))
      )                 
    )
  )
