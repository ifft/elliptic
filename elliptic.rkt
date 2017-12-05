#lang racket
(provide (all-defined-out))
;bitcoin's elliptic curve parameters
(define bc_E_a 0)
(define bc_E_b 7)
(define bc_p #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F)
(define bc_G #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798)
(define bc_G_long #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8)
(define bc_n #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141)

(define (pow_exp x n mod_prime)
  (let loop ([carry 1]
                    [result 1]
                    [r0 x]
                    [n n]
                    )
   (printf "pow_exp: ~a ~a ~a ~a ~n" carry result r0 n)
    (let ([carry (if (= carry 1) r0 (sqr carry))]
          [>> (lambda (x) (arithmetic-shift x -1))])
      (printf "pow_exp carry: ~a result: ~a r0: ~a n: ~a~n" carry result r0 n)
      (cond
        ((zero? n) result)
        (else
          (if (= (modulo n 2) 0)
            (loop carry result r0 (>> n))
            (loop carry (mod_prime (* carry result)) r0 (>> (sub1 n)))
            )
          )
        )
      )
    )
  )

(define (factor-2 x)
  (let factor-2_aux ([x x] [q 0])
    (cond
      ((= (modulo x 2) 0) (factor-2_aux (quotient x 2) (add1 q)))
      (else (values q x))
      )
    )
  )

(define (residue? x p)
 (= (pow-p x (/ (sub1 p) 2) p) 1)
)

(define (find-non-residue p)
  (let*
    (
     [pow      (lambda (x) (pow-p x (/ (sub1 p) 2) p))]
     [residue? (lambda (x) (residue? x p))]
     )
    (let find-non-residue_aux
      ([curr 2])
      (cond
        ((residue? curr) (find-non-residue_aux (add1 curr)))
        ; assert: x^((prime-1)/2) should be -1 otherwise
        ((not (= (pow curr) (sub1 p)))
         (error 'eulers-criterion-fail "~a^~a != -1" curr (/ (sub1 p) 2))
         )
        (else curr)
        )
      )
    )
  )

; Tonelli-Shanks algorithm
(define (mod-sqr n p)
  (define powp (lambda (x n) (pow-p x n p)))
  (define (find-t^2^i_eq_1 t)
    (let loop
      ([i 1])
      (cond
        ((= 1 (powp t (pow 2 i))) (begin (printf "OK! i = ~a~n" i) i))
        (else 
          (begin
            (printf "looping2 i = ~a~n" i)
            (loop (add1 i)))
          )
        )
      )
    )
  (unless (residue? n p) (error 'mod-sqr-non-residue "n: ~a p: ~a" n p))
  (let-values ([(s q) (factor-2 (sub1 p))])
              (when (= 0 (modulo q 2)) (error 'q-is-not-odd "~a" q))
              (let ([z (find-non-residue p)])
                (let loop ([M s]
                           [c (powp z q)]
                           [t (powp n q)]
                           [R (powp n (/ (add1 q) 2))]
                           )
                  (cond
                    ((= t 1) (sort `(,R ,(- p R)) <))
                    (else 
                      (let*
                        (
                         [i (find-t^2^i_eq_1 t)]
                         [b (powp c (pow 2 (- M i 1)))]
                         [M i]
                         [b^2 (powp b 2)]
                         [c b^2]
                         [t (modulo (* t b^2) p)]
                         [R (modulo (* R b) p)]
                         )
                        (loop M c t R)
                        )
                      )
                    )
                  )
                )
              )
  )

(define (pow_bitcoin x n) (pow_exp x n (lambda (x) (modulo x bc_p))))
(define (pow-p x n p) (printf "pow-p~n") (pow_exp x n (lambda (x) (modulo x p))))
(define (pow x n) (printf "pow~n") (pow_exp x n identity))
; XXX for testing    
(define (pow-p2 x p) (pow-p x (/ (sub1 p) 2) p))

(mod-sqr 8 17)
