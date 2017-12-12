#lang racket
(provide (all-defined-out))
;bitcoin's elliptic curve parameters
(define bc_E_a 0)
(define bc_E_b 7)
(define bc_p #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F)
(define bc_G #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798)
(define bc_G_long #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8)
(define bc_n #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141)

(define debug (make-parameter #t))
(define dprintf
 (lambda x
   (when (debug) (apply printf x))
 )
)

(struct elliptic-curve-abp (a b p))
(struct elliptic-curve elliptic-curve-abp (G))
(define bitcoin-curve (elliptic-curve bc_E_a bc_E_b bc_p bc_G))

(define (binary-op x n inc-carry op-step mod_prime unity_element)
  (let loop (
             [firstrun #t]
             [carry 1]
             [result unity_element]
             [r0 x]
             [n n]
             )
    (dprintf "binary-op ~a ~a ~a ~a ~n" carry result r0 n)
    (let ([carry (if firstrun r0 (inc-carry carry))]
          [>> (lambda (x) (arithmetic-shift x -1))])
      (dprintf "binary-op carry: ~a result: ~a r0: ~a n: ~a~n" carry result r0 n)
      (cond
        ((zero? n) result)
        (else
          (if (= (modulo n 2) 0)
            (loop #f carry result r0 (>> n))
            (loop #f carry (mod_prime (op-step carry result)) r0 (>> (sub1 n)))
            )
          )
        )
      )
    )
  )

(define (binary-mul x n) (binary-op x n
                                    (lambda (carry) (* 2 carry))
                                    +
                                    identity
                                    0))

(define (_binary-pow x n modfunc)
  (binary-op x n
             sqr
             *
             modfunc
             1
             )
  )

(define (binary-pow x n) (_binary-pow x n identity))

(define (binary-pow-modp x n p) (_binary-pow x n
                                 (lambda (x) (modulo x p))
                                 ))

(define (factor-2 x)
  (let loop ([x x] [q 0])
    (cond
      ((= (modulo x 2) 0) (loop (quotient x 2) (add1 q)))
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
(define (sqrt-p n p)
  (define powp (lambda (x n) (pow-p x n p)))
  (define (find-t^2^i_eq_1 t)
    (let loop
      ([i 1])
      (cond
        ((= 1 (powp t (pow 2 i))) i)
        (else 
          (begin
            (loop (add1 i)))
          )
        )
      )
    )
  (unless (residue? n p) (error 'sqrt-p "n: ~a p: ~a" n p))
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

(define (euclid++ x p)
  (let loop (
             [r (list->vector (sort `(,x ,p) >))]
             [s #(1 0)]
             [t #(0 1)]
             )
   ;(dprintf "q/r ~a ~a~n" (vector-ref r 0) (vector-ref r 1))
    (let-values ([(quot rem) (quotient/remainder (vector-ref r 0) (vector-ref r 1))])
                (let* (
                       [r2 (- (vector-ref r 0) (* quot (vector-ref r 1)))]
                       [s2 (- (vector-ref s 0) (* quot (vector-ref s 1)))]
                       [t2 (- (vector-ref t 0) (* quot (vector-ref t 1)))]
                       )
                  (cond
                    ((zero? r2) (values
                        (vector-ref r 1)
                        (vector-ref s 1)
                        (vector-ref t 1)
                        ))
                    (else
                      (loop `#(,(vector-ref r 1), r2)
                            `#(,(vector-ref s 1), s2) 
                            `#(,(vector-ref t 1), t2))
                      )
                    )

                  )
                )
    )
  )

(define (inverse-of x p)
 ;(dprintf "inverse-of ~a ~a~n" x p)
  (let-values ([(gcd a b) (euclid++ x p)])
              (unless (equal? gcd 1) (error 'gcd-not-eq-1 "gcd(~a ~a) != 1"  x p))
              (unless (equal? (modulo (+ (* a x) (* b p)) p) gcd) 'euclid-wrong-result "(~a * ~a) + (~a * ~a) != ~a"
                a x b p gcd
                )
              (modulo b p)
              )
  )

(define (pow_bitcoin x n) (binary-pow-modp x n bc_p))

(define (pow-p x n p) (binary-pow-modp x n p))
(define (mul-p x y p) (modulo (binary-mul x y) p))

(define (pow x n) (binary-pow x n))
; XXX for testing    
(define (pow-p2 x p) (pow-p x (/ (sub1 p) 2) p))

(define (calc-y x curve)
  (let* ([a (elliptic-curve-abp-a curve)]
         [b (elliptic-curve-abp-b curve)]
         [p (elliptic-curve-abp-p curve)]
         [pow3 (lambda (x) (pow-p x 3 p))]
         [mod (lambda (x) (modulo x p))]
         [sqrt (lambda (x) (sqrt-p x p))]
        )
   (values a b p)
    (+ (pow3 x) (* a x) b)
    ;(sqrt (+ (pow3 x) (* a x) b))
    )
  )

