#lang racket
(require sha)
(require (for-syntax racket/stxparam racket))
;(require racket/random)
(provide (all-defined-out))
;bitcoin's elliptic curve parameters
(define bc_E_a 0)
(define bc_E_b 7)
(define bc_n #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141)
(define bc_p #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F)
(define bc_G_long #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8)

(define bc_G_x #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798)
(define bc_G_y #x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8)

(define debug (make-parameter #t))
(define devel (make-parameter #t))
(define dprintf
 (lambda x
   (when (debug) (apply printf x))
 )
)

(define-syntax (with-helper-funcs stx)
  (syntax-case stx ()
               [(with-helper-funcs curve body0 body ...)
                (with-syntax (
                              [mod (datum->syntax #'with-helper-funcs 'mod)]
                              [inv(datum->syntax #'with-helper-funcs 'inv)]
                              [sqr   (datum->syntax #'with-helper-funcs 'sqr)]
                              [pow   (datum->syntax #'with-helper-funcs 'pow)]
                              [sqrt  (datum->syntax #'with-helper-funcs 'sqrt)]
                              [+     (datum->syntax #'with-helper-funcs '+)]
                              [+p    (datum->syntax #'with-helper-funcs '+p)]
                              [-     (datum->syntax #'with-helper-funcs '-)]
                              [*     (datum->syntax #'with-helper-funcs '*)]
                              [*p    (datum->syntax #'with-helper-funcs '*p)]
                              )
                             #'(let* (
                                      [mod   (lambda (x) (modulo x (elliptic-curve-p curve)))]
                                      [inv   (lambda (x) (inverse-of x (elliptic-curve-p curve)))]
                                      [sqr   (lambda (x) (pow-p x 2 (elliptic-curve-p curve)))]
                                      [pow   (lambda (x n) (pow-p x n (elliptic-curve-p curve)))]
                                      [sqrt  (lambda (x) (sqrt-p x (elliptic-curve-p curve)))]
                                      [+     (lambda (x y) (mod (+ x y)))]   
                                      [+p    (case-lambda ((p) (add-point p (elliptic-curve-p curve))) ((p q) (add-point p q (elliptic-curve-p curve))))]
                                      [-     (lambda (x y) (mod (- x y)))]
                                      [*     (lambda (x y) (mod (* x y)))]
                                      [*p    (lambda (x n) (scalar-mul x n (elliptic-curve-p curve)))]
                                      )

                                 body0
                                 body
                                 ...
                                 )
                             )
                ]
               )
  )


(define (test)
  (with-helper-funcs bitcoin-curve
                     (mod 5)
                     )
  )

(struct point (x y) #:transparent)
(define bc_G (point bc_G_x bc_G_y))

(struct elliptic-curve (a b p G n) #:transparent)
(define bitcoin-curve (elliptic-curve bc_E_a bc_E_b bc_p bc_G bc_n))
(define test-curve (elliptic-curve 2 3 97 (point 3 6) 0))

(define (binary-op x n inc-carry op-step mod_prime unity_element) ; TODO eliminate mod_prime
  (let loop (
             [firstrun #t]
             [carry 1]
             [result unity_element]
             [r0 x]
             [n n]
             )
    ;(dprintf "binary-op ~a ~a ~a ~a ~n" carry result r0 n)
    (let* ([stop? (zero? n)]
           [carry (if stop? 0 (if firstrun r0 (inc-carry carry)))]
           [>> (lambda (x) (arithmetic-shift x -1))])
      ;(dprintf "binary-op carry: ~a result: ~a r0: ~a n: ~a~n" carry result r0 n)
      (cond
        (stop? result)
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
; inc-carry: add-point carry carry
; op-step(c,res): add-point carry result
; r0 = P
(define (binary-mul x n) (binary-op x n
                                    (lambda (carry) (* 2 carry))
                                    +
                                    identity
                                    0))

(define (_binary-pow x n modfunc)
  (binary-op x n
             (lambda (x) (modfunc (sqr x)))
             *
             modfunc
             1
             )
  )

(define (binary-pow x n) (_binary-pow x n identity))

(define (binary-pow-modp x n p) (_binary-pow x n
                                 (lambda (x) (modulo x p))
                                 ))

(define (scalar-mul p n curve)
  (binary-op p n
             (lambda (carry)
              (dprintf "inc-carry~n")
               (add-point carry curve)
               )
             (lambda (carry result)
              (dprintf "carry-op result~a~np: ~a~n" result p)
               (if (equal? result (point 0 0))
                 carry
                 (add-point result carry curve)
                 )
               )
             identity
             (point 0 0)
             )
  )

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
    (let loop
      ([curr 2])
      (cond
        ((residue? curr) (loop (add1 curr)))
        ; assert: x^((prime-1)/2) should be -1 otherwise
        ((not (= (pow curr) (sub1 p)))
         (error 'eulers-criterion-fail "~a^~a != -1" curr (/ (sub1 p) 2))
         )
        (else curr)
        )
      )
    )
  )

(define (negate point curve)
 (point (point-x point)
  (modulo (- (elliptic-curve-p curve)
             (point-y point))
          (elliptic-curve-p curve))
  )
)

; Tonelli-Shanks algorithm
(define (sqrt-p n p)
  (define powp (lambda (x n) (pow-p x n p)))
  (define (find-t^2^i_eq_1 t)
    (let loop
      ([i 1])
      (cond
        ((= 1 (powp t (bpow 2 i))) i)
        (else 
          (loop (add1 i)))
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
                         [b (powp c (bpow 2 (- M i 1)))]
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
   (dprintf "q/r ~a ~a~n" (vector-ref r 0) (vector-ref r 1))
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
  (dprintf "inverse-of ~a ~a~n" x p)
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

(define (bpow x n) (binary-pow x n))
; XXX for testing    
(define (pow-p2 x p) (pow-p x (/ (sub1 p) 2) p))

(define (calc-y x curve)
 (with-helper-funcs curve
  (let* ([a (elliptic-curve-a curve)]
         [b (elliptic-curve-b curve)]
         [p (elliptic-curve-p curve)]
         [pow3 (lambda (x) (pow-p x 3 p))]
         )
    (sqrt (+ (+ (pow3 x) (* a x)) b))
    )
  )
  )

(define calc-m 
  (let ([calc-m-base (lambda (curve)
                       (let ([p (elliptic-curve-p curve)])
                         (let (
                               [mod (lambda (x) (modulo x p))]
                               [sqr (lambda (x) (pow-p x 2 p))]
                               [a   (elliptic-curve-a curve)]
                               [inv (lambda (x) (inverse-of x p))]
                               )
                           (case-lambda
                             ((pointP)
                              (dprintf "c-l 1~n")
                              (mod (* (+ (* 3
                                            (sqr (point-x pointP)))
                                         a)
                                      (inv (mod (* 2 (point-y pointP))))
                                      )
                                   )
                              )
                             ((pointP pointQ)
                              (dprintf "c-l 2~n")
                              (mod (* (- (point-y pointP) (point-y pointQ))
                                      (inv (mod (- (point-x pointP) (point-x pointQ)))))

                                   )
                              )
                             )
                           )
                         )
                       )])
    (case-lambda
      ((pointP curve) ((calc-m-base curve) pointP))
      ((pointP pointQ curve) (dprintf "calc-m p: ~a~nq: ~a~n" pointP pointQ) ((calc-m-base curve) pointP pointQ))
      )
    )
  )

(define add-point
  (let (
        [add-point-aux (lambda (m pointP pointQ curve)
                         (with-helper-funcs curve
                                            (let* (
                                                   [result-x (- (- (sqr m) (point-x pointP)) (point-x pointQ))]
                                                   [result-y-1 (+ (- (elliptic-curve-p curve)
                                                                          (point-y pointP)
                                                                          )
                                                                       (* m (- (point-x pointP) result-x))
                                                                       )]
                                                   [result-y-2 (+ (- (elliptic-curve-p curve)
                                                                     (point-y pointQ)
                                                                     )
                                                                  (* m (- (point-x pointQ) result-x))
                                                                  )]
                                                   [derivedy1 (car  (calc-y result-x curve))]
                                                   [derivedy2 (cadr (calc-y result-x curve))]
                                                   )
                                              (unless (validate (point result-x result-y-1) curve) (error 'add-point-not-valid1)) ; TODO remove
                                              (unless (validate (point result-x result-y-2) curve) (error 'add-point-not-valid2)) ; TODO remove
                                              (unless (equal? result-y-1 result-y-2) (error 'lofasz)) ; TODO remove
                                              (dprintf "add-point~nderived:~n~a~n~a~ncalcd:~n~a~n~a~n"
                                                       derivedy1 derivedy2 result-y-1 result-y-2
                                                       )
                                              (point result-x result-y-1)
                                              )
                                            )
                         )
                       ]
        )
    (case-lambda
      ((pointP pointQ curve)
       (add-point-aux (calc-m pointP pointQ curve) pointP pointQ curve)
       )
      ((pointP curve)
       (add-point-aux (calc-m pointP curve) pointP pointP curve)
       )
      )
    )
  )

(define (calc-m-old pointP pointQ curve)
  (let ([p   (elliptic-curve-p curve)])
    (let (
          [mod (lambda (x) (modulo x p))]
          [sqr (lambda (x) (pow-p x 2 p))]
          [a   (elliptic-curve-a curve)]
          [inv (lambda (x) (inverse-of x p))]
          )
      (if (and (equal? (point-x pointP) (point-x pointQ))
                (equal? (point-y pointP) (point-y pointQ)))
        (mod (* (+ (* 3
                      (sqr (point-x pointP)))
                   a)
                (inv (* 2 (point-y pointP)))
                )
             )
        (mod (* (- (point-y pointP) (point-y pointQ))
                (inv (- (point-x pointP) (point-x pointQ))))

             )
        )
      )
    )
  )

(define (random16bit)
 (random #xFFFF)
)

(define (random32byte)
  (let loop ([i 16]
             [result 0]
             )
    (cond
      ((zero? i) result)
      (else
        (loop (sub1 i) (+ (arithmetic-shift result 16) (random16bit))
              )
        )
      )
    )
  )

(define (select-k curve)
  (let loop ([result (random32byte)])
    (cond
      ((and (< result (elliptic-curve-n curve))
            (not (zero? result)))
       result)
      (else (loop (random32byte)))
      )
    )
  )

(define (validate point curve)
  (let ([y12 (calc-y (point-x point) curve)])
    (and
      (or (equal? (point-y point) (car y12)) (equal? (point-y point) (cadr y12)))
      (< (point-x point) (elliptic-curve-p curve))
      (< (point-y point) (elliptic-curve-p curve))
      )
    )
  )

(define (sign message key curve)
  (let* ([mod (lambda (x) (modulo x (elliptic-curve-p curve)))]
         [mul (lambda (x n) (scalar-mul x n curve))]
         [inv (lambda (x) (inverse-of x (elliptic-curve-p curve)))]
         [select-k (lambda () (select-k curve))]
         [+ (lambda (a b) (mod (+ a b) (elliptic-curve-p curve)))]
         [* (lambda (a b) (mod (* a b) (elliptic-curve-p curve)))]
         )

    (let try-again ([k (select-k)])
      (let* ([P (mul (elliptic-curve-G curve) k)]
             [r (point-x P)]
             )
        (cond
          ((zero? r) (try-again (select-k)))
          (else 
            (let* ([k^-1 (inv k)]
                   [s (mod (* k^-1 (+ message (* r key))))])
              (cond
                ((zero? s) (try-again (select-k)))
                (else (values r s))
                )
              )
            )
          )
        )
      )
    )
  )

(define (verify message r signature curve)
  (let* ([mod (lambda (x) (modulo x (elliptic-curve-p curve)))]
         [mul (lambda (x n) (scalar-mul x n curve))]
         [inv (lambda (x) (inverse-of x (elliptic-curve-p curve)))]
         [+ (lambda (a b) (mod (+ a b) (elliptic-curve-p curve)))]
         [+p (lambda (a b) (add-point a b curve))]
         [* (lambda (a b) (mod (* a b)))]
         )
    (let ([s^-1 (inv signature)])
      (let* ([u1 (* s^-1 message)]
             [u2 (* s^-1 r)]
             [P (+p (mul (elliptic-curve-G curve) u1) (mul (elliptic-curve-G curve) message))]
             )
       (equal? r (point-x P))
        )
      )
    )
  )
    ;Calculate the integer u1= s−1*z mod n
    ;Calculate the integer u2=s−1*r mod n
    ;Calculate the point P = u1*G + u2 * msg
    ;The signature is valid only if r=xP mod n

; to be removed. these are here only for testing
(when (devel) (random-seed 42))
(number->string (random32byte) 16)
(define (sign-point x curve)
  (cond
    ((zero? (point-y x)) "0")
    ((<= (point-y x) (arithmetic-shift (elliptic-curve-p curve) -1)) "-")
    (else "+")
    )
  )

(define x
  (let ([firstrun #t]
        [prev-result 0]
        )
    (lambda ()
     (dprintf "x prev-result: ~a~n" prev-result)
     (dprintf "x firstrun ~a~n" firstrun)
      (if firstrun
        (begin
          (set! prev-result (add-point bc_G bitcoin-curve))
          (set! firstrun #f)
          )
        (set! prev-result (add-point prev-result bc_G bitcoin-curve))
        )
      (dprintf "x:  ~a~ny: ~a~a~n" (point-x prev-result) (sign-point prev-result bitcoin-curve) (point-y prev-result))
      )
    )
  )


