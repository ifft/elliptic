#lang racket
(require "utility.rkt")

(provide
  residue?
  find-non-residue
  sqrt-p
  inverse-of
  pow-p
  mul-p
  )


(define debug (make-parameter #t))
(define devel (make-parameter #t))
(define dprintf
 (lambda x
   (when (debug) (apply printf x))
 )
)

(define (binary-mul x n) (binary-op x n
                                    (lambda (carry) (* 2 carry))
                                    +
                                    identity
                                    0))

(define (binary-pow_ x n modfunc)
  (binary-op x n
             (lambda (x) (modfunc (sqr x)))
             *
             modfunc
             1
             )
  )

(define (pow-p x n p) (binary-pow_ x n
                                  (lambda (x) (modulo x p))
                                  )
  )

(define (binary-pow x n) (binary-pow_ x n identity))

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
         (error 'eulers-criterion-fail "~a^~a != -1 (p = ~a))" curr (/ (sub1 p) 2) p)
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
        ((= 1 (powp t (binary-pow 2 i))) i)
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
                         [b (powp c (binary-pow 2 (- M i 1)))]
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

(define (mul-p x y p) (modulo (binary-mul x y) p))

