#lang racket
(require "modulo-arith.rkt")
(require "utility.rkt")

(provide 
  scalar-mul
  calc-y
  calc-m 
  add-point
  validate
  sign-point
  (struct-out point)
  (struct-out elliptic-curve)

  debug
  )

(define debug (make-parameter #t))
(define devel (make-parameter #t))

(struct point (x y) #:transparent)
(struct elliptic-curve (a b p G n bytelen) #:transparent)

(define (scalar-mul p n curve)
  (binary-op p n
             (lambda (carry)
               (add-point carry curve)
               )
             (lambda (carry result)
               (if (equal? result (point 0 0))
                 carry
                 (add-point result carry curve)
                 )
               )
             identity
             (point 0 0)
             )
  )

(define (calc-y x curve)
 (with-helper-funcs curve
  (let* ([a (elliptic-curve-a curve)]
         [b (elliptic-curve-b curve)]
         [p (elliptic-curve-p curve)]
         [pow3 (lambda (x) (pow-p x 3 p))]
         )
    (sqrt (+ (pow3 x) (* a x) b))
    )
  )
  )

(define calc-m 
  (let ([calc-m-base (lambda (curve)
                       (with-helper-funcs curve
                                          (let ([p   (elliptic-curve-p curve)]
                                                [a   (elliptic-curve-a curve)])
                                            (case-lambda
                                              ((pointP)
                                               (* (+ (* 3
                                                        (sqr (point-x pointP)))
                                                     a)
                                                  (inv (* 2 (point-y pointP)))
                                                  )
                                               )
                                              ((pointP pointQ)
                                               (* (- (point-y pointP) (point-y pointQ))
                                                  (inv (- (point-x pointP) (point-x pointQ))))
                                               )
                                              )
                                            )
                                          )
                       )])
    (case-lambda
      ((pointP curve) ((calc-m-base curve) pointP))
      ((pointP pointQ curve) ((calc-m-base curve) pointP pointQ))
      )
    )
  )

(define add-point
  (let (
        [add-point-aux (lambda (m pointP pointQ curve)
                         (with-helper-funcs curve
                                            (let* (
                                                   [result-x (- (sqr m) (point-x pointP) (point-x pointQ))]
                                                   [result-y-1 (+ (neg (point-y pointP))
                                                                  (* m (- (point-x pointP) result-x))
                                                                  )]
                                                   ;[result-y-2 (+ (neg (point-y pointQ))
                                                   ;               (* m (- (point-x pointQ) result-x))
                                                   ;               )]
                                                   )
                                              ;(unless (validate (point result-x result-y-1) curve) (error 'add-point-not-valid1)) ; TODO remove
                                              ;(unless (validate (point result-x result-y-2) curve) (error 'add-point-not-valid2)) ; TODO remove
                                              ;(unless (equal? result-y-1 result-y-2) (error 'lofasz)) ; TODO remove
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

(define (validate point curve)
  (let ([y12 (calc-y (point-x point) curve)])
    (and
      (or (equal? (point-y point) (car y12)) (equal? (point-y point) (cadr y12)))
      (< (point-x point) (elliptic-curve-p curve))
      (< (point-y point) (elliptic-curve-p curve))
      )
    )
  )

(define (sign-point x curve)
  (cond
    ((zero? (point-y x)) "0")
    ((<= (point-y x) (arithmetic-shift (elliptic-curve-p curve) -1)) "-")
    (else "+")
    )
  )

