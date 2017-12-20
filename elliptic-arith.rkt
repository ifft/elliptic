#lang racket
(require "utility.rkt")
(require "modulo-arith.rkt")

(provide 
  scalar-mul
  negate
  calc-y
  calc-m 
  add-point
  validate
  sign-point
  elliptic-curve
  elliptic-curve

  debug
  (struct-out point)
  (struct-out elliptic-curve)
  )

(define debug (make-parameter #t))
(define devel (make-parameter #t))
(define dprintf
 (lambda x
   (when (debug) (apply printf x))
 )
)

(struct point (x y) #:transparent)
(struct elliptic-curve (a b p G n) #:transparent)

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

(define (negate point curve)
 (with-helper-funcs curve
 (point (point-x point)
  (- (elliptic-curve-p curve)
             (point-y point))
  )
 )
)

(define (calc-y x curve)
 (with-helper-funcs curve
  ;(printf "calc-y ~a ~a~n" x curve)
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
                                                   [result-x (- (sqr m) (point-x pointP) (point-x pointQ))]
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

