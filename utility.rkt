#lang racket
(provide (all-defined-out))

(define (binary-op x n inc-carry op-step mod_prime unity_element) ; TODO eliminate mod_prime
  (let loop (
             [firstrun #t]
             [carry 1]
             [result unity_element]
             [r0 x]
             [n n]
             )
    (let* ([stop? (zero? n)]
           [carry (if stop? 0 (if firstrun r0 (inc-carry carry)))]
           [>> (lambda (x) (arithmetic-shift x -1))])
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

(define-syntax (with-helper-funcs stx)
  (syntax-case stx ()
               [(with-helper-funcs curve body0 body ...)
                (with-syntax (
                              ; functions came from modulo-arith
                              [elliptic-curve-p (datum->syntax stx 'elliptic-curve-p)]
                              [inverse-of (datum->syntax stx 'inverse-of)]
                              [pow-p (datum->syntax stx 'pow-p)]
                              [sqrt-p (datum->syntax stx 'sqrt-p)]
                              [point? (datum->syntax stx 'point?)]
                              [add-point (datum->syntax stx 'add-point)]
                              [scalar-mul (datum->syntax stx 'scalar-mul)]
                              ; helper functions to be defined
                              [mod (datum->syntax #'with-helper-funcs 'mod)]
                              [inv (datum->syntax #'with-helper-funcs 'inv)]
                              [sqr   (datum->syntax #'with-helper-funcs 'sqr)]
                              [pow   (datum->syntax #'with-helper-funcs 'pow)]
                              [sqrt  (datum->syntax #'with-helper-funcs 'sqrt)]
                              [+     (datum->syntax #'with-helper-funcs '+)]
                              [-     (datum->syntax #'with-helper-funcs '-)]
                              [*     (datum->syntax #'with-helper-funcs '*)]
                              [neg   (datum->syntax #'with-helper-funcs 'neg)]
                              )
                             #'(let* (
                                      [mod   (lambda (x) (modulo x (elliptic-curve-p curve)))]
                                      [inv   (lambda (x) (inverse-of x (elliptic-curve-p curve)))]
                                      [sqr   (lambda (x) (pow-p x 2 (elliptic-curve-p curve)))]
                                      [pow   (lambda (x n) (pow-p x n (elliptic-curve-p curve)))]
                                      [sqrt  (lambda (x) (sqrt-p x (elliptic-curve-p curve)))]
                                      [+     (lambda x   
                                               (cond
                                                 ((andmap point? x)
                                                  (if (null? (cdr x))
                                                    (add-point (car x) curve)
                                                    (add-point (car x) (cadr x) curve)
                                                    )
                                                  )
                                                 (else (mod (apply + x)))
                                                 )
                                               )
                                             ]   
                                      [-     (lambda x   (mod (apply - x)))]
                                      [*     (lambda x
                                               (cond
                                                 ((and (point? (car x)) (number? (cadr x)))
                                                  (scalar-mul (car x) (cadr x))
                                                  )
                                      ;TODO use mod-p without the flexibility of 'apply'
                                                 (else (mod (apply * x)))
                                                 )
                                               )
                                             ]
                                      [neg   (lambda (x) (neg x (elliptic-curve-p curve)))]
                                      )

                                 body0
                                 body
                                 ...
                                 )
                             )
                ]
               )
  )

