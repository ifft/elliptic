#lang racket
(provide debug? debug)
(define debug? (make-parameter #f))
(define stdout (current-output-port))

(define debug
  (lambda args
    (when (debug?)
      (apply fprintf (cons stdout args)))))
