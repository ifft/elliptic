#lang racket
(require rackunit "elliptic-crypto.rkt")
(require "ripemd160.rkt")
(require "utility.rkt")
(require "bitcoin-curve.rkt")
(require "modulo-arith.rkt")
(require "elliptic-arith.rkt")

(with-helper-funcs
  bitcoin-curve

  (random-seed 42)

  (define message #"Message to be signed")
  (define tamperedmessage #"Message to be Signed")
  (define alicepriv (gen-priv-key bitcoin-curve))
  (define alicepub (* bc_G alicepriv))

  (define-values (r s) (sign (n-byte-int->number 32 (padmessage message)) alicepriv bitcoin-curve))
  (check-equal? r 107259175415431880076523644623259231832630027381936395642049712571715173317563)
  (check-equal? s 104460022056839873524628905760380203278361492473266195692294703911620822836515)

  (check-true  (verify (n-byte-int->number 32 (padmessage message)) r s alicepub bitcoin-curve))
  (check-false (verify (n-byte-int->number 32 (padmessage tamperedmessage)) r s alicepub bitcoin-curve))
  )

;;;;; sign ;;;;;

