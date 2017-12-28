#lang racket
(require rackunit "elliptic-crypto.rkt")
(require "hash-utils.rkt")
(require "utility.rkt")
(require "bitcoin-curve.rkt")
(require "modulo-arith.rkt")
(require "elliptic-arith.rkt")
(with-helper-funcs
  bitcoin-curve

  (random-seed 42)
  (define message #"Message to be signed")
  (define alicepriv (gen-priv-key bitcoin-curve))
  (define alicepub (* bc_G alicepriv))

  (define-values (r s) (sign (n-byte-int->number 32 (padmessage message)) alicepriv bitcoin-curve))
  (check-equal? r 107259175415431880076523644623259231832630027381936395642049712571715173317563)
  (check-equal? s 68778258076178924419337151631623653645223893458184992207447324235564698265435)

  (check-true (verify (n-byte-int->number 32 (padmessage message)) r s alicepub bitcoin-curve))
  )

;;;;; sign ;;;;;

