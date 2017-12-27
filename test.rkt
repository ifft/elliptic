(require "modulo-arith.rkt")
(require "elliptic-arith.rkt")
(require "bitcoin-curve.rkt")
(require "utility.rkt")
(require "elliptic-crypto.rkt")
(require xrepl)

(define alicepriv 36281381264312944114660412731460996624433877351060966911175032316224544298925)
(define bobpriv 109377126302409292064350767041684127893334715379768434484720197907651178515954)

(define-helper-funcs bitcoin-curve)
