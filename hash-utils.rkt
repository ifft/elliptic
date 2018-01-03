#lang racket
(provide 
 padmessage
 n-byte-int->number
 compress
 ;XXX
 dumpdword
)

(define blocklen 64)
(define extradatalen 8)
(define dword-size-in-bytes 4)

; expands message+extradatalen to the next bloclen boundary
(define (expand-message msgbytes)
  (let (
        [len (+ (bytes-length msgbytes) extradatalen)]
        [%   (lambda (x) (modulo x blocklen))]

        )
    (bytes-append msgbytes (make-bytes extradatalen 0) (make-bytes (% (- blocklen (% len))) 0))
    )
  )


; adds the stop-bit to the end of the message
(define (addstopbit msgbytes)
 (bytes-append msgbytes (bytes #x80))
  )

; insert message bit-length to the last extradatalen bytes.
; last extradatalen bytes will be unconditionally overwritten
(define (insert-msglen msgbytes bit-len)
  (when (< (bytes-length msgbytes) extradatalen) (error 'insert-msglen "byte stream is too short"))
  (let ([bit-len (integer->integer-bytes bit-len extradatalen #f)])
    (bytes-append (subbytes msgbytes 0 (- (bytes-length msgbytes) 8)) bit-len
                  )
    )
  )

; pad message
; TODO implement it with ports
(define (padmessage msgbytes)
  (let* (
         [bit-len (* 8 (bytes-length msgbytes))]
         [msgbytes (addstopbit msgbytes)]
         )
    (insert-msglen (expand-message msgbytes) bit-len)
    )
  )

(define (n-byte-int->number n msgbytes)
  (let-values ([(ret x)
                (let (
                      [by-8-bytes (* 8 8)]
                      [numsteps   (/ n 8)]
                      )
                 (unless (integer? numsteps) (error 'n-byte-int->number "n should be dividable by 8"))
                  (for/fold
                    (
                     [number 0]
                     [msg msgbytes]
                     )
                    ([i (in-range numsteps)])
                    (values (+ (arithmetic-shift number by-8-bytes)
                               (integer-bytes->integer msg #f #t 0 8)
                               )
                            (subbytes msg 8)
                            )
                    )
                  )
                ])
              ret
              )
  )

(define (ripemd160_f x y z)
 (bitwise-xor x y z)
 )

(define (ripemd160_g x y z)
  (bitwise-ior
    (bitwise-and x y)
    (bitwise-and
      (bitwise-not x)
      z
      )
    )
  )

(define (ripemd160_h x y z)
  (bitwise-xor
    (bitwise-ior
      x
      (bitwise-not y)
      )
    z
    )
  )

(define (ripemd160_i x y z)
  (bitwise-ior
    (bitwise-and x z)
    (bitwise-and
      y
      (bitwise-not z)
      )
    )
  )

(define (ripemd160_j x y z)
  (bitwise-xor
    x
    (bitwise-ior
      y
      (bitwise-not z)
      )
    )
  )

(define (crot-dword-left x n)
  (when (>= x (expt 2 32)) (error 'crot-dword-left "out of range"))
  (bitwise-bit-field
    (bitwise-ior
      (arithmetic-shift x n)
      (arithmetic-shift x (- (- 32 n)))
      )
    0
    32
    )
  )

(define blockinit
  '#(#x67452301
     #xefcdab89
     #x98badcfe
     #x10325476
     #xc3d2e1f0
     )
  )

(define magic-left
  '#(
     #x00000000
     #x5A827999
     #x6ED9EBA1
     #x8F1BBCDC
     #xA953FD4E
     )
  )

(define magic-right
  '#(
     #x50A28BE6
     #x5C4DD124
     #x6D703EF3
     #x7A6D76E9
     #x00000000
     )
  )

(define functions-left
 `#(,ripemd160_f ,ripemd160_g ,ripemd160_h ,ripemd160_i ,ripemd160_j)
 ;; TODO
)

(define functions-right
 `#(,ripemd160_j ,ripemd160_i ,ripemd160_h ,ripemd160_g ,ripemd160_f)
)

(define wordselect-left '#(
			   #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
			   #(7 4 13 1 10 6 15 3 12 0 9 5 2 14 11 8)
			   #(3 10 14 4 9 15 8 1 2 7 0 6 13 11 5 12)
			   #(1 9 11 10 0 8 12 4 13 3 7 15 14 5 6 2)
			   #(4 0 5 9 7 12 2 10 14 1 3 8 11 6 15 13)
			   )
  )
(define wordselect-right '#(
			    #(5 14 7 0 9 2 11 4 13 6 15 8 1 10 3 12)
			    #(6 11 3 7 0 13 5 10 14 15 8 12 4 9 1 2)
			    #(15 5 1 3 7 14 6 9 11 8 12 2 10 0 4 13)
			    #(8 6 4 1 3 11 15 0 5 12 2 13 9 7 10 14)
			    #(12 15 10 4 1 5 8 7 6 2 13 14 0 3 9 11)
			    )
  )

(define rotselect-left '#(
			  #(11 14 15 12 5 8 7 9 11 13 14 15 6 7 9 8)
			  #(7 6 8 13 11 9 7 15 7 12 15 9 11 7 13 12)
			  #(11 13 6 7 14 9 13 15 14 8 13 6 5 12 7 5)
			  #(11 12 14 15 14 15 9 8 9 14 5 6 8 6 5 12)
			  #(9 15 5 11 6 8 13 12 5 12 13 14 11 8 5 6)
			  )
  )

(define rotselect-right '#(
			   #(8 9 9 11 13 15 15 5 7 7 8 11 14 14 12 6)
			   #(9 13 15 7 12 8 9 11 7 7 12 7 6 15 13 11)
			   #(9 7 15 11 8 6 6 14 12 13 5 14 13 13 7 5)
			   #(15 5 8 11 14 14 6 14 6 9 12 9 12 5 15 8)
			   #(8 5 12 9 12 5 14 6 8 13 6 5 15 13 11 11)
			   )
  )

; TODO organize these to structs
; TODO calculate these at phase level 1
(struct branch (functions magics wordselect rotselect) #:transparent)
(define left-branch (branch functions-left magic-left wordselect-left rotselect-left))
(define right-branch (branch functions-right magic-right wordselect-right rotselect-right))
(define machine `(,left-branch ,right-branch))

(define dword+
  (lambda args
    (bitwise-bit-field (apply + args) 0 32)
    )
  )

(define (perform-function fun magic a b c d e x s)
  (values
    (dword+ (crot-dword-left (dword+ (fun b c d) x magic) s) e)
    b
    (crot-dword-left c 10)
    d
    e
    )
  )

(define (rotateboxes a b c d e)
 (values e a b c d))

(define (calc-branch msgbytes branch blocks-state)
  (let-values ([(a0 b0 c0 d0 e0) (vector->values blocks-state)])
              (for*/fold
                (
                 [a a0]
                 [b b0]
                 [c c0]
                 [d d0]
                 [e e0]
                 )
                (
                 [iter (in-range 5)]
                 [ix (in-range 16)]
                 )
                (let* (
                      [dword-index (* dword-size-in-bytes (vector-ref (vector-ref (branch-wordselect branch) iter) ix))]
                      [msg-as-int (integer-bytes->integer msgbytes #f #f dword-index (+ dword-index dword-size-in-bytes))]
                      [rotate-index (vector-ref (vector-ref (branch-rotselect branch) iter) ix)]
                      [function (vector-ref (branch-functions branch) iter)]
                      [magic (vector-ref (branch-magics branch) iter)]
                      )
                  (call-with-values
                  (lambda ()
                    (perform-function
                      function magic a b c d e
                      msg-as-int
                      rotate-index)
                    )
                    rotateboxes
                    )
                  )
                )
              )
  )


(define (merge-branch-results blockstates blocks-from-left blocks-from-right)
  (let-values
    (
    ;   b  c  d  e  a
     [( a  b  c  d  e) (vector->values       blockstates)]
     ;[(al bl cl dl el) (vector->values  blocks-from-left)]
     ;[(ar br cr dr er) (vector->values blocks-from-right)]
     [(bl cl dl el al) (vector->values  blocks-from-left)]
     [(br cr dr er ar) (vector->values blocks-from-right)]

   ;ideal  JJ(bb, cc, dd, ee, aa, X[13],  6);
   ;actual JJ(aa, bb, cc, dd, ee, X[ 0], 15);


     )
    `#(
       ,(dword+ cl b)
       ,(dword+ c dl er)
       ,(dword+ d el ar)
       ,(dword+ e al br)
       ,(dword+ a bl cr)
       )
    )
  )

(define (dumpdword val)
 (foldr 
  string-append
  ""
  (map (lambda (byte)
        (let ([str (format "~x " byte)])
         (if (= (string-length str) 1) (string-append "0" str) str)
        )
       )
   (bytes->list val)
  )
 )
 )

(define (assemble-result final-blocks)
  (bitwise-ior
    (arithmetic-shift (vector-ref final-blocks 0) (* 4 32))
    (arithmetic-shift (vector-ref final-blocks 1) (* 3 32))
    (arithmetic-shift (vector-ref final-blocks 2) (* 2 32))
    (arithmetic-shift (vector-ref final-blocks 3) 32)
    (vector-ref final-blocks 4)
    )
  )

(define (compress msgbytes)
  (let loop (
             [msgbytes msgbytes]
             [blockstates blockinit]
             )
    (cond
      ((< (bytes-length msgbytes) blocklen) (assemble-result blockstates))
      (else (let* ([new-blocks
                     (map
                       (lambda (branch)
                         (let-values
                           ( [(a b c d e)
                              (calc-branch (subbytes msgbytes 0 64) branch blockstates)]
                            )
                           `#(,a ,b ,c ,d ,e)
                           )
                         )
                       machine
                       )
                     ]
                   [blocks-from-left (car new-blocks)]
                   [blocks-from-right (cadr new-blocks)]
                   [next-chunk (subbytes msgbytes 64 (bytes-length msgbytes))]
                   )
              (loop
                next-chunk
                (merge-branch-results
                  blockstates
                  blocks-from-left
                  blocks-from-right
                  )
                )
              )
            )
      )
    )
  )

