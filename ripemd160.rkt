#lang racket
(require "utility.rkt")
(provide 
 padmessage
 ripemd160
)

(define blocklen 64)
(define extradatalen 8)
(define dword-size-in-bytes 4)

; expands message+extradatalen to the next bloclen boundary
(define (expand-message msgbytes)
  (let (
        [len (+ (bytes-length msgbytes) extradatalen)]
        [%   (lambda (x) (modulo x blocklen))])
    (bytes-append msgbytes (make-bytes extradatalen 0) (make-bytes (% (- blocklen (% len))) 0))))

; adds the stop-bit to the end of the message
(define (addstopbit msgbytes)
 (bytes-append msgbytes (bytes #x80)))

; insert message bit-length to the last extradatalen bytes.
; last extradatalen bytes will be unconditionally overwritten
(define (insert-msglen msgbytes bit-len)
  (when (< (bytes-length msgbytes) extradatalen) (error 'insert-msglen "byte stream is too short"))
  (let ([bit-len (integer->integer-bytes bit-len extradatalen #f)])
    (bytes-append (subbytes msgbytes 0 (- (bytes-length msgbytes) 8)) bit-len)))

; pad message
(define (padmessage
          (inport (current-input-port))
          (outport (current-output-port)))
  (let ([buffer (make-bytes blocklen 0)])
    (let loop ([read-so-far 0]
               [read-last (read-bytes! buffer inport 0 blocklen)])
      (let ([last? (eof-object? (peek-bytes 1 0 inport))]
            [read-last (if (eof-object? read-last) 0 read-last)])
        (cond
          (last?
            (let ([sub-buffer (subbytes buffer 0 read-last)]
                  [msglen (* 8 (+ read-so-far read-last))])
              (write-bytes (insert-msglen (expand-message (addstopbit sub-buffer)) msglen))))
          (else
            (write-bytes buffer)
            (loop (+ read-so-far read-last) (read-bytes! buffer inport 0 blocklen))))))))
    
(define (n-byte-int->number n stepsize msgbytes)
  (let-values
    ([(ret x)
      (let ([by-stepsize-bytes (* 8 stepsize)]
            [numsteps   (/ n stepsize)])
        (unless (integer? numsteps) (error 'n-byte-int->number "n should be dividable by stepsize"))
        (for/fold
          ([number 0]
           [msg msgbytes])
          ([i (in-range numsteps)])
          (values (+ (arithmetic-shift number by-stepsize-bytes)
                     (integer-bytes->integer msg #f #t 0 stepsize))
                  (subbytes msg stepsize)))) ])
    ret))

(define (ripemd160_f x y z)
 (bitwise-xor x y z))

(define (ripemd160_g x y z)
  (bitwise-ior
    (bitwise-and x y)
    (bitwise-and
      (bitwise-not x)
      z)))

(define (ripemd160_h x y z)
  (bitwise-xor
    (bitwise-ior
      x
      (bitwise-not y))
    z))

(define (ripemd160_i x y z)
  (bitwise-ior
    (bitwise-and x z)
    (bitwise-and
      y
      (bitwise-not z))))

(define (ripemd160_j x y z)
  (bitwise-xor
    x
    (bitwise-ior
      y
      (bitwise-not z))))

(define (crot-dword-left x n)
  (when (>= x (expt 2 32)) (error 'crot-dword-left "out of range"))
  (bitwise-bit-field
    (bitwise-ior
      (arithmetic-shift x n)
      (arithmetic-shift x (- (- 32 n))))
    0
    32))

(define blockinit
  '#(#x67452301
     #xefcdab89
     #x98badcfe
     #x10325476
     #xc3d2e1f0))

(define magic-left
  '#(
     #x00000000
     #x5A827999
     #x6ED9EBA1
     #x8F1BBCDC
     #xA953FD4E))

(define magic-right
  '#(
     #x50A28BE6
     #x5C4DD124
     #x6D703EF3
     #x7A6D76E9
     #x00000000))

(define functions-left
  `#(,ripemd160_f ,ripemd160_g ,ripemd160_h ,ripemd160_i ,ripemd160_j))

(define functions-right
  `#(,ripemd160_j ,ripemd160_i ,ripemd160_h ,ripemd160_g ,ripemd160_f))

(define wordselect-left '#(
                           #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
                           #(7 4 13 1 10 6 15 3 12 0 9 5 2 14 11 8)
                           #(3 10 14 4 9 15 8 1 2 7 0 6 13 11 5 12)
                           #(1 9 11 10 0 8 12 4 13 3 7 15 14 5 6 2)
                           #(4 0 5 9 7 12 2 10 14 1 3 8 11 6 15 13)))
(define wordselect-right '#(
                            #(5 14 7 0 9 2 11 4 13 6 15 8 1 10 3 12)
                            #(6 11 3 7 0 13 5 10 14 15 8 12 4 9 1 2)
                            #(15 5 1 3 7 14 6 9 11 8 12 2 10 0 4 13)
                            #(8 6 4 1 3 11 15 0 5 12 2 13 9 7 10 14)
                            #(12 15 10 4 1 5 8 7 6 2 13 14 0 3 9 11)))

(define rotselect-left '#(
                          #(11 14 15 12 5 8 7 9 11 13 14 15 6 7 9 8)
                          #(7 6 8 13 11 9 7 15 7 12 15 9 11 7 13 12)
                          #(11 13 6 7 14 9 13 15 14 8 13 6 5 12 7 5)
                          #(11 12 14 15 14 15 9 8 9 14 5 6 8 6 5 12)
                          #(9 15 5 11 6 8 13 12 5 12 13 14 11 8 5 6)))

(define rotselect-right '#(
                           #(8 9 9 11 13 15 15 5 7 7 8 11 14 14 12 6)
                           #(9 13 15 7 12 8 9 11 7 7 12 7 6 15 13 11)
                           #(9 7 15 11 8 6 6 14 12 13 5 14 13 13 7 5)
                           #(15 5 8 11 14 14 6 14 6 9 12 9 12 5 15 8)
                           #(8 5 12 9 12 5 14 6 8 13 6 5 15 13 11 11)))

(struct branch (functions magics wordselect rotselect) #:transparent)
(define left-branch (branch functions-left magic-left wordselect-left rotselect-left))
(define right-branch (branch functions-right magic-right wordselect-right rotselect-right))
(define machine `(,left-branch ,right-branch))

(define dword+
  (lambda args
    (bitwise-bit-field (apply + args) 0 32)))

(define (perform-function fun magic a b c d e x s)
  (values
    (dword+ (crot-dword-left (dword+ a (fun b c d) x magic) s) e)
    b
    (crot-dword-left c 10)
    d
    e))

(define (rotateboxes a b c d e)
  (values e a b c d))

(define (calc-branch msgbytes branch blocks-state)
  (let-values ([(a0 b0 c0 d0 e0) (vector->values blocks-state)])
              (for*/fold
                ([a a0]
                 [b b0]
                 [c c0]
                 [d d0]
                 [e e0])
                ([iter (in-range 5)]
                 [ix (in-range 16)])
                (let* ([dword-index (* dword-size-in-bytes (vector-ref (vector-ref (branch-wordselect branch) iter) ix))]
                       [msg-as-int (integer-bytes->integer msgbytes #f #f dword-index (+ dword-index dword-size-in-bytes))]
                       [rotate-index (vector-ref (vector-ref (branch-rotselect branch) iter) ix)]
                       [function (vector-ref (branch-functions branch) iter)]
                       [magic (vector-ref (branch-magics branch) iter)])
                  (call-with-values
                    (lambda ()
                      (perform-function
                        function magic a b c d e
                        msg-as-int
                        rotate-index))
                    (lambda (a b c d e)
                      (rotateboxes a b c d e)))))))


(define (merge-branch-results blockstates blocks-from-left blocks-from-right)
  (let-values
    (
     [( a  b  c  d  e) (vector->values       blockstates)]
     [(al bl cl dl el) (vector->values  blocks-from-left)]
     [(ar br cr dr er) (vector->values blocks-from-right)])
    (let  ([A (dword+ dr cl b)]
           [B (dword+ c dl er)]
           [C (dword+ d el ar)]
           [D (dword+ e al br)]
           [E (dword+ a bl cr)]
           )
      `#(,A ,B ,C ,D ,E))))

(define (assemble-result final-blocks)
  (define (dword->bytes dword) (integer->integer-bytes dword 4 #f #t))

  (apply
    bytes-append
    (map
      dword->bytes
      (vector->list final-blocks))))

(define (compress
          (inport (current-input-port))
          (outport (current-output-port)))
  (let ([buffer (make-bytes blocklen 0)])
    (let loop (
               [bytes-read (read-bytes! buffer inport 0 blocklen)]
               [blockstates blockinit])
      (if (or (eof-object? bytes-read)
              (< bytes-read blocklen))
        (assemble-result blockstates)
        (let* ([new-blocks
                 (map
                   (lambda (branch)
                     (let-values
                       ([(a b c d e)
                         (calc-branch buffer branch blockstates)])
                       `#(,a ,b ,c ,d ,e)))
                   machine)]
               [blocks-from-left (car new-blocks)]
               [blocks-from-right (cadr new-blocks)])
          (loop
            (read-bytes! buffer inport 0 blocklen)
            (merge-branch-results
              blockstates
              blocks-from-left
              blocks-from-right)))))))

(define (dword-little-endian->big-endian bstr)
  (define (convertdword dword)
    (list->bytes (reverse (bytes->list dword))))

  (let loop ([bstr bstr]
             [accum #""])
    (cond
      ((< (bytes-length bstr) 4) accum)
      (else
        (loop
          (subbytes bstr 4 (bytes-length bstr))
          (bytes-append accum (convertdword (subbytes bstr 0 4))))))))

(define (md160->string hash)
  (foldr
    string-append
    ""
    (let ([converted (dword-little-endian->big-endian hash)])
      (map
        padhex
        (bytes->list converted)))))

(define (md160->number hash)
  (n-byte-int->number 20 4 (dword-little-endian->big-endian hash)))

; TODO parallelize padding and hash calculation
(define (ripemd160
          (inport (current-input-port))
          (outport (current-output-port))
          (string? #f))
  (let-values ([(in out) (make-pipe)])
              (padmessage inport out)
              (compress in outport)
              ))
#|
  (if string?
    (md160->string
      (compress
        (with-output-to-bytes
          (lambda () (call-with-input-bytes message padmessage)))))
    (md160->number
      (compress 
        (with-output-to-bytes
          (lambda () (call-with-input-bytes message padmessage)))))))
|#
