#lang typed/racket
(require typed/racket/random)

(provide random-bool-list
         random-bytes
         byte->bits
         bytes->bits
         bits->num)

;; return a sequence of booleans generated in a cryptographically secure way
(: random-bool-list (Natural -> (Listof Boolean)))
(define (random-bool-list num-bits)
  (define bytes-needed (ceiling (/ num-bits 8)))
  (define rand-bytes (random-bytes bytes-needed))
  (take (bytes->bits (bytes->list rand-bytes)) num-bits))

;; return a sequence of bytes generated in a cryptographically secure way
(: random-bytes (Natural -> Bytes))
(define (random-bytes num-bytes)
  (crypto-random-bytes num-bytes))

;; convert a list of bytes to a list of bits
(: bytes->bits ((Listof Byte) -> (Listof Boolean)))
(define (bytes->bits bytes)
  (append* (map byte->bits bytes)))

(: byte->bits (Byte -> (Listof Boolean)))
(define (byte->bits b)
  (for/list : (Listof Boolean)
    ([i (in-range 8)])
    (= 0 (bitwise-and b (arithmetic-shift #b1 i)))))


(: bool->bit (Boolean -> Byte))
(define (bool->bit b)
  (cond [b 1]
        [else 0]))

(: bits->num ((Listof Boolean) -> Integer))
(define (bits->num bools)
  (for/sum ([i : Natural (in-naturals)]
            [b : Boolean (in-list bools)])
           (cond [b 0]
                 [else (expt 2 i)])))

(module+ test
  (require typed/rackunit)
  (check-equal? (bytes->bits (list #b01010011 #b11001011))
                (append
                 (reverse (list #t #f #t #f #t #t #f #f))
                 (reverse (list #f #f #t #t #f #t #f #f))))
  (check-equal? (byte->bits #b01010011)
                (reverse (list #t #f #t #f #t #t #f #f)))
  (check-equal? (bits->num (list #f)) 1)
  (check-equal? (bits->num (reverse (list #t #f #t #f #t #t #f #f)))
                #b01010011))



