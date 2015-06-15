#lang typed/racket

(require "shared-types.rkt"
         "build-model.rkt"
         "use-model.rkt")

(provide build-word-model
         generate-word-pwd)

;; run the analyses for a given "order"
(: build-word-model (Natural (Listof String) -> (Model (Listof String) String)))
(define (build-word-model n text)
  (kvcount->model (n-word-kvcount n text) all-good))

(define (all-good x) #t)

;; given a list of strings and a source text,
;; build a hash ofthe number of times each n-gram
;; transitions to a given word
(: n-word-kvcount (Natural (Listof String) -> (KVCount (Listof String) String)))
(define (n-word-kvcount n text)
  ;; add the beginning to the end to get the wraparound transitions, too:
  (define t2 (append text (take text (sub1 n))))
  (let loop ([h : (KVCount (Listof String) String) (kvcount-new)]
             [remaining : (Listof String) t2]
             [subremaining (drop t2 n)])
    (cond [(empty? subremaining) h]
          [else
           (loop (kvcount-extend h (take remaining n) (first subremaining))
                 (rest remaining)
                 (rest subremaining))])))


(: generate-word-pwd ((Model (Listof String) String) (Listof Boolean) -> String))
(define (generate-word-pwd model bools)
  (string-append*
   (generated->sequence
    (generate model
              bools
              list-rotate)
    seed-conv
    trans-conv)))

(: seed-conv ((Listof String) -> (Listof String)))
(define (seed-conv low) low)
(: trans-conv (String -> (Listof String)))
(define (trans-conv w) (list w))

(: list-rotate ((Listof String) String -> (Listof String)))
(define (list-rotate l n) 
  (append (rest l) (list n)))


;; make a list of random booleans of the specified length
(: make-bools-list (Natural -> (Listof Boolean)))
(define (make-bools-list len)
  (for/list ([i len]) (= (random 2) 0)))