#lang typed/racket

(require "shared-types.rkt"
         "build-model.rkt")

(provide build-word-model)

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