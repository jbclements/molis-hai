#lang typed/racket

;; Copyright 2015 John Clements <clements@racket-lang.org>

;; generating text using a model

(require "huffman-wrapper.rkt"
         "huffman.rkt"
         typed/rackunit)

(provide generate-sequence-from-bools
         make-bools-list
         pick-leaf)

;; given a (state+transition->state) mapping
;; and a seed (markov cell) and a list of booleans and a hash
;; of huffman trees, generate a sequence of (cons thingy number),
;; where the number indicates how many bits of entropy were used for
;; each thingy.
(: generate-sequence-from-bools
   (All (S V)
        ((S V -> S) S (Listof Boolean) (Trans S V) ->
                    (Listof (Pair V Natural)))))
(define (generate-sequence-from-bools rotate seed bools tree-hash)
  (let loop ([chars seed]
             [bools bools])
    (cond [(empty? bools) empty]
          [else (match-define (cons next remaining)
                  (pick-leaf (hash-ref tree-hash chars) bools))
                (cons (cons next (ensure-nonnegative
                                  (- (length bools) (length remaining))))
                      (loop (rotate chars next)
                            remaining))])))

(: ensure-nonnegative (Integer -> Nonnegative-Integer))
(define (ensure-nonnegative f)
  (cond [(< f 0) (error 'ensure-nonnegative "expected positive number, got: ~v" f)]
        [else f]))


;; make a list of random booleans of the specified length
(: make-bools-list (Natural -> (Listof Boolean)))
(define (make-bools-list len)
  (for/list ([i len]) (= (random 2) 0)))

;; given a decision tree and a boolean generator,
;; return a leaf and the remaining bools
(: pick-leaf (All (T) ((MyTree T) (Listof Boolean) -> (Pair T (Listof Boolean)))))
(define (pick-leaf dtree bools)
  (cond [(Branch? dtree)
         ;; if we run out, just go to #t all the time:
         (cond [(null? bools)
                (pick-leaf (Branch-l dtree) null)]
               [(car bools)
                (pick-leaf (Branch-l dtree) (cdr bools))]
               [else
                (pick-leaf (Branch-r dtree) (cdr bools))])]
        [else (cons (Leaf-n dtree) bools)]))


;; TESTS
(let ()
    (define-type NumConsTree (U (cons NumConsTree NumConsTree) Number))

  ;; convert a cons tree of numbers into a mytree with weights zero (for testing)
  (: num-cons-tree->mytree (NumConsTree -> (MyTree Number)))
  (define (num-cons-tree->mytree t)
    (cond [(number? t) (Leaf 0 t)]
          [(pair? t) (Branch 0 (num-cons-tree->mytree (car t))
                             (num-cons-tree->mytree (cdr t)))]
          [else (error 'num-cons-tree->mytree "expected conses or numbers, given: ~v"
                       t)]))
  
  (define test-tree '(3 . ((1 . 2) . 4)))
  (check-equal? (pick-leaf (num-cons-tree->mytree test-tree) '())
                (cons 3 '()))
  (check-equal? (pick-leaf (num-cons-tree->mytree test-tree) '(#t))
                (cons 3 '()))
  (check-equal? (pick-leaf (num-cons-tree->mytree test-tree) '(#t #t))
                (cons 3 '(#t)))
  (check-equal? (pick-leaf (num-cons-tree->mytree test-tree) '(#f #t))
                (cons 1 '()))
  (check-equal? (pick-leaf (num-cons-tree->mytree test-tree) '(#f #f))
                (cons 4 '())))
