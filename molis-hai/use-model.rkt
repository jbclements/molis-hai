
#lang typed/racket

;; Copyright 2015 John Clements <clements@racket-lang.org>

;; generating text using a model

(require "shared-types.rkt"
         "huffman.rkt"
         typed/rackunit)

(provide (struct-out Generated)
         generated->sequence
         generate
         generate-sequence)

;; represents a generated sequence, including the chosen initial state
;; and the sequence of transitions. Each is accompanied by the number
;; of bits of entropy used by each decision.
(define-struct (S T) Generated ([init-state : (List S Natural)]
                                [sequence : (Listof (List T Natural))]))


;; convert a generated to a sequence
(: generated->sequence (All (S T U)
                            ((Generated S T) (S -> (Listof U)) (T -> (Listof U))
                                             -> (Listof U))))
(define (generated->sequence g seed-conv trans-conv)
  (append*
   (cons (seed-conv (car (Generated-init-state g)))
         (map trans-conv (map (ann car ((List T Natural) -> T))
                              (Generated-sequence g))))))

;; given a model and a list of booleans and a transition function, generate a seed and
;; a sequence of transitions
(: generate (All (S V)
                 ((Model S V) (Listof Boolean) (S V -> S) -> (Generated S V))))
(define (generate model bools t-fun)
  (define-values (seed used-bits bits-left) (pick-leaf (Model-seed-chooser model) bools))
  (define seq (generate-sequence t-fun seed bits-left (Model-trans model)))
  (Generated (list seed used-bits) seq))

;; given a (state+transition->state) mapping
;; and a seed (markov cell) and a list of booleans and a hash
;; of huffman trees, generate a sequence of (cons thingy number),
;; where the number indicates how many bits of entropy were used for
;; each thingy.
(: generate-sequence
   (All (S V)
        ((S V -> S) S (Listof Boolean) (Trans S V) ->
                    (Listof (List V Natural)))))
(define (generate-sequence rotate seed bools tree-hash)
  (let loop ([chars seed]
             [bools bools])
    (cond [(empty? bools) empty]
          [else (define-values (next used-bits remaining)
                  (pick-leaf (hash-ref tree-hash chars) bools))
                (cons (list next used-bits)
                      (loop (rotate chars next)
                            remaining))])))

(: ensure-nonnegative (Integer -> Nonnegative-Integer))
(define (ensure-nonnegative f)
  (cond [(< f 0) (error 'ensure-nonnegative "expected positive number, got: ~v" f)]
        [else f]))

;; GENERIC FUNCTIONS ON HUFFMAN TREES

;; given a decision tree and a boolean generator,
;; return a leaf and the number of bits used and the remaining bools
(: pick-leaf (All (T) ((MyTree T) (Listof Boolean) -> (Values T Natural (Listof Boolean)))))
(define (pick-leaf dtree bools)
  (pick-leaf/h dtree 0 bools))

;; helper function
(: pick-leaf/h (All (T) ((MyTree T) Natural (Listof Boolean) -> (Values T Natural (Listof Boolean)))))
(define (pick-leaf/h dtree count bools)
  (cond [(Branch? dtree)
         ;; if we run out, just go to #t all the time:
         (cond [(null? bools)
                (pick-leaf/h (Branch-l dtree) count null)]
               [(car bools)
                (pick-leaf/h (Branch-l dtree) (add1 count) (cdr bools))]
               [else
                (pick-leaf/h (Branch-r dtree) (add1 count) (cdr bools))])]
        [else (values (Leaf-n dtree) count bools)]))


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

  (define-syntax v2l
    (syntax-rules ()
      [(_ a) (call-with-values (lambda () a) list)]))
  
  (define test-tree '(3 . ((1 . 2) . 4)))
  (check-equal? (v2l (pick-leaf (num-cons-tree->mytree test-tree) '()))
                (list 3 0 '()))
  (check-equal? (v2l (pick-leaf (num-cons-tree->mytree test-tree) '(#t)))
                (list 3 1 '()))
  (check-equal? (v2l (pick-leaf (num-cons-tree->mytree test-tree) '(#t #t)))
                (list 3 1 '(#t)))
  (check-equal? (v2l (pick-leaf (num-cons-tree->mytree test-tree) '(#f #t)))
                (list 1 2 '()))
  (check-equal? (v2l (pick-leaf (num-cons-tree->mytree test-tree) '(#f #f)))
                (list 4 2 '())))
