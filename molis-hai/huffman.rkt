#lang typed/racket/base

;; copyright 2015 John Clements <clements@racket-lang.org>

;; Our Representation of Huffman Trees. They include weights,
;; to allow constructions

(require pfds/heap/binomial)

(provide (struct-out Leaf)
         (struct-out Branch)
         MyTree
         clump
         build-huffman-tree)


(define-struct (T) Leaf ([wt : Natural]
                         [n : T]) #:transparent)
(define-struct (T) Branch ([wt : Natural]
                           [l : (MyTree T)]
                           [r : (MyTree T)])
  #:transparent)

;; a binary tree with T's at the leaves and weights at every leaf and node.
(define-type (MyTree T) (U (Leaf T) (Branch T)))

;; given a list of leaves, build a single tree.
(: build-huffman-tree (All (T) ((Listof (Leaf T)) -> (MyTree T))))
(define (build-huffman-tree leaves)
  (clump
   (apply heap (inst weight<? T)
          (ann leaves (Listof (MyTree T))))))

;; keep clumping until we get one tree
(: clump (All (T) ((Heap (MyTree T)) -> (MyTree T))))
(define (clump the-heap)
  (if (empty? (delete-min/max the-heap))
      (find-min/max the-heap)
      (clump (one-clump the-heap))))

;; perform one huffman clumping. No way to check if heap is
;; of size one, so just return tree if we're done.
(: one-clump (All (T) ((Heap (MyTree T)) -> (Heap (MyTree T)))))
(define (one-clump heap)
  (define a (find-min/max heap))
  (define heap2 (delete-min/max heap))
  (define b (find-min/max heap2))
  (define heap3 (delete-min/max heap2))
  (insert (Branch (+ (nodeweight a) (nodeweight b)) a b) heap3))


;; return the weight of a leaf or branch
(: nodeweight (All (T) ((MyTree T) -> Natural)))
(define (nodeweight tree)
  (cond [(Leaf? tree) (Leaf-wt tree)]
        [else (Branch-wt tree)]))

(: weight<? (All (T) ((MyTree T) (MyTree T) -> Boolean)))
(define (weight<? a b)
  (< (nodeweight a) (nodeweight b)))

;; TESTS
(module+ test
  (require typed/rackunit)
  (check-equal? (sorted-list
                 (one-clump
                  (heap (inst weight<? Natural)
                        (Branch 13 (Leaf 2 14) (Leaf 11 1))
                        (Leaf 2 2)
                        (Branch 0 (Leaf 0 3) (Leaf 0 4)))))
                (list (Branch 2
                              (Branch 0 (Leaf 0 3) (Leaf 0 4))
                              (Leaf 2 2))
                      (Branch 13 (Leaf 2 14) (Leaf 11 1))))

  (check-equal? (clump (heap
                        (inst weight<? Natural)
                        (Branch 13 (Leaf 2 14) (Leaf 11 1))
                        (Leaf 2 2)
                        (Branch 0 (Leaf 0 3) (Leaf 0 4))))
                (Branch 15
                        (Branch 2 (Branch 0 (Leaf 0 3) (Leaf 0 4))
                                (Leaf 2 2))
                        (Branch 13 (Leaf 2 14) (Leaf 11 1)))))





