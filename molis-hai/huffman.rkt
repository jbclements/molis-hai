#lang typed/racket/base

;; copyright 2015 John Clements <clements@racket-lang.org>

;; Our Representation of Huffman Trees. They include weights,
;; to allow constructions

(require (except-in pfds/heap/binomial map)
         racket/match)

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

;; we want to shorten the path to the left leaf, so we're going to track
;; the length of this path on each tree, and always put the one with the
;; shorter path on the left:

(define-struct (T) Tree+Pathlen ([tree : (MyTree T)]
                                 [pathlen : Natural])
  #:transparent)

;; annotate a leaf with a pathlen of zero
(: add-pathlen (All (T) ((Leaf T) -> (Tree+Pathlen T))))
(define (add-pathlen l)
  (Tree+Pathlen l 0))

;; given a list of leaves, build a single tree.
(: build-huffman-tree (All (T) ((Listof (Leaf T)) -> (MyTree T))))
(define (build-huffman-tree leaves)
  (define with-pathlens (map (inst add-pathlen T) leaves))
  (clump
   (apply heap (inst weight<? T) with-pathlens)))

;; keep clumping until we get one tree
(: clump (All (T) ((Heap (Tree+Pathlen T)) -> (MyTree T))))
(define (clump the-heap)
  (if (empty? (delete-min/max the-heap))
      (Tree+Pathlen-tree (find-min/max the-heap))
      (clump (one-clump the-heap))))

;; perform one huffman clumping. No way to check if heap is
;; of size one, so just return tree if we're done.
(: one-clump (All (T) ((Heap (Tree+Pathlen T))
                       -> (Heap (Tree+Pathlen T)))))
(define (one-clump heap)
  (match-define (Tree+Pathlen a alen) (find-min/max heap))
  (define heap2 (delete-min/max heap))
  (match-define (Tree+Pathlen b blen) (find-min/max heap2))
  (define heap3 (delete-min/max heap2))
  (define newnode
    (cond [(<= alen blen)
           (Tree+Pathlen (Branch (+ (nodeweight a) (nodeweight b))
                                 a b)
                         (add1 alen))]
          [else
           (Tree+Pathlen (Branch (+ (nodeweight b) (nodeweight a))
                                 b a)
                         (add1 blen))]))
  (insert newnode heap3))


;; return the weight of a leaf or branch
(: nodeweight (All (T) ((MyTree T) -> Natural)))
(define (nodeweight tree)
  (cond [(Leaf? tree) (Leaf-wt tree)]
        [else (Branch-wt tree)]))

(: weight<? (All (T) ((Tree+Pathlen T) (Tree+Pathlen T) -> Boolean)))
(define (weight<? a b)
  (< (nodeweight (Tree+Pathlen-tree a))
     (nodeweight (Tree+Pathlen-tree b))))

;; TESTS
(module+ test
  (require typed/rackunit)
  (check-equal? (sorted-list
                 (one-clump
                  (heap (inst weight<? Natural)
                        (Tree+Pathlen (Branch 13 (Leaf 2 14) (Leaf 11 1)) 1)
                        (Tree+Pathlen (Leaf 2 2) 0)
                        (Tree+Pathlen (Branch 0 (Leaf 0 3) (Leaf 0 4)) 1))))
                (list (Tree+Pathlen
                       (Branch 2
                               (Leaf 2 2)
                               (Branch 0 (Leaf 0 3) (Leaf 0 4)))
                       1)
                      (Tree+Pathlen
                       (Branch 13 (Leaf 2 14) (Leaf 11 1))
                       1)))

  (check-equal? (clump (heap
                        (inst weight<? Natural)
                        (Tree+Pathlen (Branch 13 (Leaf 2 14) (Leaf 11 1)) 1)
                        (Tree+Pathlen (Leaf 2 2) 0)
                        (Tree+Pathlen (Branch 0 (Leaf 0 3) (Leaf 0 4)) 1)))
                (Branch 15
                        (Branch 2
                                (Leaf 2 2)
                                (Branch 0 (Leaf 0 3) (Leaf 0 4)))
                        (Branch 13 (Leaf 2 14) (Leaf 11 1)))))





