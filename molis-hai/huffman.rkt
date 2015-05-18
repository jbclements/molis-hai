#lang typed/racket/base

;; copyright 2015 John Clements <clements@racket-lang.org>

(require typed/rackunit)

(provide (struct-out Leaf)
         (struct-out Branch)
         MyTree
         clump)



;;; Our Representation of Huffman Trees. They include weights, to allow constructions

(define-struct (T) Leaf ([wt : Natural] [n : T]) #:transparent)
(define-struct (T) Branch ([wt : Natural] [l : (MyTree T)] [r : (MyTree T)]) #:transparent)

(define-type (MyTree T) (U (Leaf T) (Branch T)))


;; keep clumping until we get one tree
(: clump (All (T) ((Listof (MyTree T)) -> (MyTree T))))
(define (clump treelist)
  (if (not (null? (cdr treelist)))
      (clump (one-clump treelist))
      (car treelist)))

;; perform one huffman clumping
(: one-clump (All (T) ((Listof (MyTree T)) -> (Listof (MyTree T)))))
(define (one-clump treelist)
  (let* ([l (sort treelist (lambda ([a : (MyTree T)] 
                                    [b : (MyTree T)])
                             (< (nodeweight a) (nodeweight b))))]
         [a (car l)]
         [b (cadr l)])
    (cons (Branch (+ (nodeweight a) (nodeweight b)) a b) (cddr l))))


;; return the weight of a leaf or branch
(: nodeweight (All (T) ((MyTree T) -> Natural)))
(define (nodeweight tree)
  (cond [(Leaf? tree) (Leaf-wt tree)]
        [else (Branch-wt tree)]))


;; TESTS



(check-equal? (one-clump (list (Branch 13 (Leaf 2 14) (Leaf 11 1)) (Leaf 2 2) (Branch 0 (Leaf 0 3) (Leaf 0 4))))
              (list (Branch 2 (Branch 0 (Leaf 0 3) (Leaf 0 4))
                            (Leaf 2 2))
                    (Branch 13 (Leaf 2 14) (Leaf 11 1))))

(check-equal? (clump (list (Branch 13 (Leaf 2 14) (Leaf 11 1)) (Leaf 2 2) (Branch 0 (Leaf 0 3) (Leaf 0 4))))
              (Branch 15
                      (Branch 2 (Branch 0 (Leaf 0 3) (Leaf 0 4))
                              (Leaf 2 2))
                      (Branch 13 (Leaf 2 14) (Leaf 11 1))))





