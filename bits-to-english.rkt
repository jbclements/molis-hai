#lang typed/racket

;; putting back in the memoized part would probably
;; require making it untyped again...

(require typed/rackunit
         #;memoize
         "huffman.rkt")

(provide count-hash->huff-tree
         pick-leaf
         (struct-out Branch)
         (struct-out Leaf))

;; a decision-tree is either 
;; a (non-cons) value, or
;; (cons decision-tree decision-tree)

;; convert a cons tree of numbers into a mytree with weights zero (for testing)
(: num-cons-tree->mytree (Any -> (MyTree Number)))
(define (num-cons-tree->mytree t)
  (cond [(number? t) (Leaf 0 t)]
        [(cons? t) (Branch 0 (num-cons-tree->mytree (car t))
                           (num-cons-tree->mytree (cdr t)))]
        [else (error 'num-cons-tree->mytree "expected conses or numbers, given: ~v"
                     t)]))

;; given a decision tree and a boolean generator,
;; return a leaf and the remaining bools
(: pick-leaf (All (T) ((MyTree T) (Listof Boolean) -> (Pair T (Listof Boolean)))))
(define (pick-leaf dtree bools)
  (cond [(Branch? dtree)
         ;; if we run out, just go to #t all the time:
         (cond [(empty? bools)
                (pick-leaf (Branch-l dtree) empty)]
               [(first bools)
                (pick-leaf (Branch-l dtree) (cdr bools))]
               [else
                (pick-leaf (Branch-r dtree) (cdr bools))])]
        [else (cons (Leaf-n dtree) bools)]))

(let ()
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
#|
;; oops... knapsack isn't as good as simple huffman coding here....
(define (bestfit limit remaining)
  (cond [(empty? remaining)
         (list 0 (list))]
        [else
         (define this-weight (cdr (first remaining)))
         ;; what if we don't use this one?
         (define dont-use-this-one (bestfit limit (rest remaining)))
         ;; don't try this one if it's ridiculously large, or if we've already
         ;; solved the problem exactly:
         (cond [(or (> this-weight limit) (= (first dont-use-this-one) limit))
                dont-use-this-one]
               [else
                ;; what if we do use this one?
                (define do-use-this-one 
                  (add-this-weight (bestfit (- limit this-weight) (rest remaining))
                                   (first remaining)))
                ;; pick the better one:
                (argmax first (list dont-use-this-one do-use-this-one))])]))

;; add this weight to a solution
(define (add-this-weight sum-and-indexes weight-and-value)
  (list (+ (cdr weight-and-value) (first sum-and-indexes))
        (cons weight-and-value (second sum-and-indexes))))


(check-equal? (bestfit 10 '((a . 3) (b . 1) (c . 298) (d . 7) (e . 29)
                                    (f . 6) (g . 17) (h . 14)))
              (list 10 '((a . 3) (d . 7))))

;; regression only:
(check-equal? (bestfit 50 (map (lambda (x) (cons 'z x))
                               (list 23 19 17 1 7 14 38 4 20 21)))
              (list 50 (map (lambda (x) (cons 'z x)) (list 1 7 38 4))))


(define (build-tree letter-weights)
  (cond 
    [(empty? letter-weights) (error 'build-tree "broken invariant: called with empty list")]
    [(empty? (rest letter-weights)) (first letter-weights)]
    [else
     (define sum (apply + (map cdr letter-weights)))
     (define half-sum (round (/ sum 2)))
     (define best-fit (bestfit half-sum letter-weights))
     (define taken (second best-fit))
     (define untaken (remove-taken letter-weights taken))
     (cons (build-tree taken)
           (build-tree untaken))]))

;; given a list and an (ordered) sublist, return the list of the elements that aren't
;; in the sublist
(define (remove-taken l subl)
  (cond [(empty? subl) l]
        [else (cond [(empty? l) (error 'remove-taken "l empty, broken invariant")]
                    [else (cond [(equal? (first l) (first subl))
                                 (remove-taken (rest l) (rest subl))]
                                [else 
                                 (cons (first l)
                                       (remove-taken (rest l) subl))])])]))

(check-equal? (remove-taken '(a b c d e f g h) '(b c g))
  '(a d e f h))

(score (build-tree hash-as-list) 0)

|#

(: test-data (HashTable Char Natural))
(define test-data
  '#hash((#\, . 1) (#\. . 1) (#\: . 1) (#\b . 8) (#\c . 14) (#\d . 7) (#\f . 3)
                   (#\g . 4) (#\l . 209) (#\m . 174) (#\n . 221) (#\p . 94) (#\r . 278)
                   (#\s . 153) (#\t . 111) (#\u . 111) (#\v . 5) (#\y . 2)))


(define hash-as-list (hash->list test-data))

;; given a letter-hash, convert it to a list of Leaf structures
(: count-hash->leafs (All (T) ((HashTable T Natural) -> (Listof (Leaf T)))))
(define (count-hash->leafs letterhash)
  (map (lambda: ([pr : (Pair T Natural)]) (Leaf (cdr pr) (car pr)))
       (hash->list letterhash)))

;; convert a single mytree back to a tree of conses
;; NB: NOT NEEDED IN TYPED
#;(define (MyTree->conses/noweight tree)
  (cond [(Branch? tree) (cons (MyTree->conses/noweight (Branch-l tree))
                              (MyTree->conses/noweight (Branch-r tree)))]
        [else (Leaf-n tree)]))

;; NB: NOT NEEDED IN TYPED
#;(define (MyTree->conses tree)
  (cond [(Branch? tree) (cons (MyTree->conses (Branch-l tree))
                              (MyTree->conses (Branch-r tree)))]
        [else (cons (Leaf-n tree) (Leaf-wt tree))]))

;; a metric for scoring treeifications. a lower score is in some sense
;; worse because it encodes fewer bits per character, but better in the
;; sense that it probably produces more idiomatic text.
;; NB: NOT ADAPTED TO TYPED
#;(define (score cons-tree depth)
  (cond [(char? (car cons-tree))
         (* depth (cdr cons-tree))]
        [else
         (+ (score (car cons-tree) (add1 depth))
            (score (cdr cons-tree) (add1 depth)))]))

(: count-hash->huff-tree (All (T) ((HashTable T Natural) -> (MyTree T))))
(define (count-hash->huff-tree letterhash)
  (clump (count-hash->leafs letterhash)))

#;(letter-hash->huff-tree test-data)
