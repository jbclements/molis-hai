#lang typed/racket

;; I want a levenshtein distance that returns the details of the optimal
;; set of changes. I think I'm just going to implement it myself....


;; first, following the trail of

;; http://people.cs.pitt.edu/~kirk/cs1501/Pruhs/\\
;; Fall2006/Assignments/editdistance/Levenshtein%20Distance.htm:

;; a dist-vec is a vector of cells.
;; it represents a single row of the grid shown in in the reference
;; above. In addition to the number shown in the reference, each
;; cell also contains a list of paths, showing (in reverse)
;; what sequence of operations
;; (insert, delete, copy, change) corresponds to the optimal solution.

(define-type DistVec (Vectorof Cell))
(struct Cell ([cost : Integer] [trails : (Listof Trail)])
  #:transparent)
(define-type Trail (Listof Operation))
(define-type Operation (U 'insert 'delete 'copy 'change))

;; produce an initial vector for a string
(: init-vec (Index -> DistVec))
(define (init-vec len)
  (define v (make-vector (add1 len) (Cell 0 '(()))))
  (let loop ([i : Natural 0] [trail : Trail '()])
    (cond [(< len i) 'done]
          [else
           (vector-set! v i (Cell i (list trail)))
           (loop (add1 i) (cons 'delete trail))]))
  v)

;; given two strings, return the list of trails (turned forward) showing
;; how to get from the first to the second
(: string-levenshtein/path (String String -> (Listof Trail)))
(define (string-levenshtein/path a b)
  (define scratch-vec (init-vec (string-length a)))
  (for ([i (in-range (string-length b))]
        [b-char (in-string b)])
    (update-vec! scratch-vec a b-char))
  (map (ann reverse (Trail -> Trail))
       (Cell-trails (vector-ref scratch-vec
                                (sub1 (vector-length scratch-vec))))))

;; given a dist-vec and a string of input and a single output character,
;; mutate the dist-vec to reflect the addition of that character (thereby
;; generating the "next row" as it appears in the reference
(: update-vec! (DistVec String Char -> Void))
(define (update-vec! v s c)
  (unless (= (vector-length v) (add1 (string-length s)))
    (raise-argument-error 'update-vec! "string of length one less than vec"
                          1 v s c))
  ;; a "boundary condition" cell so terrible it will never be chosen:
  (define terrible-cell (Cell (add1 (string-length s)) (list)))
  (define old-zerocell (vector-ref v 0))
  (vector-set! v 0 (Cell (add1 (Cell-cost old-zerocell))
                         (map (ann (λ (t) (cons 'insert t))
                                   (Trail -> Trail))
                              (Cell-trails old-zerocell))))
  (let loop ([i : Index 1]
             ;; represents the cell that used to be above and to the left
             ;; initially, the terrible cell
             [upleftcell : Cell old-zerocell])
    (cond
      [(= (vector-length v) i)
       (void)]
      [else
       (define abovecell (vector-ref v i))
       (define sub1i (- i 1))
       (define leftcell (cond [(<= 0 sub1i) (vector-ref v sub1i)]
                              [else terrible-cell]))
       
       (define bestchoice
         (bestcell
          (list (cell-incr 1 'insert abovecell)
                (cell-incr 1 'delete leftcell)
                (cond [(eq? (string-ref s (sub1 i)) c)
                       (cell-incr 0 'copy upleftcell)]
                      [else
                       (cell-incr 1 'change upleftcell)]))))
       (vector-set! v i bestchoice)
       (loop (ensure-index (add1 i)) abovecell)])))

(define-predicate index? Index)
(: ensure-index (Number -> Index))
(define (ensure-index n)
  (cond [(index? n) n]
        [else (raise-argument-error 'ensure-index
                                    "Index"
                                    0 n)]))
;; produce a new cell with the cost increased by dcost and
;; the label 'label' added to all trails
(: cell-incr (Integer Operation Cell -> Cell))
(define (cell-incr dcost label cell)
  (Cell (+ dcost (Cell-cost cell))
        (map (ann (λ (t) (cons label t))
                  (Trail -> Trail))
             (Cell-trails cell))))

(: bestcell ((Listof Cell) -> Cell))
(define (bestcell l)
  (when (empty? l)
    (raise-argument-error 'bestcell "nonempty list" 0 l))
  (let loop ([remaining (rest l)]
             [best-trails (Cell-trails (first l))]
             [best-cost (Cell-cost (first l))])
    (cond [(empty? remaining) (Cell best-cost best-trails)]
          [else
           (define elt (first remaining))
           (define elt-cost (Cell-cost elt))
           (cond [(< best-cost elt-cost)
                  (loop (rest remaining)
                        best-trails
                        best-cost)]
                 [(= elt-cost best-cost)
                  (loop (rest remaining)
                        (append (Cell-trails elt) best-trails)
                        best-cost)]
                 [else
                  (loop (rest remaining)
                        (Cell-trails elt)
                        elt-cost)])])
    ))

(require typed/rackunit)
(check-equal? (init-vec 4)
              (vector (Cell 0 '(()))
                      (Cell 1 '((delete)))
                      (Cell 2 '((delete delete)))
                      (Cell 3 '((delete delete delete)))
                      (Cell 4 '((delete delete delete delete)))))

(define tvec (vector (Cell 0 '(()))
                     (Cell 1 '((delete)))
                     (Cell 2 '((delete delete)))
                     (Cell 3 '((delete delete delete)))))
(update-vec! tvec
             "abc"
             #\b)

(check-equal? tvec (vector (Cell 1 '((insert)))
                           (Cell 1 '((change)))
                           (Cell 1 '((copy delete)))
                           (Cell 2 '((delete copy delete)))))

(check-equal? (string-levenshtein/path "ab" "bd")
              (list (list 'change 'change)
                    (list 'delete 'copy 'insert)))

(check-equal? (list->set (string-levenshtein/path "bernie" "ermine"))
             (list->set
              '((delete copy copy change copy insert copy))))