#lang typed/racket

(provide string-levenshtein/path)

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
(define-type Operation (U (List 'insert String) (List 'delete String) 'copy (List 'change String String)))

;; produce an initial vector for a string
(: init-vec ((Listof String) -> DistVec))
(define (init-vec elts)
  (define len (length elts))
  (define v (make-vector (add1 len) (Cell 0 '(()))))
  (let loop ([i : Natural 0] [trail : Trail '()])
    (vector-set! v i (Cell i (list (reverse trail))))
    (cond [(<= len i) 'done]
          [else
           (loop (add1 i) (cons (list 'delete (list-ref elts i)) trail))]))
  v)

;; given two lists of strings, return the list of trails (turned forward) showing
;; how to get from the first to the second
(: string-levenshtein/path ((Listof String) (Listof String) -> (Listof Trail)))
(define (string-levenshtein/path a b)
  (define scratch-vec (init-vec a))
  (for ([i (in-range (length b))]
        [b-char (in-list b)])
    (update-vec! scratch-vec a b-char))
  (map (ann reverse (Trail -> Trail))
       (Cell-trails (vector-ref scratch-vec
                                (sub1 (vector-length scratch-vec))))))

;; given a dist-vec and a list of strings of input and a single output character,
;; mutate the dist-vec to reflect the addition of that string (thereby
;; generating the "next row" as it appears in the reference
(: update-vec! (DistVec (Listof String) String -> Void))
(define (update-vec! v s c)
  (unless (= (vector-length v) (add1 (length s)))
    (raise-argument-error 'update-vec! "list of length one less than vec"
                          1 v s c))
  ;; a "boundary condition" cell so terrible it will never be chosen:
  (define terrible-cell (Cell (add1 (length s)) (list)))
  (define old-zerocell (vector-ref v 0))
  (vector-set! v 0 (Cell (add1 (Cell-cost old-zerocell))
                         (map (ann (λ ([t : Trail]) (cons (list 'insert c) t))
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
       (define old-elt (list-ref s (sub1 i)))
       
       (define bestchoice
         (bestcell
          (list (cell-incr 1 (list 'insert c) abovecell)
                (cell-incr 1 (list 'delete old-elt) leftcell)
                (cond [(equal? old-elt c)
                       (cell-incr 0 'copy upleftcell)]
                      [else
                       (cell-incr 1 (list 'change old-elt c) upleftcell)]))))
       (vector-set! v i bestchoice)
       (loop (cast (add1 i) Index) abovecell)])))

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
(check-equal? (init-vec (list "a" "b" "c" "d"))
              (vector (Cell 0 '(()))
                      (Cell 1 '(((delete "a"))))
                      (Cell 2 '(((delete "a") (delete "b"))))
                      (Cell 3 '(((delete "a") (delete "b") (delete "c"))))
                      (Cell 4 '(((delete "a") (delete "b") (delete "c") (delete "d"))))))

(define tvec (vector (Cell 0 '(()))
                      (Cell 1 '(((delete "a"))))
                      (Cell 2 '(((delete "a") (delete "b"))))
                      (Cell 3 '(((delete "a") (delete "b") (delete "c"))))))
(update-vec! tvec
             '("a" "b" "c")
             "b")

(check-equal? tvec (vector (Cell 1 '(((insert "b"))))
                           (Cell 1 '(((change "a" "b"))))
                           (Cell 1 '((copy (delete "a"))))
                           (Cell 2 '(((delete "c") copy (delete "a"))))))

(check-equal? (string-levenshtein/path (list "a" "b") (list "b" "d"))
              (list '((change "a" "b") (change "b" "d"))
                    '((delete "a") copy (insert "d"))))

(check-equal? (list->set (string-levenshtein/path '("b" "e" "r" "n" "i" "e") '("e" "r" "m" "i" "n" "e")))
             (list->set
              '(((delete "b") copy copy (change "n" "m") copy (insert "n") copy))))