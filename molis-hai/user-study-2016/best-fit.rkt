#lang racket

(require math/statistics)

;; this file tries to determine the "best-fit" l for each user.

;; the model in this case is a one-parameter model, defining this
;; simplified learning function:

;; f(t) = (1 - t/l) when 0 < t < l
;; f(t) = 0         when t >= l

(require plot
         rackunit)

(define d (file->value "/tmp/timesequences.rktd"))

(define longenough
  (filter (λ (d) (< 2 (length (second d)))) d))

(length longenough)

(define meanerrs
  (for/list ([person (in-list longenough)])
    (list (< (first person) 13)
          (mean
           (rest (map (λ (p) (vector-ref p 1)) (second person)))))))

(define groupa (map second (filter (λ (p) (first p)) meanerrs)))
(define groupb (map second (filter (λ (p) (not (first p))) meanerrs)))

(plot (list (density groupa #:color 1)
            (density groupb #:color 2)))


(define example-sequence #;(second (first d))
  '(#(0 1)
    #(400000 2/9)
    #(700000 3/9)
    #(800000 0)
    #(1000000 0)))

(define t0 (vector-ref (first example-sequence) 0))

(define shifted-sequence
  (for/list ([pt (in-list example-sequence)])
    (vector (- (vector-ref pt 0) t0) (vector-ref pt 1))))

(plot (list (points shifted-sequence)
            (lines shifted-sequence))
      #:x-max (* 2 1000000)
      #:width 1300)

(define (badness l)
  (for/sum ([pt (in-list shifted-sequence)])
    (match-define (vector t e) pt)
    (expt (cond [(< t l) (- e (- 1 (/ t l)))]
                [else e])
          2)))

(check-equal? (badness 1)
              (badness 1000))
(check-equal? (badness 1000)
              (badness 300000))
(check-equal? (badness 1)
              (+ 4/81 9/81))
(check-equal? (badness 700000)
              (+ (* (- 2/9 3/7) (- 2/9 3/7))
                 9/81))

(exact->inexact (badness 515000))

(plot (function badness)
      #:width 1300
      #:x-min 1
      #:x-max (* 2 1000000)
      #:y-min 0)

(plot (function badness)
      #:width 300
      #:x-min 400000
      #:x-max 700000)