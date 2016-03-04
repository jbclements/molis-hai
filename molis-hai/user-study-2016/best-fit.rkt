#lang racket

(require math/statistics
         math/flonum
         plot)

;; this file tries to determine the "best-fit" l for each user.
;; see below for the details of the one-parameter model.

;; given a sequence, return a pair containing the best value of
;; l, and the quality of the fit (represented as mean squared
;; error)
(define (optimize seq)
  (unless (equal? (first seq) #(0 1))
    (raise-argument-error 'optimize
                          "shifted sequence"
                          0 seq))
    (define fit (seq->fit seq))
    (define dfit/dl (D/dx fit))
    (define candidates (append (candidates-a dfit/dl seq)
                               (list
                                (last-candidate dfit/dl seq))))    
    (define best (argmin fit candidates))
    (list best (fit best)))


;; given a sequence of vectors of length two #(x_i y_i),
;; subtract x_0 from each x so that the sequence starts at
;; time zero, and change y_0 to be 1.
(define (shift-seq seq)
  (define t0 (vector-ref (first seq) 0))
  (cons (vector 0 1)
        (for/list ([pt (in-list (rest seq))])
          (vector (- (vector-ref pt 0) t0) (vector-ref pt 1)))))


;; the model in this case is a one-parameter model, defining this
;; simplified learning function:

;; f(t) = (1 - t/l) when 0 < t < l
;; f(t) = 0         when t >= l

;; the metric that we're trying to minimize is 'fit', defined as
;; fit(l) = mean_over_i (e_i - f(t_i))^2

;; measures the distance from the sample points to the
;; model parameterized by 'l'.
(define ((seq->fit sequence) l)
  (mean
   (for/list ([pt (in-list sequence)])
     (match-define (vector t e) pt)
     (expt (cond [(< t l) (- e (- 1 (/ t l)))]
                 [else e])
           2))))



;; ERM. Actually, not doing this:
;; hand-rolling minimization, with many test cases...
;; after rewriting, the function is of the form
;; fit(l) = k_0 / l^2 + k_1 / l + k_2.
;; we'll the the constant term first

;; following suggestion from Jens Axel Søegaard, using root-finder instead:

(define EPSILON 1e-4)
(define (bump+ x) (+ x EPSILON))
(define (bump- x) (- x EPSILON))

;; given a function from Real->Real, return the (numeric)
;; derivative
(define ((D/dx f) x)
 (define d (/ (- (f (bump+ x)) (f (bump- x)))
              (* 2.0 EPSILON)))
  d)

;; given the derivative and the left and right
;; edges, compute the point with the minimum value.
;; NOTE: relies HEAVILY on invariants of this particular
;; problem, in particular the fact that each segment has
;; at most one change in the sign of the derivative.
(define (best-in-interval df/dx pt1x pt2x)
    (define deriv1 (df/dx (bump+ pt1x)))
    (define deriv2 (df/dx (bump- pt2x)))
    ;; here comes the domain-specific knowledge...
    ;; because the second derivatives are >= 0, then
    ;; if either end has first derivative 0, it must be
    ;; the minimum on that interval.
    (cond [(= deriv1 0) pt1x]
          [(= deriv2 0) pt2x]
          ;; both < 0 : return rightmost point
          [(and (< deriv1 0) (< deriv2 0)) pt2x]
          ;; both > 0 : return leftmost point
          [(and (< 0 deriv1) (< 0 deriv2)) pt1x]
          [(and (< deriv1 0) (< 0 deriv2))
           (flbracketed-root df/dx
                             (+ pt1x (* 2 EPSILON))
                             (- pt2x (* 2 EPSILON)))]
          [else (error "find-min" "impossible 56:d6")]))

;; keep increasing until you find a positive value:
(define (find-positive fun x)
  (let loop ([x x] [incr 1])
    (cond [(< 0 (fun x)) x]
          [else (loop (+ x incr) (* 2 incr))])))


;; in each interval between two points of the sequence,
;; pick the point that could minimize the badness of fit.
(define (candidates-a dfit/dx seq)
  (for/list ([pt1 (in-list seq)]
             [pt2 (in-list (rest seq))])
    (define pt1x (vector-ref pt1 0))
    (define pt2x (vector-ref pt2 0))
    (best-in-interval dfit/dx pt1x pt2x)))

;; in the interval starting at the last point in the sequence
;; and stretching toward +inf.0, find the best candidate point.
;; if the derivative is >0 at the left interval, just return it
;; (things will only get greater). Otherwise, scan for a point
;; where the derivative is positive, and then call bracketed-root
(define (last-candidate dfit/dx example-sequence)
  (define lastpt (last example-sequence))
  (define lastptx (vector-ref lastpt 0))
  (define lastderiv (dfit/dx (bump+ lastptx)))
  (cond [(< lastderiv 0)
         (flbracketed-root dfit/dx
                           (exact->inexact lastptx)
                           (exact->inexact (find-positive dfit/dx lastptx)))]
        [else lastptx]))

;; given a sequence, compute the best-fit l and overlay that line on the
;; sequence graph
(define (plot-best-fit seq)
  (define b (optimize seq))
  (define b-points `(#(0 1) #(,(first b) 0)))
  (define x-max (* 1.1 (vector-ref (last seq) 0)))
  (plot (list (points (shift-seq seq))
              (lines (shift-seq seq) #:color 0)
              (lines b-points #:color 1))
        
        #:x-max x-max
        #:width 1300
        #:y-min 0))

(define (plot-fit seq)
  (define fit (seq->fit seq))
  (define x-max (* 1.1 (vector-ref (last seq) 0)))
  (plot (list (function fit)
              (function (λ (x) 0.0)))
          #:width 1300
          #:x-min 1
          #:x-max x-max
          #:y-min 0))

(module+ test
  (require rackunit)
  (check-equal? (shift-seq '(#(234 8/10) #(1234 2/10) #(2234 1/10)))
                '(#(0 1) #(1000 2/10) #(2000 1/10)))
  
  (define example-sequence
    (shift-seq
     '(#(0 1)
       #(400000 2/9)
       #(700000 3/9)
       #(800000 0)
       #(1000000 0))))
  
  #;(plot (list (points shifted-sequence)
              (lines shifted-sequence))
        #:x-max (* 2 1000000)
        #:width 1300)
  
  
  (define test-fit (seq->fit example-sequence))
  (check-equal? (test-fit 1)
                (test-fit 1000))
  (check-equal? (test-fit 1000)
                (test-fit 300000))
  (check-equal? (test-fit 1)
                (/ (+ 4/81 9/81) 5))
  (check-equal? (test-fit 700000)
                (/ (+ (* (- 2/9 3/7) (- 2/9 3/7))
                      9/81)
                   5))
  
  
  
  #;(plot (function test-fit)
        #:width 1300
        #:x-min 1
        #:x-max (* 2 1000000)
        #:y-min 0)
  
  #;(plot (list (function (D/dx test-fit))
              (function (λ (x) 0.0)))
        #:width 1300
        #:x-min 1
        #:x-max 1200000
        #:y-min -5e-8)

  (define ((close-to x eps) y)
    (< (abs (- x y)) eps))
  ;; REGRESSION TESTING:
  (check-match (optimize example-sequence)
               (list (? (close-to 514286 1.0) _1)
                     (? (close-to 0.022 1e-3))))

  (define off-rhs-example
    (shift-seq
     '(#(100 9/10) #(5100 9/10) #(10100 8/10))))

  (check-match (optimize off-rhs-example)
               (list (? (close-to 50000 1e-5))
                     (? (close-to 0.0 1e-5))))

  )

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

(for ([record (in-list longenough)]
      [i (in-naturals)])
  (define seq (shift-seq (second record)))
  (match-define (list seconds-to-learn fit) (optimize seq))
  (begin
    (printf "processing #: ~v\n" i)
    (display (plot-best-fit seq))
    (newline)
    (printf "l in days, fit: ~v, ~v\n" (/ seconds-to-learn 86400) fit)))




