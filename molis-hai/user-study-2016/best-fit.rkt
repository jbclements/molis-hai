#lang racket

(require math/statistics
         math/flonum
         plot
         racket/date)

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
  (when (completely-wrong? seq)
    (raise-argument-error 'optimize
                          "sequence where err is not uniformly 1"
                          0 seq))
  (define fit (seq->fit seq))
  (define dfit/dl (D/dx fit))
  (define candidates (append (candidates-a dfit/dl seq)
                             (list
                              (last-candidate dfit/dl seq))))    
  (define best (argmin fit candidates))
  (list best (fit best)))

;; return true for sequences whose error values are uniformly 1
(define (completely-wrong? seq)
  (andmap (λ (samp) (= (py samp) 1)) seq))

(define (px v) (vector-ref v 0))
(define (py v) (vector-ref v 1))

;; given a sequence of vectors of length two #(x_i y_i),
;; subtract x_0 from each x so that the sequence starts at
;; time zero, and change y_0 to be 1.
(define (shift-seq seq)
  (define t0 (px (first seq)))
  (cons (vector 0 1)
        (for/list ([pt (in-list (rest seq))])
          (vector (- (px pt) t0) (py pt)))))


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
    (define pt1x (px pt1))
    (define pt2x (px pt2))
    (best-in-interval dfit/dx pt1x pt2x)))

;; in the interval starting at the last point in the sequence
;; and stretching toward +inf.0, find the best candidate point.
;; if the derivative is >0 at the left interval, just return it
;; (things will only get greater). Otherwise, scan for a point
;; where the derivative is positive, and then call bracketed-root
(define (last-candidate dfit/dx example-sequence)
  (define lastpt (last example-sequence))
  (define lastptx (px lastpt))
  (define lastderiv (dfit/dx (bump+ lastptx)))
  (cond [(< lastderiv 0)
         (flbracketed-root dfit/dx
                           (exact->inexact lastptx)
                           (exact->inexact
                            (find-positive dfit/dx lastptx)))]
        [else lastptx]))

;; given a sequence, compute the best-fit l and overlay that line on the
;; sequence graph
(define (plot-best-fit seq)
  (define b (optimize seq))
  (define b-points `(#(0 1) #(,(first b) 0)))
  (define x-max (* 1.1 (px (last seq))))
  (plot (list (points (shift-seq seq))
              (lines (shift-seq seq) #:color 0)
              (lines b-points #:color 1))
        
        #:x-max x-max
        #:width 1300
        #:y-min 0))

(define (plot-fit seq)
  (define fit (seq->fit seq))
  (define x-max (* 1.1 (px (last seq))))
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

(define (experimental-group? record)
  (< 13 (first record)))

;; seconds offset for PST:
(define PST (* -8 3600))

;; days in february with tests:
(define feb-days '(3 5 8 10 12 16 17 19 22 24 26))

(define test-day-ends
  (append
   (for/list ([d (in-list feb-days)])
     (find-seconds 0 0 0 (add1 d) 2 2016 PST))
   (list (find-seconds 0 0 0 1 3 2016 PST)
         (find-seconds 0 0 0 3 3 2016 PST))))

(plot (list
       (density
        (map (λ (pt) (px pt)) (apply append (map second d)))
        0.05)
       (points (for/list ([tde (in-list test-day-ends)])
                 (vector tde 1e-6))))
      
      #:width 1200)

(define (has-early-practice? trace)
  (< 2 (length (filter (λ (pt) (< (px pt) 1e6)) trace))))

(define (longest-gap seq)
  (apply max
         (for/list ([a (in-list seq)]
                    [b (in-list (rest seq))])
           (- (px b) (px a)))))

(define (no-giant-gaps? trace)
  (< (longest-gap trace) (* 8 86400)))

(define good-traces
  (for*/list ([record (in-list d)]
              [seq (in-value (second record))]
              #:when (< 2 (length seq))
              [shifted (in-value (shift-seq seq))]
              #:when (not (completely-wrong? shifted))
              ;#:when (has-early-practice? shifted)
              ;#:when (no-giant-gaps? shifted)
              )
    (list (first record) shifted)))

(length good-traces)



#;(for ([record (in-list good-traces)]
      [i (in-naturals)])
    (define seq (second record))
    (match-define (list seconds-to-learn fit) (optimize seq))
    (begin
      (printf "processing #: ~v\n" i)
      (display (plot-best-fit seq))
      (newline)
      (printf "l in days, fit: ~v, ~v\n" (/ seconds-to-learn 86400) fit)))

(define experimental-traces
  (filter experimental-group? good-traces))
(printf "number of experimental traces: ~v\n"
        (length experimental-traces))

(define control-traces
  (filter (λ (t) (not (experimental-group? t))) good-traces))
(printf "number of control traces: ~v\n" (length control-traces))

;; oh boy. Okay, taking a look at fraction of people
;; who got n% correct in each training.

(require racket/block)
(block
 ;; return the last point in the interval, or #f if none exists:
 (define (last-in-interval start-sec end-sec seq)
   (let loop ([remaining seq] [previous #f])
     (cond [(empty? remaining) previous]
           ;; too early:
           [(< (px (first remaining)) start-sec)
            (loop (rest remaining) #f)]
           ;; in the interval:
           [(< start-sec (px (first remaining)) end-sec)
            (loop (rest remaining) (first remaining))]
           ;; past the interval:
           [else previous])))
 ;; these aren't shifted...
 (define control-seqs
   (map second (filter (λ (rec) (not (experimental-group? rec))) d)))
 (define experimental-seqs
   (map second (filter experimental-group? d)))
 (define (n%points seqs thresh)
   (for/list ([test-start (in-list (cons 0 test-day-ends))]
              [test-day-end (in-list test-day-ends)])
     (define errs
       (map py
            (filter
             (λ (x) x)
             (for/list ([c (in-list seqs)])
               (last-in-interval test-start test-day-end c)))))
     (vector
      test-day-end
      (/ (length (filter (λ (e) (<= e thresh)) errs))
         (length errs)))))
 (for/list ([i 11])
   (define thresh (* 0.1 i))
   (define control-pct-pts (n%points control-seqs thresh))
   (define experimental-pct-pts (n%points experimental-seqs thresh))
   (printf "less than or equal to ~v% error:\n"
           (* 10 i))
   (display
    (plot (list
           (points control-pct-pts)
           (lines control-pct-pts
                  #:color 1)
           (points experimental-pct-pts)
           (lines experimental-pct-pts
                  #:color 0))
          
          #:width 1000))
   (newline)))


;; mean # of errors

(define (meanerrs person)
  (mean (rest (map py (second person)))))

(define experimental-meanerrs (map meanerrs experimental-traces))
(define control-meanerrs (map meanerrs control-traces))

(plot (list (density experimental-meanerrs #:color 0)
            (density control-meanerrs #:color 1)))






(define (traces-points traces)
  (filter
   (λ (x) (< (py x) 0.05))
   (for/list ([record (in-list traces)])
     (define seq (shift-seq (second record)))
     (list->vector (optimize seq)))))

(define experimental-points (traces-points experimental-traces))
(define control-points (traces-points control-traces))

(plot (list (points experimental-points #:color 0)
            (points control-points #:color 1)))

(plot (list
       (density (map (λ (p) (px p)) experimental-points)
                0.3
                #:color 0)
       (density (map (λ (p) (px p)) control-points)
                0.3
                #:color 1)))

#;(take experimental-traces 10)

#;(plot (list (density '(3 4 3 4) #:color 0)
            (density '(7 8) #:color 1)))



