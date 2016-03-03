#lang racket

(require math/statistics)

;; this file tries to determine the "best-fit" l for each user.

;; the model in this case is a one-parameter model, defining this
;; simplified learning function:

;; f(t) = (1 - t/l) when 0 < t < l
;; f(t) = 0         when t >= l

;; the metric that we're trying to minimize is 'fit', defined as
;; fit(l) = mean_over_i (e_i - f(t_i))^2

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


(define example-sequence
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

;; measures the distance from the sample points to the
;; model parameterized by 'l'.
(define (fit l)
  (mean
   (for/list ([pt (in-list shifted-sequence)])
     (match-define (vector t e) pt)
     (expt (cond [(< t l) (- e (- 1 (/ t l)))]
                 [else e])
           2))))

(check-equal? (fit 1)
              (fit 1000))
(check-equal? (fit 1000)
              (fit 300000))
(check-equal? (fit 1)
              (+ 4/81 9/81))
(check-equal? (fit 700000)
              (+ (* (- 2/9 3/7) (- 2/9 3/7))
                 9/81))

(exact->inexact (fit 515000))

(plot (function fit)
      #:width 1300
      #:x-min 1
      #:x-max (* 2 1000000)
      #:y-min 0)

(plot (function fit)
      #:width 300
      #:x-min 400000
      #:x-max 700000)

;; ERM. Actually, not doing this:
;; hand-rolling minimization, with many test cases...
;; after rewriting, the function is of the form
;; fit(l) = k_0 / l^2 + k_1 / l + k_2.
;; we'll the the constant term first

;; following suggestion from Jens A.S., using root-finder instead

(require math/flonum)

(define EPSILON 1e-4)
(define (bump+ x) (+ x EPSILON))
(define (bump- x) (- x EPSILON))

(define (df/dx x)
 (define f fit)
 (define d (/ (- (f (bump+ x)) (f (bump- x)))
              (* 2.0 EPSILON)))
  d)

(plot (list (function df/dx)
           (function (λ (x) 0.0)))
     #:width 1300
     #:x-min 1
     #:x-max 1200000
     #:y-min -5e-8)


(flbracketed-root df/dx 5.0e5  6e5)
(flbracketed-root df/dx 7.5e5 10e5)

(for/list ([pt1 (in-list example-sequence)]
           [pt2 (in-list (rest example-sequence))])
  (define pt1x (vector-ref pt1 0))
  (define pt2x (vector-ref pt2 0))
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

(define lastpt (last example-sequence))
(define lastptx (vector-ref lastpt 0))
(define lastderiv (df/dx (bump+ lastptx)))
(cond [(< lastderiv 0)
       (flbracketed-root df/dx lastptx (find-positive df/dx lastptx))]
      [else lastptx])




