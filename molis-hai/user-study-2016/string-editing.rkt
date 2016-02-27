#lang typed/racket

(require typed/rackunit)

(provide legal-change?)

;; can the new one be obtained from the old one by deleting a range
;; of characters and (optionally) inserting one in its place?
(: legal-change? (String String -> Boolean))
(define (legal-change? old new)
  (define new-len (string-length new))
  (define p-chars (prefix-match old new))
  (define s-chars (suffix-match old new))
  (>= (+ p-chars s-chars) (- (string-length new) 1)))

;; how many characters match at the beginning of the strings?
(: prefix-match (String String -> Natural))
(define (prefix-match a b)
  (let loop ([posn : Natural 0])
    (cond [(or (= posn (string-length a))
               (= posn (string-length b))) posn]
          [(equal? (string-ref a posn)
                   (string-ref b posn))
           (loop (add1 posn))]
          [else posn])))

;; how many characters match at the end of the strings?
(: suffix-match (String String -> Natural))
(define (suffix-match a b)
  (define a-len (string-length a))
  (define b-len (string-length b))
  (let loop ([posn : Natural 0])
    (cond [(or (= posn (string-length a))
               (= posn (string-length b))) posn]
          [(equal? (string-ref a (- a-len posn 1))
                   (string-ref b (- b-len posn 1)))
           (loop (add1 posn))]
          [else posn])))

;; scan to see if the second may be obtained from the first
;; by deleting a contiguous range of chars.
;; scan from position posn to ensure no more than one replaced
;; char. Assumes after is longer than before by exactly one char.
#;(: delete-loop (String String -> Boolean))
#;(define (replacement-loop before after)
  (let loop ([posn : Natural 0]
             ;; can be 0 or 1:
             [b-diff : Boolean #f])
    (cond [(= posn (string-length before)) #t]
          [(equal? (string-ref before posn)
                   (string-ref after posn))
           (loop (add1 posn) b-diff)]
          ;; first difference, but maybe not fatal:
          [(not b-diff) (loop (add1 posn) #t)]
          ;; second difference, give up:
          [else #f])))

(check-true (legal-change? "a" "ab"))
(check-true (legal-change? "a" "ba"))
(check-true (legal-change? "abcde" "abcxde"))
(check-equal? (prefix-match "abcde" "abcxde") 3)
(check-equal? (suffix-match "abcde" "abcxde") 2)
(check-true (legal-change? "abcde" "xabcde"))
(check-false (legal-change? "abcde" "abcxye"))
(check-true (legal-change? "abcde" "abxde"))
(check-false (legal-change? "abcde" "abxye"))
(check-equal? (legal-change? "abcdefg" "abcg") true)
(check-equal? (legal-change? "abcdefg" "abxfg") true)
(check-false (legal-change? "a" "dac"))

