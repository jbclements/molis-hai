#lang racket/base

(require racket/runtime-path
         "char-model.rkt"
         "random-bits.rkt"
         raco/command-name
         racket/cmdline
         racket/file
         racket/match)

(define-runtime-path here ".")

(define entropy-bits (make-parameter 56))
(define num-pwds (make-parameter 1))

;; given a string, set the number of bits
(define (set-entropy-bits bits-str)
  (match (string->number bits-str)
    [(? number? n) (entropy-bits n)]
    [else (raise-argument-error 'set-entropy-bits "number" 0 bits-str)]))

;; given a string, set the number of bits
(define (set-num-pwds n-str)
  (match (string->number n-str)
    [(? number? n) (num-pwds n)]
    [else (raise-argument-error 'set-num-pwds "number" 0 n-str)]))

(command-line
 #:program (short-program+command-name)
 #:once-each
 [("-b" "--bits") bits-str "Specify number of bits of entropy"
                  (set-entropy-bits bits-str)]
 [("-n" "--passwords") num-str "Specify number of passwords generated"
                       (set-num-pwds num-str)])

(define atotc-path (build-path here "a-tale-of-two-cities.txt"))

(define model (build-char-model 2 (file->string atotc-path)))

(for ([i (in-range (num-pwds))])
  (display (substring (generate-char-pwd model
                                         (random-bool-list (entropy-bits))) 1))
  (newline))

