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
(define model-order (make-parameter 2))

(define (set-numeric-parameter-from-string param str name limit)
  (match (string->number str)
    [(? number? n) (cond [(<= 0 n limit) (param n)]
                         [else
                          (raise-argument-error
                           name
                           (format "number in range 0..~a" limit)
                           0 n)])]
    [else (raise-argument-error name "number" 0 str)]))

(command-line
 #:program (short-program+command-name)
 #:once-each
 [("-b" "--bits") bits "Specify number of bits of entropy"
                  (set-numeric-parameter-from-string
                   entropy-bits bits 'set-entropy-bits 500)]
 [("-n" "--passwords") pwds "Specify number of passwords generated"
                       (set-numeric-parameter-from-string
                        num-pwds pwds 'set-num-pwds 100)]
 [("-o" "--model-order") order "Specify the order of the model"
                         (set-numeric-parameter-from-string
                          model-order order 'set-model-order 10)])

(define atotc-path (build-path here "a-tale-of-two-cities.txt"))

(define model (build-char-model (model-order) (file->string atotc-path)))

(for ([i (in-range (num-pwds))])
  (display (substring (generate-char-pwd model
                                         (random-bool-list (entropy-bits))) 1))
  (newline))

