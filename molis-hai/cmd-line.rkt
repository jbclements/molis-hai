#lang typed/racket/base

(require racket/runtime-path
         "char-model.rkt"
         "random-bits.rkt"
         racket/cmdline
         racket/file
         racket/match
         )

(require/typed raco/command-name
               [short-program+command-name (-> String)])

(define-runtime-path here ".")

(: entropy-bits (Parameterof Natural))
(define entropy-bits (make-parameter 56))

(: num-pwds (Parameterof Natural))
(define num-pwds (make-parameter 1))

(: model-order (Parameterof Natural))
(define model-order (make-parameter 2))

(: set-numeric-parameter-from-string
   ((Parameterof Natural) Any Symbol Natural -> Void))
(define (set-numeric-parameter-from-string param str name limit)
  (cond
    [(string? str)
     (match (string->number str)
       [(? natural? n) (cond [(<= 0 n limit) (param n)]
                             [else
                              (raise-argument-error
                               name
                               (format "number in range 0..~a" limit)
                               0 n)])]
       [else (raise-argument-error name "number" 0 str)])]
    [else
     (raise-argument-error 'set-numeric-parameter-from-string
                           "String" 1 param str name limit)]))

(define-predicate natural? Nonnegative-Integer)

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

