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

(: source-text (Parameterof Path-String))
(define source-text
  (make-parameter (build-path here "a-tale-of-two-cities.txt")))



(: cleanup-filter (Parameterof (String -> String)))
(define cleanup-filter
  (make-parameter whitespace-crunch))

(: word-start-filter (Parameterof (String -> Boolean)))
(define word-start-filter
  (make-parameter starts-with-space?))

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

;; replace all space and non-alnum chars with hyphens in the source text
(: enable-whitespace-to-hyphens (-> Void))
(define (enable-whitespace-to-hyphens)
  (cleanup-filter non-alnum-to-hyphens)
  (word-start-filter starts-with-hyphen?))


;; set the source text (if the file exists)
(: set-source-text (Any -> Void))
(define (set-source-text path)
  (cond
    [(string? path)
     (cond [(file-exists? path)
            (source-text path)]
           [else
            (raise-argument-error
             'set-source-text
             "path of existing file"
             0 path)])]
    [else
     (raise-argument-error 'set-source-text
                           "String" 1 path)]))

(define-predicate natural? Nonnegative-Integer)

(command-line
 #:program (short-program+command-name)
 #:once-each
 [("-b" "--bits") bits "Number of bits of entropy"
                  (set-numeric-parameter-from-string
                   entropy-bits bits 'set-entropy-bits 500)]
 [("-n" "--passwords") pwds "Number of passwords generated"
                       (set-numeric-parameter-from-string
                        num-pwds pwds 'set-num-pwds 100)]
 [("-o" "--model-order") order "Order of the model"
                         (set-numeric-parameter-from-string
                          model-order order 'set-model-order 10)]
 [("-t" "--source-text") source-text "Source text corpus"
                         (set-source-text source-text)]
 ["--hyphens" "replace whitespace with hyphens in source text"
              (enable-whitespace-to-hyphens)])

(define atotc-path (build-path here "a-tale-of-two-cities.txt"))

(define model (build-char-model (model-order)
                                ((cleanup-filter) (file->string (source-text)))
                                (word-start-filter)))

(for ([i (in-range (num-pwds))])
  (display (substring (generate-char-pwd model
                                         (random-bool-list (entropy-bits))) 1))
  (newline))

