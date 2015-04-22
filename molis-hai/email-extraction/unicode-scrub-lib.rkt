#lang racket

(require rackunit)

(provide (contract-out
          [unicode-scrub (-> path-string? path-string? void?)]
          [de-unicode/path (->* (path-string? path-string?)
                                (#:char-mapping (hash/c char?
                                                        string?))
                                void?)]))

;; this file contains functions for replacing all non-ASCII characters
;; in a file, using a given mapping. Its first use was in cleaning up
;; files for MOSS, which doesn't seem to like non-ASCII files.

;; the character mapping to use if no other is specified.
(define DEFAULT-CHAR-MAPPING
  (hash #\→ "->"
        #\λ "lambda"
        #\’ "'"
        #\” "\""
        #\“ "\""
        #\π "pi"
        #\· "*"))

;; given an input and output port, rewrite the input-port
;; chars to remove unicode chars and write to the output port
(define (de-unicode ip op char-mapping)
  ;; do as many ascii characters as possible:
  (define ascii-bytes (first (regexp-match #px"[[:ascii:]]*" ip)))
  (display ascii-bytes op)
  ;; grab a non-ascii character
  (define non-ascii-char (read-char ip))
  (cond [(eof-object? non-ascii-char)
         (close-input-port ip)]
        [else (match (hash-ref char-mapping non-ascii-char #f)
                [#f (error 'de-unicode "unexpected non-ascii char at file position ~s: ~s (code point: #x~a)" 
                           (file-position ip)
                           non-ascii-char
                           (number->string (char->integer non-ascii-char) 16))]
                [str (display str op)
                     (de-unicode ip op char-mapping)])]))

(module+ test
  (define os (open-output-string))
  (de-unicode (open-input-string "→ λ ’” “ ") os DEFAULT-CHAR-MAPPING)
  (check-equal? (get-output-string os)
                "-> lambda '\" \" "))

;; given a source file and a target path, rewrite the source
;; to remove unicode chars and smush the to-path
(define (de-unicode/path from-path to-path #:char-mapping [char-mapping
                                                           DEFAULT-CHAR-MAPPING])
  (define op (open-output-file to-path #:exists 'truncate))
  (define ip (open-input-file from-path))
  (de-unicode ip op char-mapping)
  (close-output-port op))

#;(de-unicode "/tmp/t1/foo.rkt" "/tmp/t1/bar.rkt")

(check-equal? (regexp-match #px"[[:ascii:]]*" "abc¥d") (list "abc"))
(check-equal? (regexp-match #px"[[:ascii:]]*" "") (list ""))

;; given a 'from' directory containing directories corresponding
;; to student handins and a 'to' directory, scrub the submissions
;; of unicode chars and put them in the target dir.
(define (unicode-scrub from-dir target-dir)
  (parameterize ([current-directory from-dir])
    (unless (directory-exists? target-dir)
      (error 'target-dir "target dir doesn't exist: ~s" target-dir))
  (for ([d (directory-list)]
        #:when (file-exists? (build-path d "grading" "text.rkt")))
    (printf "path: ~s\n" d)
    (de-unicode/path (build-path d "grading" "text.rkt") 
                     (build-path target-dir (~a (path->string d) ".rkt"))))))