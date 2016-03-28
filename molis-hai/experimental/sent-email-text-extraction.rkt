#lang racket

;; this code is for extracting sent emails. More specifically,
;; it extracts only non-quoted text from an mboxcl2-formatted
;; file (that is, the standard UNIX 'mbox' format.) If you
;; want to run this code, you'll have to install the mboxrd
;; package.

(require mboxrd-read
         net/head
         net/mime
         net/qp
         (only-in net/unihead generalize-encoding))



;; message -> (listof string)
;; given a message, use net/mime to extract a viable "text part", and
;; then return only that text which is not quoted--that is, which appears
;; to have been written by me.
(define (only-text-parts analyzed raw-msg)
  (define (recur msg) (only-text-parts msg raw-msg))
  (define msg-entity (message-entity analyzed))
  (match (disposition-type (entity-disposition msg-entity))
    ['inline
     (match (entity-type msg-entity)
       ['multipart
        (match (entity-subtype msg-entity)
          ['alternative
           (define parts (entity-parts msg-entity))
           (apply append (map recur parts))
           ]
          ['mixed ;; I think for these, the first one will be the text
           ;; of the message... can there be 0 sub-parts?
           (define parts (entity-parts msg-entity))
           (recur (first parts))]
          ['related ;; For these... we'll try taking them all?
           (define parts (entity-parts msg-entity))
           (apply append (map recur parts))]
          ['signed
           (recur (first
                   (entity-parts msg-entity)))])]
       ['text
        (match (entity-subtype msg-entity)
          ['plain
           (define body-bytes
             (call-with-output-bytes
              (lambda (port) ((entity-body msg-entity) port))))
           (define body-str
             (with-handlers [(exn?
                              (lambda (exn)
                                (fprintf (current-error-port)
                                         "error: ~v\n" exn)
                                (raise exn)))]
               (define charset
                 (match (entity-charset msg-entity)
                   ['us-ascii "us-ascii"]
                   [(? string? s) s]
                   [other (error 'only-text-parts
                                 "unexpected charset: ~v\n"
                                 (entity-charset msg-entity))]))
               (define re-encoded
                 (first
                  (regexp-match #px".*"
                                (reencode-input-port
                                 (open-input-bytes body-bytes)
                                 (generalize-encoding
                                  charset)))))
               (match (entity-encoding msg-entity)
                 ['7bit re-encoded]
                 ['quoted-printable
                  (qp-decode re-encoded)]
                 [other
                  (error "unexpected encoding: ~v\n"
                         (entity-encoding msg-entity))]
                 )))
           (list body-str)]
          ['html
           (printf "giving up on HTML text entity.\n")
           '()]
          #;['csv (error 'decode 
                       "what the heck? ")])]
       ['application
        (match (entity-subtype msg-entity)
          ['pdf '()]
          [else
           (error 'only-text-parts
               (format "found inline application in e-mail with headers: ~e
and raw text:
~e"
                       (message-fields analyzed)
                       raw-msg))])
        ]
       ['image '()])]
    ['attachment '()]))

;; don't keep it if it starts with a '>' or if it's of the form
;; "On ..., such-and-such wrote:"
(define (only-mine str)
  (not (or (regexp-match #px"^>" str)
           (regexp-match #px"^On [^,]+,.* wrote:$" str))))

(define (extract-my-text msg)
  (define text-parts
    (only-text-parts (mime-analyze msg) msg))
  (when (> (length text-parts) 1)
    (write text-parts)
    (newline)
    #;(write msg)
    #;(newline)
    (error 'extract-my-text "expected message with one text part, got: ~e" msg))
  (define text-part (match text-parts
                      [(list) #""]
                      [(list one-part) one-part]))
  (define paras (regexp-split #px"\r\n" text-part))
  (filter only-mine paras))




(define (process-file filename)
  (define-values (closer msg-stream)
    (mboxcl2-parse (string->path filename)
                   #:fallback #t))
  (call-with-output-file "/tmp/email-texts.txt"
    (lambda (port)
      (for ([msg (in-stream msg-stream)])
        (define my-text-paras
          (extract-my-text
           (bytes-append (first msg)
                         (regexp-replace* #rx"(\r\n|\n)"
                                          ((second msg))
                                          "\r\n"
                                          ))))
        (for ([para my-text-paras])
          (fprintf port "~a\n" para))))
    #:exists 'append)  
  (closer))

(call-with-output-file "/tmp/email-texts.txt"
  (λ (port) 13)
  #:exists 'truncate)

#;(process-file "/tmp/sent/Sent Messages")
(process-file "/tmp/sent/Sent-computer")
(process-file "/tmp/sent/Sent-falcon")





