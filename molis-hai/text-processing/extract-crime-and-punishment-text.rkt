#lang racket

(require (planet neil/html-parsing)
         racket/runtime-path
         sxml/sxpath
         rackunit)

(define-runtime-path here ".")

(define cp-sxml
  (call-with-input-file (build-path here "crime-and-punishment.html")
    (lambda (port)
      (html->xexp
       (reencode-input-port port "windows-1251")))))

;; truncate after n elements (calling to-dotted on each one),
;; inserting '... , otherwise leave alone
(define (my-trunc cap n l)
  (cond [(empty? l) empty]
        [(= n 0) (list (~a (length l)" more..."))]
        [else (cons (to-dotted cap (first l))
                    (my-trunc cap (sub1 n) (rest l)))]))

;; remove elements after the nth of each list
(define (to-dotted cap sexp)
  (cond [(pair? sexp)
         (my-trunc cap cap sexp)]
        [else sexp]))

(define paragraphs ((sxpath '(html body div dd)) cp-sxml))

;; flatten a 'dd'. Return a list of hopefully-strings
(define (flatten-dd s)
  (match s
    [(cons 'dd elts) (apply append (map flatten-element elts))]))

;; problematic paragraphs contain these search strings:
;;наказание за преступление гораздо
;;слышу под моим окном в третьем часу.

;; flatten certain HTML forms into a list of strings OR the symbol
;; 'this-is-the-end
(define (flatten-element elt)
  (match elt
    ["THE END\n" 'this-is-the-end]
    [(list '& 'nbsp) (list " ")]
    ;; sneaky attribute removal:
    [(cons sym (cons (cons '@ attrs) elements))
     (flatten-element (cons sym elements))]
    [(cons 'b elements) (flatten-many elements)]
    [(cons 'div elements)
     (flatten-many elements)]
    [(cons 'p elements) (flatten-many elements)]
    [(cons 'i elements) (flatten-many elements)]
    [(cons 'u elements) (flatten-many elements)]
    [(cons 'font elements)
     (flatten-many elements)]
    ;; superscript, just eat it:
    [(list 'sup "1") (list "")]
    [(list 'sup "2") (list "")]
    [(list 'sup (? string? s)) (list "")]
    [(list 'br) (list "")]
    [(? string? s) (list s)]
    [other (error 'flatten-element
                  "unknown element: ~e"
                  other)]))

;; flatten a list of elements into one list
(define (flatten-many elts)
  (apply append (map flatten-element elts)))

(check-equal? (flatten-element '(u (@ (foo "oheut")) "rubber"))
              '("rubber"))

(list-ref paragraphs 3977)

(define flattened-pars (map flatten-dd (take paragraphs
                                             #;3978
                                             (- (length paragraphs)
                                                10))))

split-at

(define flattened (apply append (map flatten-dd (take paragraphs
                                                      #;3978
                                                      (- (length paragraphs)
                                                         10)))))

(for ([s (in-list flattened)])
  (when (not (string? s))
    (error 'all-flattened "not yet a string: ~e\n" s)))

(display-to-file (apply string-append flattened)
                 "/tmp/crime-and-punishment.txt"
                 #:exists 'truncate)

#;(to-dotted 15 paragraphs)