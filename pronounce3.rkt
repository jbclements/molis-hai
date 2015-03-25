#lang racket

(define pre-templates
  '(aabbabba abababba ababbaba abbaabba
             abbababa abbabbaa baababba baabbaba
             babaabba babababa bababbaa babbaaba
             babbabaa))

;; translate pre-templates into templates.
;; a template is (listof (symbols 'vowel 'consonant))
(define templates
  (for/list ([pt pre-templates])
    (for/list ([c (in-string (symbol->string pt))])
      (match c
        [#\a 'vowel]
        [#\b 'consonant]))))

(define consonants
  (map symbol->string 
       '(b c ch d f g h j k l m
           n p ph r s st v w x y z)))

(define vowels
  '("a" "e" "i" "o" "u"))

(define (generate-consonant)
  (list-ref consonants (random (length consonants))))

(define (generate-vowel)
  (list-ref vowels (random (length vowels))))

(define (template->word template)
  (apply
   string-append
   (for/list ([char (in-list template)])
     (match char
       ['vowel (generate-vowel)]
       ['consonant (generate-consonant)]))))

(define (generate-word)
  (template->word (list-ref templates (random (length templates)))))

(define (generate-password)
  (string-append (generate-word) " " (generate-word)))

(define NUM-PASSWORDS 8)

(for ([i (in-range NUM-PASSWORDS)])
  (display (generate-password))
  (newline))

