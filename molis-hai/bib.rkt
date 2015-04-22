#lang racket

(require scriblib/autobib)

(provide (all-defined-out))

(define-cite ~cite citet generate-bibliography)

(define bogus-ref
   (make-bib #:title "FIXME"
             #:author (authors "Bobby Bogus")
             #:date "2097"
             #:location "Add a real reference here..."))

(define bonneau-guessing-phd 
  (make-bib #:title "Guessing human-chosen secrets"
            #:author (authors "Joseph Bonneau")
            #:date 2012
            #:location
            (dissertation-location #:institution "University of Cambridge")))

(define bonneau-guessing
  (make-bib #:title 
            "The science of guessing: analyzing an anonymized corpus of 70 million passwords"
            #:author (authors "Joseph Bonneau")
            #:date "2012"
            #:location
            (proceedings-location
             "2012 IEEE Symposium on Security and Privacy")))

(define bs-56-bit 
  (make-bib #:title "Towards reliable storage of 56-bit secrets in human memory"
            #:author (authors "Joseph Bonneau" "Stuart Schechter")
            #:date "2014"
            #:location
            (proceedings-location "Proc. USENIX Security")))


(define asl-making-passwords-usable
  (make-bib #:title "Making passwords secure and usable"
            #:author (authors "Anne Adams" "Martina Angela Sasse"  "Peter Lunt")
            #:date 1997
            #:location
            (proceedings-location
             "People and Computers XII"
             #:pages '(1 19))))

(define ebbinghaus 
  (make-bib 
   #:title "Über das gedächtnis: untersuchungen zur experimentellen psychologie"
   #:author "Hermann Ebbinghaus"
   #:date 1885
   #:location
   (book-location #:publisher "Duncker & Humblot")))

(define cpvwr-spaced-repetition 
  (make-bib 
   #:title "Distributed practice in verbal recall tasks: A review and quantitative synthesis"
   #:author (authors "Nicholas J. Cepeda"  "Harold Pashler" 
                     "Edward Vul"  "John T. Wixted" "Doug Rohrer")
   #:date 2006
   #:location
   (journal-location "Psychological Bulletin"
                     #:volume 132
                     #:number 3)))

#;(define bdl-spaced-repetition bogus-ref)
(define horse-battery-staple
  (make-bib #:title "Password Strength, XKCD #936"
            #:author "Randall Monroe"
            #:date 2011
            #:location "on the web: http://www.xkcd.com/936/"))

(define ridyhew 
  (make-bib #:title "RIDYHEW. The RIDiculouslY Huge English Wordlist"
            #:author "Chris Street"
            #:location "on the web: http://www.codehappy.net/wordlist.htm"))

(define nist-passwords
  (make-bib #:title "Automated Password Generator"
            #:author "NIST"
            #:location "Federal Information Processing Standards Publication No. 181"
            #:date 1993))

(define gd-nist-is-broken 
  (make-bib #:title "A new attack on random pronounceable password generators"
            #:author (authors "Ravi Ganesan" "Chris Davies")
            #:date "1994"
            #:location
            (proceedings-location
             "Proceedings of the 17th NIST-NCSC National Computer Security Conference")))


(define lv-pronounce3 
  (make-bib #:title "A comparative study of three random password generators"
            #:author (authors "Michael D. Leonhard" "VN Venkatakrishnan")
            #:date "2007"
            #:location
            (journal-location "IEEE EIT" #:pages '("227" "232"))))

#;(define markov bogus-ref)

(define huffman-trees 
  (make-bib 
   #:title "A method for the construction of minimum redundancy codes"
   #:author (authors "David A. Huffman" "others")
   #:date 1952
   #:location
   (journal-location "Proceedings of the IRE"
                     #:volume 40
                     #:number 9
                     #:pages '(1098 1101))))

(define dickens-a-tale-of-two-cities
  (make-bib #:title "A Tale of Two Cities"
            #:author (authors "Charles Dickens")
            #:date "1859"
            #:location
            (book-location #:publisher "Chapman & Hall")))


(define shannon
  (make-bib #:title "A Mathematical Theory of Communication"
            #:author (authors "Claude E. Shannon")
            #:date "1948"
            #:location
            (journal-location "Bell System Technical Journal"
                              #:volume 7
                              #:pages '(379 423))))
