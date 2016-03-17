#lang racket

(require racket/runtime-path
         sha
         net/base64
         threading
         math/statistics
         levenshtein
         "string-editing.rkt"
         "table.rkt"
         plot
         rackbot/tai64n
         srfi/19
         racket/block
         racket/struct)



(define-runtime-path here ".")

;; the .tsv file containing session start information:
(define SESSIONS-FILE "/tmp/molis-hai-2162-experiment-sessions.tsv")
;; the .tsv file containing the keystroke information:
(define KEYSTROKES-FILE "/tmp/molis-hai-2162-experiment-keystrokes.tsv")

;; given the name of a .tsv file, read the lines and break them on tabs:
(define (tsv-file->lists filename)
  (call-with-input-file filename
    (λ (port)
      (for/list ([l (in-lines port)])
        (regexp-split #px"\t" l)))))

;; column names in the sessions.tsv file
(define session-start-col-names '(server-timestamp userid key training-str))
(define session-start-converters '(num str str str))
;; column names in the keystroke.tsv file
(define session-data-col-names '(server-timestamp key timestamp boxnum content))
(define session-data-converters '(num str num num str))

;; given a list of converter symbols (currently only 'num and 'str)
;; and a list of strings, apply the appropriate conversions to
;; obtain the values for the table:
(define ((convert-row converters) values)
  (for/list ([val (in-list values)]
             [conv (in-list converters)])
    (match conv
      ['num (string->number val)]
      ['str val])))

;; this table holds the session start information
(define session-starts
  (make-table
   session-start-col-names
   (map (convert-row session-start-converters)
        (tsv-file->lists SESSIONS-FILE))))

;; add an index for userids to this table:
(add-index! session-starts '(userid))

(define session-keys
  (in-table-column session-starts 'key))

;; this table holds the keystroke information:
(define session-datas
  (make-table
   session-data-col-names
   (map (convert-row session-data-converters)
        (tsv-file->lists KEYSTROKES-FILE))))

(time (add-index! session-datas '(key)))

(printf "~v session starts\n" (table-row-count session-starts))

#;(plot
 (density
  (map string-length (hash-values uid->training-str-table))
  0.05))

;; looks like 13 is between the lengths of the two password lengths
(define PASSWORD-LENGTH-THRESHOLD 13)




;; SHOW THE DENSITY OF THE SESSION DATES
(define session-dates
  (in-table-column session-starts 'server-timestamp))
(printf "density plot of session start times:\n")
(plot (density session-dates 0.1)
      #:width 1000)

(plot-file (density session-dates 0.01)
           "/tmp/log-density.pdf"
      #:width 1000)
(plot (density session-dates 0.01)
      #:width 1000)


(printf "total lines of session data: ~v\n" (table-row-count session-datas))

(define timestamp-lists
  (sort
   (for/list ([uid (in-table-column session-starts 'userid)])
     (sort (table-ref session-starts 'userid 'server-timestamp uid) <))
   <
   #:key last))

(plot
 (apply
  append
  (for/list ([t-list (in-list timestamp-lists)]
             [i (in-naturals)])
    (define p
      (for/list ([t (in-list t-list)])
        (vector t i)))
    (list (points p)
          (lines p))))
 #:width 1000)

;; session keys with no data:
(define no-session
  (remove* (in-table-column session-datas 'key) session-keys))

(printf "~v session keys have no data\n" (length no-session))

(define good-session-keys (in-table-column session-datas 'key))

;; given a sub-table containing the rows associated with one key,
;; produce a list of lists of datas. All are in chronological order.
(define (clean-session table key)
  (define session-table (sub-table table `((key ,key))))
  ;; these will be reversed as they go into the hash bins:
  (define by-nth
    (for/hash ([boxnum (in-table-column session-table 'boxnum)])
      (define selected
        (select-where session-table '(timestamp content) `((boxnum ,boxnum))))
      (when (empty? selected)
        (error 'by-nth "internal error 56d4"))
      (values boxnum
              (sort selected <
               #:key (λ (v) (vector-ref v 0))))))
  (define boxes-with-data (sort (hash-keys by-nth) <))
  ;; check box 1 is present
  (unless (= (first boxes-with-data) 1)
    (error 'clean-session
           "session ~v is missing box 1"
           key))
  ;; check the boxes occur in numeric order
  (for/list ([a (in-list boxes-with-data)]
             [b (in-list (rest boxes-with-data))])
    (unless (= (add1 a) b)
      (error 'clean-session
             "session ~v has incomplete box-nums: ~v"
             key
             boxes-with-data)))
  ;; warn if missing late boxes:
  (when (< (length boxes-with-data) 5)
    (printf "warning: session ~v has data for only ~v boxes\n"
            key
            (length boxes-with-data)))
  (define last-box-num (last boxes-with-data))
  (for/list ([i (in-list boxes-with-data)])
    ;; ensure chronological order is preserved:
    (when (< i last-box-num)
      (define earliest-of-next
        (vector-ref (first (hash-ref by-nth (add1 i))) 0))
      (for ([l (in-list (hash-ref by-nth i))])
        (unless (< (vector-ref l 0) earliest-of-next)
          (error 'out-of-order))))
    ;; eliminate lines whose strings are equal to the previous one:
    (define deduped
      (let loop ([prev #f] [remaining (hash-ref by-nth i)])
        (cond [(empty? remaining) '()]
              [else
               (define f (car remaining))
               (define fc (vector-ref f 1))
               (cond [(equal? prev fc)
                      (loop prev (rest remaining))]
                     [else
                      (when (string? prev)
                        (when (not (legal-change? prev fc))
                          (printf
                           "warning: illegal change (spellcheck?) from ~v to \
~v in session: ~v\n"
                           prev fc key)))
                      (cons f
                            (loop fc (rest remaining)))])])))
    (when (< 100 (length deduped))
      (error 'deduping "still too many entries in deduped list: ~v"
             deduped))
    deduped))

(define cleaned
  (time
  (for/hash ([key (in-table-column session-datas 'key)])
    (values key (clean-session session-datas key)))))

(define session-nums
  (for/list ([uid (in-table-column session-starts 'userid)])
    (length (table-ref session-starts 'userid 'key uid))))

(define max-num-sessions (apply max session-nums))
(for ([i (in-range (add1 max-num-sessions))])
  (printf "users with ~v sessions: ~v\n"
          i
          (length (filter (λ (n) (= n i)) session-nums))))

(printf "mean sessions per uid: ~v"
        (mean session-nums))

;; plot the times of participation of each user
#;(block
 (define start-sets (for/list ([(uid sessions) (in-hash uid-session-starts)])
                      (map session-start-server-timestamp sessions)))
 (sort start-sets < #:key last))

;; given a list of 
#;(define ((box-dist training-str) datas)
  (define key (session-data-key (last datas)))
  (define last-entry (session-data-content (last datas)))
  (list last-entry (string-levenshtein training-str last-entry)))


;; printout of first and finals
#;(for/list ([(uid sessions) (in-hash uid-sessions)])
  (define str (hash-ref uid->training-str-table uid))
  (list uid str (map (box-dist str) (map first sessions))))

;; I REALLY DO NOT HAVE TIME TO IMPLEMENT INNER JOIN.
;; I REALLY SHOULD JUST HAVE USED SQLITE.

(define user-traces
  (for/list ([userid (in-table-column session-starts 'userid)])
    (define keys-by-time
      (sort (filter (λ (kt) (hash-has-key? cleaned (vector-ref kt 0)))
                    (select-where session-starts '(key server-timestamp)
                                  `((userid ,userid))))
            <
            #:key (λ (v) (vector-ref v 1))))
    (define training-str
      (first (table-ref session-starts 'userid 'training-str userid)))
    (list
     (string-length training-str)
     (for/list ([kt (in-list keys-by-time)])
       (match-define (vector key ts) kt)
       (define last-entry
         (last (first (hash-ref cleaned key))))
       (vector ts
               (/ (string-levenshtein (vector-ref last-entry 1)
                                      training-str)
                  (string-length training-str)))))))

(define user-plot-pairs
  (for/list ([lp (in-list user-traces)])
    (match-define (list pstrlen user-data) lp)
    (list
     (lines user-data
            #:color
            (cond [(< pstrlen PASSWORD-LENGTH-THRESHOLD) 0]
                  [else 1]))
     (points user-data))))

(plot
 #:width 1300
 (apply
  append
  user-plot-pairs))

(for/list ([i 10])
  (plot
   #:width 1300
   (list-ref user-plot-pairs i)))


(write-to-file user-traces "/tmp/timesequences.rktd"
               #:exists 'truncate)


#;(







#;(
(for/list ([key (in-list session-keys)]
           
           #:when (hash-ref data key #f))
  (define s (hash-ref data key))
  (map length (sort (group-by fifth s) < #:key (λ (x) (fifth (first x)))))
  #;(first (sort s > #:key session-data-timestamp))
  )

(define zzpass (findf (λ (x) (equal? (fourth x) "0vC8R3lvzro")) session-starts))
(define zz (sort (hash-ref data "0vC8R3lvzro") < #:key fourth))
(call-with-output-file "/tmp/attempts.txt"
  #:exists 'truncate
  (λ (port)
    (for ([l (in-list (map sixth zz))])
      (fprintf port "~a\n" l))))))