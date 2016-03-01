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


;; these users are just a consequence of testing... ignore them.
(define bogus-users
  (list "consenter"
        "nonconsenter"
        "guest"
        "clements"))

;; likewise
(define bogus-session-keys
  (list "7728koh"))

;; this string occurs once and appears to be some kind of auto-paste:
(define bogus-boxentries
  (list "(string->symbol (randomTerm maxDepth))"))

;; represents a start-of-session log line
(struct session-start [server-timestamp userid key training-str] #:transparent)
(define session-start-col-names '(server-timestamp userid key training-str))
;; represents a keystroke log line
(struct session-data [server-timestamp key timestamp boxnum content] #:transparent)
(define session-data-col-names '(server-timestamp key timestamp boxnum content))

(define-runtime-path here ".")

(define USERS-LIST-FILE (build-path here "users.rktd"))

(when (not (file-exists? USERS-LIST-FILE))
  (error 'users-rktd "expected to find users list in users.rktd (should probably \
just discover these from logs)."))

(define users
  (file->value (build-path here "users.rktd")))

;; this salt is used to map uids to hashed strings, just to allow me to
;; look at the data without making names obvious
(define uid-salt #"BJ\313\374\342")

;; remove '=\r\n' from base64-encoding
(define (remove-last3 bytes)
  (match bytes
    [(regexp #px#"=\r\n$")
     (subbytes bytes 0 (- (bytes-length bytes) 3))]
    [other (error 'remove-last3
                  "unexpected last 3 chars")]))

;; a map from users to anonymized ids
(define user-hash
  (for/hash ([u (in-list users)])
    (values u
            (~> u
                string->bytes/utf-8
                (bytes-append uid-salt #"a")
                sha224
                (subbytes 0 2)
                base64-encode
                remove-last3
                bytes->string/utf-8))))

(unless (= (length (hash-keys user-hash))
           (length (remove-duplicates (hash-values user-hash))))
  (error 'user-hash "hash collisions! two users have the same hash.\n"))

(define raw-lines
  (file->lines "/tmp/log10"))

;; map a line to either a session-start or a session-data (or false
;; if it's bogus)
(define (parse-line l)
  (match (regexp-match #px"^@([0-9a-f]{24}) (.*)" l)
    [#f (error 'parse-line "missing tai64 tag on line: ~v\n" l)]
    [(list _1 tai64n  content)
     (define readed (call-with-input-string content read))
     (match readed
       [(list 'session-start (? string? userid) (? string? session-key)
              (? string? training-str))
        (cond [(or (member userid bogus-users)
                   (member session-key bogus-session-keys))
               (printf "ignoring bogus session start with key ~v from user ~v\n"
                       session-key userid)]
              [else
               (session-start (~> tai64n
                                  parse-tai64n
                                  time-second)
                              (hash-ref user-hash userid)
                              session-key training-str)])]
       [(list 'session-data (? string? session-key) (? number? timestamp)
              (? number? box-num) (? string? box-content))
        (cond [(member session-key bogus-session-keys)
               (printf "ignoring bogus session data with key ~v"
                       session-key)]
              [else
               (session-data (time-second
                              (parse-tai64n
                               tai64n))
                             session-key timestamp box-num box-content)])]
       ;; old style, ignore:
       [(list 'session-start (or "guest" "clements" "guest2") (? string? sessionkey))
        #f]
       [(list 'session-data (or "guest" "clements")
              (? string? session-key) (? number? timestamp)
              (? number? box-num) (? string? box-content))
        #f]
       [other
        (printf "ignoring bogus line: ~v\n" l)
        #f])]))

;; parse all the lines, throw out the falses:
(define log-lines (filter (λ (x) x) (map parse-line raw-lines)))

(define session-starts
  (make-table
   session-start-col-names
   (map struct->list (filter session-start? log-lines))))

(add-index! session-starts '(userid))

(define session-keys
  (in-table-column session-starts 'key)
  #;(map session-start-key session-starts))

(define session-datas
  (make-table
   session-data-col-names
   (map
    struct->list
    (filter (λ (sd)
              (and
               (session-data? sd)
               (not (member (session-data-content sd)
                            bogus-boxentries))))
            log-lines))))

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

(define user-plot-pairs
  (for/list ([userid (in-table-column session-starts 'userid)])
    (define keys-by-time
      (filter (λ (kt) (< (vector-ref kt 1) 1455800000))
      (sort (filter (λ (kt) (hash-has-key? cleaned (vector-ref kt 0)))
                    (select-where session-starts '(key server-timestamp)
                                  `((userid ,userid))))
            <
            #:key (λ (v) (vector-ref v 1)))))
    (define training-str
      (first (table-ref session-starts 'userid 'training-str userid)))
    (define one-users-data
      (for/list ([kt (in-list keys-by-time)])
        (match-define (vector key ts) kt)
        (define last-entry
          (last (first (hash-ref cleaned key))))
        
        (vector (+ ts (random 300))
                (+ (/ (- (random 20) 10) 20)
                   (string-levenshtein (vector-ref last-entry 1)
                                       training-str)))))
    (list
     (lines one-users-data
            #:color
            (cond [(< (string-length training-str) PASSWORD-LENGTH-THRESHOLD) 0]
                  [else 1]))
     (points one-users-data))))

(plot
 #:width 1300
 (apply
  append
  user-plot-pairs))

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