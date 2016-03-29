#lang racket

(require levenshtein
         plot
         math)

(define responses
  (regexp-split #px"\n"
                #;#<<|
Thint exam a gul.
rommese insfirec
zm<yr2ih{
AGnB,\a}
A)WAr]ASy
It--Mad hus, sigho a
it so Roymbred. B
thea
ijJTO&0
onging down the o
GZItnh!mq
qWx/OJcj
chin toon husewhis
a mentron tons Prenv
-3F50<rFj
0sG0wMfea
condily flow to hade
xX$0$&NAT
R!gSIAY<y
py)Uth1f
}3jyI->Jq
BiD.m4n<m
ingain whompood shad a
thisomigornis fors
wNiAgg92R
wit hatudissect sa
M@l/.6kdr
darry wing. A minse ti.
thea
|
                #<<|
859*S-UJR
@X-GU+9Tm
jn*biwgFE
.JhUvUqdg
I.jEMZZ>d
Strubt, ang tho.
II. I plented; andst
Mrs not pary, toncon,
_heretterhalluch a
UaI06Hj3N
lfxOAg087
rekt something something
the pat and affiess, on t
4hOI#n}h
nKQbr?,L
covotheng to knigre a
Shourfe, wor Madi
cappeop fis,_in hatt
gwer Mrs', ste as ap
*KMQvrMto
tilivil of tor hountr
vere' onenere! s
!klk=^P$,
cU}7.B8x/
kingastion here tore
,k>!m[rT
6O*KkHO[/
to so faccartaing that a
on. Calls. Bustoong
-Qtp,(Qbt
m trounto monoun ow
try stardid Do ita
Cl<ertgy
$g3jt&0j=
Mr. Ugh farseep one,
ovagairiaging bach
/R.g$<K+3

|
  ))

;;NB: ADD THEA BACK IN:

;; inspecting the correct responses, these
;; are adjusted for what are almost certainly
;; my inability to read handwriting, for instance
;; the replacement of '5' by 'S'.
(define adjusted-responses
  (regexp-split #px"\n"
                #<<|
thea
Thint exam a gul.
rommese insfirec
zm<yr2ih{
AGnB,\a}
A)WAr]A5y
It--Mad hus, sigho a
it so Roymbred. B
xjJTO&0
onging down the o
GZItnh!mq
qWx/0Jcj
chin toon husewhis
a mention tons Prenv
-3F50<rFj
0sG0wMfea
condily flow to hade
xX&0$&NAT
R!gSIAY<y
py)Uth1f
}3jyI->Jq
BiD.m4n<m
ingain whompood shad a
thisomigornis fors
wNiAgg92R
Wit hatudissect sa
M@l/.6kdr
darry wing. A minse ti.
859*S-UJR
@x-GUt9Tm
jn*bmgFE
.JhUvUqdg
I.jEMZZ>d
Strubt, ang tho.
II. I plented; andst
Mrs not pary, toncon,
_heretterhalluch a
UaIO6Hk}N
lfxOAg087
rekt something something
the pat and offiess, on t
4hOI#n}h
nKQbr?,L
covotheng to knigre a
shourfe, wor Madi
cappeop fis,_in hatt
swer Mys', ste as ap
*KMQveM+o
tilivil of tor hountr
vere' onenere! s
!klk=^P$,
cU}7.B8x/
kingastion here tore
,K>!m[rT
6O*KkHO[/
to so faccortaing that a
on. Calls. Bustoong
-Qtp,(Qbt
m trounto monown ow
try stardid Do ita
Cl<ertgy
$g3j+&0j=
Mr. Ugh farseep one,
ovagairiaging bach
/R.g$<K+3
|
  ))

(define input-lines
  (with-input-from-file "/tmp/molis-hai-2162-experiment-sessions.tsv"
    (λ ()
      (for/list ([l (in-lines (current-input-port))])
        (regexp-split #px"\t" l)))))

(define correct-responses (remove-duplicates (map fourth input-lines)))

;; for some responses, the correct match is not the one with the smallest
;; levenshtein distance. This "patch-up" list enforces certain matches
(define manual-matches
  '(("Cl<ertgy" . "+ox/cl>rg")
    ("thea" . "fall ever thad therve thei")
    ("rekt something something" . "repery therst romet a")
    ("-Qtp,(Qbt" . "Q+t0P,t(B")))

(define paired-responses
  (for/list ([r (in-list adjusted-responses)])
    (define best-match
      ;; does it appear in the list of manual-matches?
      (match (dict-ref manual-matches r #f)
        [(? string? s) (list (string-levenshtein r s) s)]
        [#f (argmin first
                  (for/list ([c (in-list correct-responses)])
                    (list (string-levenshtein r c) c)))]))
    (list r best-match)))

paired-responses



(define passwords-taken (map second (map second paired-responses)))
(length passwords-taken)


(define passwords-not-taken (remove* passwords-taken correct-responses))

(define (experimental-group? r)
  (< 13 (string-length (second (second r)))))
(define (not-experimental-group? r)
  (not (experimental-group? r)))

(define experimental-responses (filter experimental-group? paired-responses))
(define control-responses (filter not-experimental-group? paired-responses))

(define (rel-error r)
  (/ (first (second r))
     (string-length (second (second r)))))


(define bandwidth 0.2)

(plot
 (list
  (density (map rel-error experimental-responses)
         bandwidth
         #:color 2
         #:label "experimental")

  (density (map rel-error control-responses)
         bandwidth
         #:label "control")))


(define (success-rate responses)
  (/ (length (filter (λ (r)
                       (= (first (second r)) 0)) responses))
     (length responses)))

(success-rate experimental-responses)
(success-rate control-responses)

(define p-hat (success-rate paired-responses))

(define z-value
  (/ (- (success-rate experimental-responses)
        (success-rate control-responses))
     (sqrt
      (* p-hat (- 1 p-hat)
         (+ (/ 1 (length experimental-responses))
            (/ 1 (length control-responses)))))))
(printf "z value: ~v\n" z-value)

(define p-value
  (- 1.0 ((ordered-dist-cdf (normal-dist 0 1.0)) z-value)))
(printf "likelihood that null hypothesis is true (out of 1): ~v\n"
        p-value)