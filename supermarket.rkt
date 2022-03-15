#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

(define-struct counter (index tt et queue open) #:transparent)

(define (empty-counter index)
  (define COUNTER (make-counter index 0 0 empty-queue #t))
  ; campul "true" spune ca acest counter este deschis 
  COUNTER)
   
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei

(define (update f counters index)
  (cond ((null? counters) counters)
        ((null? (filter (lambda (C) (= (counter-index C) index)) counters)) counters)
        (else (define C (struct-copy counter (f (first (filter (lambda (C) (= (counter-index C) index)) counters)))))
              (cond ((>= (length counters) index)
              (append (take counters (- index 1)) (list C) (drop counters index)))))))

(define (result counters listaa)
  ; returneaza lista cozilor care au clienti 
  (cond  ((null? counters) listaa)
        ((not (queue-empty? (counter-queue (first counters))))
         (result (rest counters) (append listaa (list (cons (counter-index (first counters)) (counter-queue (first counters)))))))
        (else (result (rest counters) listaa))))

(define (add-to-counter name items)
  ; adauga o persoana la casa C 
   (λ (C)
     (if (equal? (counter-open C) #t)
     (cond ((queue-empty? (counter-queue C))
            (define C2 (struct-copy counter C [tt (+ (counter-tt C) items)] [queue (enqueue (cons name items) (counter-queue C))]
            [et (+ (counter-et C) items)])) C2)
            (else (define C2 (struct-copy counter C [tt (+ (counter-tt C) items)] [queue (enqueue (cons name items) (counter-queue C))])) C2))
     C)))

(define (abstract counters parameter)
  (cond
      ((null? counters) '(0 0))
      ((null? (rest counters)) (cons (counter-index (first counters)) (parameter (first counters))))
      ; daca lista are un singur element, intoarce acel element 
      ((<= (parameter (first counters)) (cdr (abstract (rest counters) parameter))) (cons (counter-index (first counters)) (parameter (first counters))))
      ; daca primul element din lista are tt-ul mai mic decat al tuturor celorlalte elemente din lista
      ; se returneaza acel prim counter 
      (else (abstract (rest counters) parameter))))
  
(define (min-tt counters) (abstract counters counter-tt)) 
(define (min-et counters) (abstract counters counter-et))

(define (bestCounter counters)
  ; intoarce casa cu cel mai bun tt din lista si care este deschisa si are indexul cel mai mic!! 
  (cond ((null? counters) (empty-counter 0))
         ((and (= (counter-tt (first counters)) (cdr (min-tt (open-counters counters '())))) (eq? (counter-open (first counters)) #t))
      (first counters))
      (else (bestCounter (rest counters)))))


(define tt+
  (lambda (minutes)
    (lambda (C)  
      (define C2 (struct-copy counter C [tt (+ (counter-tt C) minutes)]))
  C2)))

(define et+
 (lambda (minutes)
    (lambda (C)  
      (define C2 (struct-copy counter C [et (+ (counter-et C) minutes)]))
  C2)))

(define (thisCounter counters index)
  ; intoarce casa care are acest index 
  (cond ((null? counters) (empty-counter 0))
    ((= (counter-index (first counters)) index)
      (first counters))
      (else (thisCounter (rest counters) index))))

(define (find-best-et counters)
  ; returneaza cel mai bun et-care nu este 0 al unei case cu clienti 
  (cond ((null? counters) 0)  
      ((or (= (cdr (min-et counters)) 0) (queue-empty? (counter-queue (thisCounter counters (car (min-et counters)))))) 
      (find-best-et (remove (thisCounter counters (car (min-et counters))) counters)))
      (else (cdr (min-et counters)))))

(define (pass-time-through-counter minutes)
  (λ (C)
    (cond ((queue-empty? (counter-queue C))
           ; daca nu am clienti
           (cond ((> (counter-tt C) minutes)
                  (define C2 (struct-copy counter C [tt (- (counter-tt C) minutes)] [et (- (counter-et C) minutes)]))
                  C2)
                 (else (define C2 (struct-copy counter C [tt 0] [et 0])) C2)))
    (else (define C2 (struct-copy counter C [tt (- (counter-tt C) minutes)] [et (- (counter-et C) minutes)]))
    C2))))

(define (find-tt C)
  ; calculeaza tt-ul unei case fara intarzieri 
  (cond
    ((queue-empty? (counter-queue C)) 0)
    (else (define C2 (struct-copy counter C [queue (dequeue (counter-queue C))])) (+ (cdr (top (counter-queue C))) (find-tt C2)))))

(define (second-remove-first-from-counter listaaa)
  (λ (C)
  (if (not (equal? (member C listaaa) #f))
      ; daca casa face parte din lista, aplic functia pe ea, altfel doar actualizez tt-ul si et-ul  
  (cond
    ((queue-empty? (counter-queue C)) C)
    ((queue-empty? (dequeue (counter-queue C)))
       (define C2 (struct-copy counter C [queue (dequeue(counter-queue C))] [tt 0] [et 0])) C2)
    (else (define C2 (struct-copy counter C [queue (dequeue (counter-queue C))] [et (cdr (top (dequeue (counter-queue C))))]))
          (define C3 (struct-copy counter C2 [tt (find-tt C2)])) C3))
  ((pass-time-through-counter (counter-et (car listaaa))) C))))

(define (counters-best-et counters et listaa)
  ; returneaza o lista cu toate casele care au best-et
  ; si care au clienti in coada!!!
  (cond  ((null? counters) listaa)
        ((and (= et (counter-et (first counters))) (not (queue-empty? (counter-queue (first counters)))))
       (counters-best-et (rest counters) et (append listaa (list (first counters)))))
        (else (counters-best-et (rest counters) et listaa))))

(define (pass-time minutes fast-counters slow-counters listaa)
  ; actualizare tt, et si queue dupa trecerea a "minutes" minute asupra unei liste de case
  (cond ((= (find-best-et (append fast-counters slow-counters)) 0)
         ; nu exista case cu clienti 
         (list listaa (map (pass-time-through-counter minutes) fast-counters) (map (pass-time-through-counter minutes) slow-counters)))
    ((< minutes (find-best-et (append fast-counters slow-counters)))
         ; daca "minutes" < best_et
         ; nu iese niciun client din coada si actualizez tt-ul si et-ul tuturor caselor
         (list listaa (map (pass-time-through-counter minutes) fast-counters) (map (pass-time-through-counter minutes) slow-counters)))
        (else
         ; scot primul client de la coada tuturor caselor care au best_et si actualizez et-ul si tt-ul tuturor caselor  
         (pass-time (- minutes (find-best-et (append fast-counters slow-counters)))
                    (map (second-remove-first-from-counter (counters-best-et (append fast-counters slow-counters)
                                                                             (find-best-et (append fast-counters slow-counters)) '())) fast-counters)
                    (map (second-remove-first-from-counter (counters-best-et (append fast-counters slow-counters)
                                                                             (find-best-et (append fast-counters slow-counters)) '())) slow-counters)
                    (append listaa (map cons (map counter-index (counters-best-et (append fast-counters slow-counters)
                    (find-best-et (append fast-counters slow-counters)) '())) (map car (map top
                    (map counter-queue (counters-best-et (append fast-counters slow-counters)
                    (find-best-et (append fast-counters slow-counters)) '()))))))))))

(define (tt-average sum counters)
  (/ sum (length counters)))

(define (tt-sum counters)
  (if (null? counters)
      0
      (+ (counter-tt (first counters)) (tt-sum (rest counters)))))

(define (open-counters counters listaa)
  ; intoarce o lista cu toate casele deschise sortate dupa index
  (cond ((null? counters) listaa)
        ((equal? (counter-open (first counters)) #t)
         (open-counters (rest counters) (append listaa (list (first counters)))))
        (else
         (open-counters (rest counters) listaa))))
  
(define (add-counters nr-counters listt index)
  ; intoarce lista de case adaugate pentru a se ajunge la un timp <= average
  (cond ((= nr-counters 0)
             listt) 
        (else
             (add-counters (- nr-counters 1) (append listt (list (empty-counter index))) (+ index 1)))))

(define (needed-counters sum average number-counters)
  ; calculeaza numarul de case care ar trebui adaugate
  (cond ((= (remainder sum average) 0)
            (floor (- (/ sum average) number-counters)))
        (else
            (+ 1 (floor (- (/ sum average) number-counters))))))

(define (close-counter counters index)
; inchide casa cu acest index si intoarce lista de case actualizata 
  (define C (struct-copy counter (thisCounter counters index) [open #f]))
  (append (take counters (- index 1)) (list C) (drop counters index)))

(define (serve-helper requests fast-counters slow-counters listaaa)
(cond ((null? requests) (cons (remove-duplicates listaaa) (result (append fast-counters slow-counters) '())))
    ((and (equal? (list? (first requests)) #t) (= (length (first requests)) 2)
          (not (equal? (car (first requests)) 'ensure)) (not (equal? (car (first requests)) 'close)))
    ; persoana trebuie asezata la o casa cu tt minim
       (cond ((> (cadr (first requests)) ITEMS)
              ; clientul poate fi adaugat doar la o casa slow
              (serve-helper (rest requests) fast-counters (drop (update (add-to-counter (car (first requests)) (cadr (first requests)))
                     (append fast-counters slow-counters) (counter-index (bestCounter slow-counters))) (length fast-counters)) listaaa)) 
             (else
              ; clientul poate fi adaugat la toate casele
              (define index (counter-index (bestCounter (append fast-counters slow-counters))))
              (if (< index (counter-index (first slow-counters)))
                  ; adaug intr-un fast-counter
                  (serve-helper (rest requests) (take (update (add-to-counter (car (first requests)) (cadr (first requests)))
                  (append fast-counters slow-counters) index) (length fast-counters)) slow-counters listaaa)
                  ; adaug intr-un slow-counter
                  (serve-helper (rest requests) fast-counters (drop (update (add-to-counter (car (first requests)) (cadr (first requests)))
                  (append fast-counters slow-counters) index) (length fast-counters)) listaaa)))))
    ((and (equal? (list? (first requests)) #t) (= (length (first requests)) 3))
      ; casa index trebuie intarziata cu "minutes" minute
      (cond ((< (cadr (first requests)) (counter-index (first slow-counters))) 
            ; intarzii o casa fast 
              (serve-helper (rest requests) (take (update (et+ (caddr (first requests))) (update (tt+ (caddr (first requests)))
              (append fast-counters slow-counters) (cadr (first requests))) (cadr (first requests))) (length fast-counters)) slow-counters listaaa))
            ; intarzii o casa slow
            (else
              (serve-helper (rest requests) fast-counters (drop (update (et+ (caddr (first requests))) (update (tt+ (caddr (first requests)))
              (append fast-counters slow-counters) (cadr (first requests))) (cadr (first requests))) (length fast-counters)) listaaa))))
    ((and (= (length (list (first requests))) 1) (not (equal? (list? (first requests)) #t)))
     ; trec "x" minute
      (serve-helper (rest requests) (cadr (pass-time (first requests) fast-counters slow-counters listaaa))
             (caddr (pass-time (first requests) fast-counters slow-counters listaaa))
             (append listaaa (car (pass-time (first requests) fast-counters slow-counters listaaa)))))      
    ((and (equal? (list? (first requests)) #t) (= (length (first requests)) 2) (equal? (car (first requests)) 'ensure))
    ; request de tip "ensure"
      (cond ((> (tt-average (tt-sum (open-counters (append fast-counters slow-counters) '())) (open-counters (append fast-counters slow-counters) '()))
                (cadr (first requests)))
      ; daca este nevoie sa se adauge case
              (serve-helper (rest requests) fast-counters (add-counters (needed-counters (tt-sum (open-counters (append fast-counters slow-counters) '()))
              (cadr (first requests)) (length (open-counters (append fast-counters slow-counters) '())))
              slow-counters (+ 1 (counter-index (last slow-counters)))) listaaa))
      (else
              (serve-helper (rest requests) fast-counters slow-counters listaaa))))
    ((and (equal? (list? (first requests)) #t) (= (length (first requests)) 2) (equal? (car (first requests)) 'close))
     (cond ((< (cadr (first requests)) (counter-index (first slow-counters)))
            ; daca se inchide o casa fast
              (serve-helper (rest requests) (take (close-counter (append fast-counters slow-counters) (cadr (first requests)))
                                                  (- (counter-index (first slow-counters)) 1)) slow-counters listaaa))
           (else
            ; daca se inchide o casa slow
              (serve-helper (rest requests) fast-counters (drop (close-counter (append fast-counters slow-counters) (cadr (first requests)))
                                                                (- (counter-index (first slow-counters)) 1))listaaa))))))

(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '()))

