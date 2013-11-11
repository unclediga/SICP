;; =====================================
(define (my-list-ref items n)
  (if (= n 0)
      (car items)
      (my-list-ref (cdr items) (- n 1))))

(define (my-append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (my-append (cdr lst1) lst2))))
;; -- 2.17 ------------------------------
(define (last-pair lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (car lst))
        (else (last-pair (cdr lst)))))
;; -- 2.18------------------------------
(define (my-reverse lst)
  (define (rev l r)
    (if (null? l)
        r
        (rev (cdr l) (cons (car l) r))))
  (rev lst '()))
;; -- 2.19 ------------------------------
;(define (count-change amount)
;  (cc amount 5))
;(define (cc amount kinds-of-coins)
;  (cond ((= amount 0) 1)
;        ((or (< amount 0) (= kinds-of-coins 0)) 0)
;        (else (+ (cc amount
;                     (- kinds-of-coins 1))
;                 (cc (- amount
;                        (first-denomination kinds-of-coins))
;                     kinds-of-coins)))))
;(define (first-denomination kinds-of-coins)
;  (cond ((= kinds-of-coins 1) 1)
;        ((= kinds-of-coins 2) 5)
;        ((= kinds-of-coins 3) 10)
;        ((= kinds-of-coins 4) 25)
;        ((= kinds-of-coins 5) 50)))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (except-first-denomination  c-v)
  (cdr c-v))

(define (first-denomination  c-v)
  (car c-v))

(define (no-more? c-v)
  (null? c-v))

;; -- 2.20 ------------------------------
(define (same-parity x . z)
    (define (ev-odd? itm)
      (if (even? x)
          (even? itm)
          (odd? itm)))
  (define (s-p lst)
    (cond ((null? lst) '())
          ((ev-odd? (car lst)) (cons (car lst) (s-p (cdr lst))))
          (else (s-p (cdr lst)))))
  (s-p z))

