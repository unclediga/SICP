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

;; -- 2.20 ------------------------------
(define (same-parity x . z))
  