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

;; -- 2.21 ------------------------------
(define (square-list1 items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list1 (cdr items)))))
;; -------------
(define (square-list2 items)
  (map (lambda(x)(* x x)) items))
;; -- 2.22 ------------------------------
;; NO WORK
(define nil '())
(define (square x)(* x x))
(define (square-listHugo1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))
;; NO WORK
(define (square-listHugo2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))
;; GOOD ---
(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (append answer (list (square (car things)))))))
  (iter items nil))

;; -- 2.23 -------------------------------
(define (my-for-each f lst)
  (if (null? lst)
      nil
      (cons (f (car lst))(f (cdr lst))))
  nil)
;; with 'cond' you may excecute more then one form
;; with 'if'  - single only
(define (your-for-each proc items)
  (cond ((not (null? items))
         (proc (car items)) (your-for-each proc (cdr items)))))
;; -- 2.27 -------------------------------
(define (deep-reverse lst)
  (define (rev l r)
    (if (null? l)
        r
        (rev (cdr l) (cons (if (pair? (car l))
                               (deep-reverse (car l)) 
                               (car l))
                           r))))
  (rev lst '()))

;; -- 2.28 ---------------------------------
(define (fringe x)
  (cond ((null? x) '())
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))
;; -- 2.29 ---------------------------------
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; -- (a) --
(define (left-branch mob)
  (if (pair? mob)
      (car mob)
      nil))

(define (right-branch mob)
  (if (pair? mob)
      (cadr mob)
      nil))

(define (branch-length br)
  (car br))

(define (branch-structure br)
  (cadr br))

;; -- (b) --

(define (branch-weight br)
  (mobile-weight (branch-structure br)))


(define (mobile-weight mob)
  (if (pair? mob)
      (+ (branch-weight (left-branch mob))
         (branch-weight (right-branch mob)))
      mob))

;; 
(define (total-weight mobile)
  (mobile-weight mobile))

;(total-weight
; (make-mobile (make-branch 2 
;                           (make-mobile (make-branch 1 1)
;                                        (make-branch 1 
;                                                     (make-mobile (make-branch 1 10)
;                                                                  (make-branch 2 5)))))
;              (make-branch 2 
;                           (make-mobile (make-branch 1 1)
;                                        (make-branch 1 2)))))

;(total-weight
; (make-mobile (make-branch 1 1)
;              (make-branch 1 2)))
;; -- (c) --

(define (mobile-balansed? mob)
  (let ((left-b (left-branch mob))
        (right-b (right-branch mob)))
    (if (pair? mob)
        (and 
         (= 
          (* (branch-length left-b) (branch-weight left-b))
          (* (branch-length right-b) (branch-weight right-b)))
         (mobile-balansed? (branch-structure left-b))
         (mobile-balansed? (branch-structure right-b)))
        #t)))


;(mobile-balansed? 
; (make-mobile (make-branch 2 
;                           (make-mobile (make-branch 1 15)
;                                        (make-branch 1 
;                                                     (make-mobile (make-branch 1 10)
;                                                                  (make-branch 2 5)))))
;              (make-branch 2 
;                           (make-mobile (make-branch 1 15)
;                                        (make-branch 1 15)))))

;; ---------------------------------------------------
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;;-- 2.30 -----------------------------------
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

;(square-tree
; (list 1
;       (list 2 (list 3 4) 5)
;       (list 6 7)))

;; -- 2.31 ---------------------------------
(define (tree-map fn tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map fn sub-tree)
             (fn sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

;; -- 2.32 --------------------------------

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (ss)(cons (car s) ss) )  rest)))))
;; -- 2.33 --------------------------------
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; -- 2.34 --------------------------------
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) h??i)
              0
              coefficient-sequence))

;; 1 + 3*x + 5*x^3 + x^5
;; (((((x + 0) * x + 5) * x + 0) * x + 3) * x + 1)



(horner-eval 2 (list 1 3 0 5 0 1))