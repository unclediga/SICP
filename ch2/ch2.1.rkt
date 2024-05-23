(define (make-rat n d)
  (cons n d))
(define (numer x)
  (car x))
(define (denom x)
  (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
;; -- 2.1 --------------------------------------
(define (make-rat n d)
  (if (and (< (* n d) 0) (< d 0))
      (cons (* -1 n) (* -1 d))
      (cons n d)))


(define (mul-rat x y)
  (let ((n (* (numer x) (numer y)))
        (d (* (denom x) (denom y))))
    (if (and (< n 0) (< d 0))
        (make-rat (* -1 n)(* -1 d))
        (make-rat n d))))
;; -- 2.2 --------------------------------------
(define (make-segment x1 y1 x2 y2)
  (cons x1 (cons y1 (cons x2 y2))))
(define (start-segment s)
  (cons (car s) (cadr s)))
(define (end-segment s)
  (cons (caddr s) (cdddr s)))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (mid-point s)
  (let ((ave (lambda(x1 x2)(+ (/ (- x2 x1) 2.0) x1))))
    (make-point (ave (x-point (start-segment s)) (x-point (end-segment s)))
                (ave (y-point (start-segment s)) (y-point (end-segment s))))))
;; -- 2.3 --------------------------------------
;; -- 2.4 --------------------------------------
(define (mycons x y)
  (lambda (m) (m x y)))
(define (mycar z)
  (z (lambda (p q) p)))
;;(mycar (mycons 1 2))
;;(mycar (lambda (m) (m 1 2)))
;;(  (lambda (m) (m 1 2)) (lambda (p q) p) )
;;((lambda (p q) p) 1 2) 
;; 1
(define (mycdr z)
  (z (lambda (p q) q)))
;; -- 2.5 -------------------------------------
(define (cons2 a b)
  (* (expt 2 a) (expt 3 b)))

(define (car2 2a3b)
  (define (max-pow x divis)
    (if (= (remainder x divis) 0)
        (+ 1 (max-pow (/ x divis) divis))
        0))
  (max-pow 2a3b 2))

(define (cdr2 2a3b)
  (define (max-pow x divis)
    (if (= (remainder x divis) 0)
        (+ 1 (max-pow (/ x divis) divis))
        0))
  (max-pow 2a3b 3))
;; -- 2.6 ------------------------------------
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; =========== ch 2.1.4 ===========================
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
;; -- 2.7 ----------------------------------------
(define (make-interval a b) (cons a b))

(define (upper-bound int)
  (max (car int) (cdr int)))

(define (lower-bound int)
  (min (car int) (cdr int)))


(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))


;; -- 2.12 ---------------------------------------------
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (make-center-percent c prc)
  (let ((w (* c (/ prc 100))))
    (make-center-width c w)))
  



















