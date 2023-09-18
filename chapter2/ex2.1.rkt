#lang sicp

(#%require "ratio.rkt")

;; -- 2.1 --------------------------------------

(define (neg n)
  (* -1 n))

(define (make-rat n d)
  (let ((n2 (if (negative? d) (neg n) n))
        (d2 (if (negative? d) (neg d) d)))
    (cons n2 d2)))


(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

