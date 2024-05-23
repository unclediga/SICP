#lang sicp

;; The language #lang sicp provides you with a version of R5RS (the fifth revision of Scheme)
;; changed slightly in order for programs in SICP to run as is.
;; https://standards.scheme.org/corrected-r5rs/r5rs-Z-H-2.html#%_toc_start

(#%provide (all-defined))

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
