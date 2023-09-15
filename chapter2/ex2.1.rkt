#lang racket

;(load "ratio.rkt")
(load "assa.rkt")

;; -- 2.1 --------------------------------------
(define (make-rat n d)
  (if (and (< (* n d) 0) (< d 0))
      (cons (* -1 n) (* -1 d))
      (cons n d)))

