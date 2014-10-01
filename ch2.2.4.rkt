#lang racket
;; Для графонов нужна эта библиотека
(require (planet soegaard/sicp:2:1/sicp))
; Картинку клади вместе с исходниками
;(define bart 
;  (load-painter 
;   (build-path (current-directory)
;               "bart.png")))
;(paint bart)

;(paint (beside einstein (flip-vert einstein)))

;; -- 2.44------------------------------
;(define (up-split painter n)
;  (if (= n 0)
;  painter
;  (let ((smaller (up-split painter (- n 1))))
;    (below painter (beside smaller smaller)))))

;;-------------------------------------
;(define (right-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (right-split painter (- n 1))))
;        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;;-- 2.45 --------------
(define (split f1 f2)
  (lambda (v)
    (f1 v (f2 v v))))

(define right-split (split beside below))
(define up-split (split below beside))

;;-- 2.46 --------------
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))

;;-- 2.47 --------------
; variant 1
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame v)
  (car v))
(define (edge1-frame v)
  (cadr v))
(define (edge2-frame v)
  (caddr v))

; variant 2
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
; origin-frame  - unchanged
; edge1-frame   - unchanged
(define (edge2-frame v)
  (cddr v))




