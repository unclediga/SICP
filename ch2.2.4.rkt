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

