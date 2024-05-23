#lang sicp

;Упражнение 2.2.
;Рассмотрим задачу представления отрезков прямой на плоскости. Каждый отрезок представ-
;ляется как пара точек: начало и конец. Определите конструктор make-segment и селекторы
;start-segment и end-segment, которые определяют представление отрезков в терминах точек.
;Далее, точку можно представить как пару чисел: координата x и координата y. Соответствен-
;но, напишите конструктор make-point и селекторы x-point и y-point, которые определяют
;такое представление. Наконец, используя свои селекторы и конструктор, напишите процедуру
;midpoint-segment, которая принимает отрезок в качестве аргумента и возвращает его середину
;(точку, координаты которой являются средним координат концов отрезка). Чтобы опробовать
;эти процедуры, Вам потребуется способ печатать координаты точек: (define (print-point p)

(define (make-segment p1 p2)
  (cons p1 (cons p2 '())))

(define (start-segment s)
  (car s)) 

(define (end-segment s)
  (cadr s)) 
  
(define (point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))


(define (mid-point p1 p2)
  (let ((mid (lambda (x1 x2)
               (+ x1 (/ (- x2 x1) 2))))) 
    (point
     (mid (x-point p1) (x-point p2))
     (mid (y-point p1) (y-point p2)))))


(define (midpoint-segment s)
  (mid-point
   (start-segment s)
   (end-segment s)))
          

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))