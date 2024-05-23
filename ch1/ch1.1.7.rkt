;; Задача про вычисление квадратного корня методом Ньютона
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.0001))

(define (improve guess x)
  (average (/ x guess) guess))

(define (average x y)
  (/ (+ x y) 2))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))

(define (square x)
  (* x x))

;; Упр. 1.6 Паркер и спецформа if
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter2 guess x)
  (new-if (good-enough2? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(define (good-enough2? guess x)
  (new-if (< (abs (- (* guess guess) x)) 0.0001) 
          #t 
          #f))
(define (my-sqrt2 x)
  (sqrt-iter2 1.0 x))

;; Упражнение 1.7.

(define (good-enough_v2.0? guess prev-guess x)
  (< (abs (- prev-guess guess)) 0.0001))


(define (sqrt-iter_v2.0 guess prev-guess x)
  (if (good-enough_v2.0? guess prev-guess x)
      guess
      (sqrt-iter_v2.0 (improve guess x) guess x)))

(define (my-sqrt_v2.0 x)
  (sqrt-iter_v2.0 1.0 0.0 x))

;; Упражнение 1.8.

(define (cube x)
  (* x x x))

(define (cube-sqrt guess x)
  (display (improve-cube-guess guess x))
  (newline)
  (if (good-enough-cube? guess x)
      guess
      (cube-sqrt (improve-cube-guess guess x) x)))

(define (improve-cube-guess guess x)
  (/ (+ (/ x (* guess guess)) (* 2.0 guess)) 3.0))

(define (good-enough-cube? guess x)
  (< (abs (- (cube guess) x)) 0.0001))

(define (good-enough-common? guess x fn)
  (< (abs (- (fn guess) x)) 0.0001))
