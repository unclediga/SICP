;;****************************************************
;; Chapter 1.3 
;; High order functions
;;****************************************************

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
;;-----------------------------------------------
(define (cube x)
  (* x x x))
;;-----------------------------------------------
(define (inc x)
  (+ x 1))
;;-----------------------------------------------
(define (dec x)
  (- x 1))
;;-----------------------------------------------
(define (identity x) x)
;;-----------------------------------------------
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))
;;-----------------------------------------------
(define (integral f a b dx)
  (define (add-dx x)(+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx  b) 
     dx))

;;-- 1.29 ---------------------------------------

(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (koeff k)
      (cond ((or (= k 0) (= k n)) 1)
            ((even? k) 2)
            (else 4)))
    (define (f2 k)
      (*
       (f (+ a (* k h)))
       (koeff k)))
    (* (/ h 3) 
       (sum f2 0 inc n))))

;; -- 1.30 -------------------------------------
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
;; -- 1.31 --------------------------------------
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) 
         (product term (next a) next b))))
;; factorial 10!
;;(product identity 1 inc 10)
;; pi
(define (pi-prod n)
  (define (square x)(* x x))
  (define (next x)(+ x 2))
  (* 2.0 4.0
     ( /
       (product square 4 next (+ 4 (* n 2) ))
       (+ 4 (* n 2)))
     (/ 1.0
        (product square 3 next (+ 3 (* n 2) )))))
;; product-iter
(define (product-iter term a next b)
  (define (p-it x result)
    (if ( > x b)
        result
        (p-it (next x) (* x result))))
  (p-it a 1))

;; -- 1.32 ---------------------------------------------
(define (accumulate combiner null-val term a next b)
  (if (> a b)
      null-val
      (combiner a
                (accumulate combiner
                            null-val
                            term
                            (next a)
                            next 
                            b))))


;; -- 1.33 ---------------------------------------------
(define (filtered-accumulate combiner filter null-val term a next b)
  (cond ((> a b) null-val)
        ((filter a)
         (combiner a
                   (filtered-accumulate combiner
                                        filter
                                        null-val
                                        term
                                        (next a)
                                        next 
                                        b)))
        (else (filtered-accumulate combiner
                                   filter
                                   null-val
                                   term
                                   (next a)
                                   next 
                                   b))))

;; -- 1.34 ---------------------------------------------
(define (f g)
  (g 2))

;;-------------------------------------------------------
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.001))
  (define (try-guess guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try-guess next))))
    (try-guess first-guess))


;; -- 1.35 ---------------------------------------------
(define (fibb guess)
  (fixed-point (lambda(x)(+ 1 (/ 1 x))) guess))
;; -- 1.36 -----------
(define (my-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (display v1)
    (newline)
    (< (abs (- v1 v2)) 0.001))
  (define (try-guess guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try-guess next))))
    (try-guess first-guess))

(define (my-fixed-log guess)
  (my-fixed-point (lambda(x)(/ (log 1000) (log x))) guess))

;; -- 1.37 -----------
(define (cont-frac n d k)
  (define (c-f-iter i)
    (if ( < k i)
      0
      (/ (n i) (+ (d i) (c-f-iter (+ i 1))))))
  (c-f-iter 1))

(define (cont-frac-iter n d k)
  (define (c-f-iter i result)
    (if ( < k i)
      result
      (c-f-iter (+ i 1) (/ (n i) (+ (d i) result)))))
  (c-f-iter 1 0))

;; -- 1.38 -----------
(define (my-e k)
  (define (const-1 x) 1.0)
  (define (bino k)
    (cond ((< k 3) k)
          (else (if (= 0 (remainder (- k 2) 3))
                    (* 2.0 (- k 3))
                    1))))
  (+ (cont-frac const-1 bino k) 2))
  
  
  
  
