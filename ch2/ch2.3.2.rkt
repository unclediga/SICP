;;====( CH 2.3.2 )=======================

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "неизвестный тип выражения -- DERIV" exp))))  

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;; -- 2.56 ----------------

(define (deriv2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv2 (addend exp) var)
                   (deriv2 (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv2 (multiplicand exp) var))
          (make-product (deriv2 (multiplier exp) var)
                        (multiplicand exp))))
        ;; new 
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponent (base exp) (- (exponent exp) 1))))
         (else
         (error "неизв. тип выражения DERIV2 [" exp "]"))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '^)))

(define (make-exponent base exponent)
  (cond
   ((= exponent 0) 1)
   ((= exponent 1) base)
   ((number? base)
    (cond
     ((= base 1) 1)
     ((> base 0) (* base (make-exponent base (- exponent 1))))
     (#t (list '^ base exponent))))
   (#t (list '^ base exponent))))

(define (exponent exp)
  (caddr exp))

(define (base exp)
  (cadr exp))

;; -- 2.57 ----------------

(define (augend s)
  (let ((s1 (cddr s)))
    (if (> (length s1) 1)
	(cons '+ s1)
	(car s1))))
	

(define (multiplicand p)
  (let ((p1 (cddr p)))
    (if (> (length p1) 1)
	(cons '* p1)
	(car p1))))