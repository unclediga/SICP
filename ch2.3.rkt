;; -- 2.53 ------------------------
;(list 'a 'b 'c)
;(list (list 'george))
;(cdr '((x1 x2) (y1 y2)))
;(cadr '((x1 x2) (y1 y2)))
;(pair? (car '(a short list)))
;(memq 'red '((red shoes) (blue socks)))
;(memq 'red '(red shoes blue socks))

;; -- 2.54 -----------------------
(define (equal2? a b)
  (cond ((and (not (or (pair? a)(pair? b))) (eq? a b)) #t)
        ((and (pair? a)(pair? b)(eq? (car a) (car b))) (equal2? (cdr a)(cdr b)))
        (else #f)))

;; -- 2.55 -----------------------
;; (car ''abracadabra)  => (car (quote (quote abracadabra)))  => quote


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
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ;; new 
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponent (deriv2 (base exp) var) 
                                      (- (exponent exp) 1))))
         (else
         (error "неизвестный тип выражения -- DERIV" exp))))  

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '^)))

(define (make-exponent base exponent)
  (list '^ base exponent))

(define (exponent exp)
  (caddr exp))

(define (base exp)
  (cadr exp))
