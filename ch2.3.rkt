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


