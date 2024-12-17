(define x (cons 1 2))

(car x)
(cdr x)

(define x (cons 1 2))

(define y (cons 3 4))

(define z (cons x y))

(car (car z))

(car (cdr z))


(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))

(define (cdr z) (z 1))

(define x (cons 1 2))

(car x)
(cdr x)

(define x (cons 1 2))

(define y (cons 3 4))

(define z (cons x y))

(car (car z))

(car (cdr z))
