3

17.4

5

+

(+ 3 17.4 5)

(+ 3 (* 5 6) 8 2)


(+ 3 4 8)

(+ (* 3 (+ 7 19.5)) 4)

(+ (* 3 5)
   (* 47
      (- 20 6.8))
   12)

(newline)

(define a (* 5 5))
a
; (a)

(* a a)

(define b (+ a (* 5 a)))
b
(+ a (/ b 5))

(define (d) (* 5 5))
d
(d)

(newline)

(* 5 5)

(* 6 6)

(* 1001.7 1001.7)


(define (square x) (* x x))

(square 10)
(square 1001)
(square (+ 5 7))
(+ (square 3) (square 4))
(square (square (square 1001)))
square

(define square (lambda (x) (* x x)))

(square 10)
(square 1001)
(square (+ 5 7))
(+ (square 3) (square 4))
(square (square (square 1001)))
square

(newline)


(define (average x y)
  (/ (+ x y) 2))

(define (mean-square x y)
  (average (square x)
           (square y)))

+

(newline)

(define (abs x)
  (cond ((< x 0) (- x))
        ((= x 0) 0)
        ((> x 0) x)))

(abs -1)
(abs 0)
(abs 1)


(define (abs x)
  (if (< x 0)
    (- x)
    x))

(abs -1)
(abs 0)
(abs 1)

(newline)

(define (sqrt x) (try 1.0 x))

(define (try guess x)
  (if (good-enough? guess x)
    guess
    (try (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(sqrt 2)
(sqrt 100)

(newline)

(define (sqrt x)
  (define (improve guess x)
    (average guess (/ x guess)))

  (define (good-enough? guess x)
    (< (abs (- (square guess) x))
       0.001))

  (define (try guess x)
    (if (good-enough? guess x)
      guess
      (try (improve guess x) x)))

  (define (square x)
    (* x x))

  (define (average x y)
    (/ (+ x y) 2))

  (try 1.0 x))

(sqrt 2)
(sqrt 100)

