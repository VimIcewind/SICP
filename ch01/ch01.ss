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

(newline)

(define (sos x y)
  (+ (sq x) (sq y)))
(define (sq x)
  (* x x))

(sos 3 4)

(newline)

(define (+ x y)
  (if (= x 0)
    y
    (+ (-1+ x) (1+ y))))

(+ 3 4)

(define (+ x y)
  (if (= x 0)
    y
    (1+ (+ (-1+ x) y))))

(+ 3 4)

(newline)


(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2))))))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
(fib 10)

(newline)

(define (towers-of-hanoi n source dest temp)
  (cond [(= n 1)
         (printf "Move the disk from ~a to ~a~n" source dest)]
        [else
          (towers-of-hanoi (1- n) source temp dest)
          (printf "Move the disk from ~a to ~a~n" source dest)
          (towers-of-hanoi (1- n) temp dest source)]))

(towers-of-hanoi 1 "source" "dest" "temp")
(newline)
(towers-of-hanoi 2 "source" "dest" "temp")
(newline)
(towers-of-hanoi 3 "source" "dest" "temp")
(newline)
(towers-of-hanoi 4 "source" "dest" "temp")
(newline)

(newline)

(define (move n from to spare)
  (cond ((= n 0) "Done")
        (else
          (move (1- n) from spare to)
          (printf "move ~a ~a ~a ~a~n" n from to spare)
          (move (1- n) spare to from))))

(move 1 1 2 3)
(newline)
(move 2 1 2 3)
(newline)
(move 3 1 2 3)
(newline)
(move 4 1 2 3)
