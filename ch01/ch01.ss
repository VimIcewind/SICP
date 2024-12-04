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

; (define (+ x y)
;  (if (= x 0)
;    y
;    (+ (-1+ x) (1+ y))))

;(+ 3 4)

; (define (+ x y)
;   (if (= x 0)
;     y
;     (1+ (+ (-1+ x) y))))

; (+ 3 4)

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

(newline)

(define (sum-int a b)
  (if (> a b)
      0
      (+ a
         (sum-int (1+ a) b))))

(sum-int 3 4)


(define (sum-sq a b)
  (if (> a b)
      0
      (+ (square a)
         (sum-sq (1+ a) b))))

(define (square x)
  (* x x))

(sum-sq 3 4)


(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

(* 8 (pi-sum 1 1000))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term
              (next a)
              next
              b))))

(define (sum-int a b)
  (define (identity x) x)
  (sum identity a 1+ b))

(define (sum-int a b)
  (sum (lambda (x) x) a 1+ b))

(define (sum-sq a b)
  (sum square a 1+ b))

(define (pi-sum a b)
  (sum (lambda (i) (/ 1.0 (* i (+ i 2))))
       a
       (lambda (i) (+ i 4))
       b))

(sum-int 3 4)
(sum-sq 3 4)
(* 8 (pi-sum 1 1000))

(newline)

(define (sum term a next b)
  (define (iter j ans)
    (if (> j b)
        ans
        (iter (next j)
              (+ (term j) ans))))
  (iter a 0))

(define (sum-int a b)
  (define (identity x) x)
  (sum identity a 1+ b))

(define (sum-sq a b)
  (sum square a 1+ b))

(define (pi-sum a b)
  (sum (lambda (i) (/ 1.0 (* i (+ i 2))))
       a
       (lambda (i) (+ i 4))
       b))

(sum-int 3 4)
(sum-sq 3 4)
(* 8 (pi-sum 1 1000))

(newline)

(define (sqrt x)
  (fixed-point
    (lambda (y) (average (/ x y) y))
    1))

(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enuf? u v)
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close-enuf? old new)
      new
      (iter new (f new))))
  (iter start (f start)))

(sqrt 9.0)

(newline)

(define (sqrt x)
  (fixed-point
    (average-damp (lambda (y) (/ x y)))
    1))

(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enuf? u v)
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close-enuf? old new)
      new
      (iter new (f new))))
  (iter start (f start)))

(define (average-damp f)
    (define (foo x)
      (average (f x) x))
    foo)

(define (average-damp f)
    (lambda (x) (average (f x) x))))

(define average-damp
  (lambda (f)
    (lambda (x) (average (f x) x))))

(sqrt 9.0)

(newline)

(define (sqrt x)
  (newton (lambda (y) (- x (square y)))
          1))

(define (square x)
  (* x x))

(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enuf? u v)
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close-enuf? old new)
      new
      (iter new (f new))))
  (iter start (f start)))

(define (newton f guess)
  (define df (deriv f))
  (fixed-point
    (lambda (x) (- x (/ (f x) (df x))))
    guess))

(define (newton f guess)
  (fixed-point
    (lambda (x) (- x (/ (f x) ((deriv f) x))))
    guess))

(define deriv
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx))
            (f x))
         dx))))

(define dx  .000001)

(sqrt 9.0)
