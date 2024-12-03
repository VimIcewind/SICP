(define (cube x) (* x x x))

(cube 2)


(define (sum-integers a b)
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))

(sum-integers 1 10)


(define (sum-cubes a b)
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (+ a 1) b))))

(sum-cubes 1 10)


(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(* 8 (pi-sum 1 1000))


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (inc n) (+ n 1))

(define (sum-cube a b)
  (sum cube a inc b))

(sum-cubes 1 10)


(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)



(lambda (x) (+ x 4))

(lambda (x) (/ 1.0 (* x (+ x 2))))


(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(* 8 (pi-sum 1 1000))


(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)


(define (plus4 x) (+ x 4))

(define plus4 (lambda (x) (+ x 4)))


(define square (lambda (x) (* x x)))

((lambda (x y z) (+ x y (square z))) 1 2 3)


(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-heler (+ 1 (* x y))
           (- 1 y)))


(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))


(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))


(define x 5)

(+ (let ((x 3))
     (+ x (* x 10)))
   x)


(define x 2)
(let ((x 3)
      (y (+ x 2)))
  (* x y))


(define (f x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))
