(define (third x y)
  (/ (+ (/ x (square y))
        (* 2 y))
     3))

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (cuberoot x)
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.000000000000001))
  (define (improve guess)
    (third x guess))
  (define (cuberoot-iter guess)
    (if (good-enough? guess)
      guess
      (cuberoot-iter (improve guess))))
  (cuberoot-iter 1.0))

(cuberoot 27)

