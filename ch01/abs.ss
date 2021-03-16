(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

;; Test
(abs 10)
(abs 0)
(abs -10)
