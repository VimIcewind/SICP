(define (sum-two-larger x y z)
  (cond ((and (<= x y) (<= x z)) (+ y z))
        ((and (<= y x) (<= y z)) (+ x z))
        ((and (<= z x) (<= z y)) (+ x y))))

;; Test
(sum-two-larger 1 1 1)
(sum-two-larger 1 2 3)
