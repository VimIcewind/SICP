(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

;; Test
(factorial 6)
