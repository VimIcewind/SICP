(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (+ a b)
  (if  (= a 0)
    b
    (inc (+ (dec a) b))))

;; Test
(+ 1 2)
;; (+ 1 2)
;; (inc (+ (dec 1) 2))
;; (inc (+ 0 2))
;; (inc 2)
;; 3

(define (+ a b)
  (if (= a 0)
    b
    (+ (dec a) (inc b))))

;; Test
(+ 1 2)
;; (+ 1 2)
;; (+ (dec 1) (inc 2))
;; (+ 0 3)
;; 3
