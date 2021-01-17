#lang racket

;;;;1.1 newton-iterations for sqrt
(define (square x) (* x x))
(define (sqrt-iter x)
  (define eps 0.000001)
  (define (good-enough? guess x) (< (abs (- (square guess) x)) eps))
  (define (newton-sqrt guess x)
    (if (good-enough? guess x)
        guess
        (newton-sqrt (/ (+ guess (/ x guess)) 2) x)))
  (newton-sqrt 1.0 x))

;;to compare the result of newton-iteration and built-in sqrt function
(define (compare-sqrt items)
  (cond ((null? items) (void))
        (else (let ((head (car items)))
                (displayln (sqrt head))
                (displayln (sqrt-iter head))
                (newline)
                (compare-sqrt (cdr items))))))

(compare-sqrt '(2 3 13 23 75))

;;;;1.2

;; exponential-fib
(define (expo-fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (expo-fib (- n 2))
                 (expo-fib (- n 1))))))

;; linear-fib
(define (line-fib n)
  (define (inner-loop x a b)
    (if (= x n)
        a
        (inner-loop (+ x 1) b (+ a b))))
  (inner-loop 0 1 1))

