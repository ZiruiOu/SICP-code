#lang racket

(define (map func lst)
  (if (null? lst)
      '()
      (cons (func (car lst)) (map func (cdr lst)))))

(define a-lst (list 1 2 3 4))
(define (square x) (* x x))

(map square a-lst)