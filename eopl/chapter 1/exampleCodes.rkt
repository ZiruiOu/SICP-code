#lang racket

;; definition s-list s-expr
;; s-list = ( . s-expr )
;; s-expr = Symbol | s-list

;; definition: Bintree ::= Int | (Symbol Bintree Bintree)

;; list-length : List -> Int
(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1
           (list-length (cdr lst))))))

;; List-of-SYmbol ::= () | (Symbol . List-of-Symbol)

;; remove-first : Symbol x Listof(Sym) -> Listof(Sym)
(define remove-first
  (lambda (s los)
    (cond ((null? los) '())
          ((eq? s (car los)) (cdr los))
          (else (cons
                 (car los)
                 (remove-first s (cdr los)))))))


;; LcExp ::= Identifier
;;       ::= (lambda (Identifier) LcExp)
;;       ::= (LcExp LcExp)

;; occurs-free? : Sym x LcExp -> Bool
(define occurs-free?
  (lambda (var expr)
    (cond ((symbol? expr) (eqv? var expr))
          ((eqv? 'lambda (car expr))
           (and (not (eqv? var (caadr expr)))
                (occurs-free? var (caddr expr))))
          (else (or (occurs-free? var (car expr))
                    (occurs-free? var (cadr expr)))))))


;; S-list ::= ()
;;        ::= (S-expr . S-list)
;; S-expr ::= Symbol | S-list

;; Symbol x Symbol x S-list -> S-list
(define subst
  (lambda (new old slist)
    (cond ((null? slist) '())
          ((symbol? slist)
           (if (eqv? old slist)
               new
               slist))
          (else (cons (subst new old (car slist))
                      (subst new old (cdr slist)))))))

;; programs given in EOPL
(define eopl-subst
  (lambda (new old slist)
    (if (null? slist)
        slist
        (cons (eopl-subst-in-sexpr new old (car slist))
              (eopl-subst new old (cdr slist))))))

(define eopl-subst-in-sexpr
  (lambda (new old sexpr)
    (if (symbol? sexpr)
        (if (eqv? old sexpr) new sexpr)
        (eopl-subst new old sexpr))))


;; IMPORTANCE OF WRITING USAGE !!!

;; in order to make process number-elements, we first make number-elements-from procedure
;; type-check : number-elements-from : Listof(SchemeVal) x Int -> Listof(List(Int, SchemeVal))
;; usage :  (number-elements-from '(v0 v1 v2 ...) n) =
;;                                                      ((n v0) (n + 1 v1) (n + 2 v2) ... )
(define number-elements-from
  (lambda (lst n)
    (if (null? lst)
        '()
        (cons (list n (car lst))
              (number-elements-from (cdr lst) (+ n 1))))))

;; number-elements : Listof(SchemeVal) -> Listof(List(Int, SchemeVal))
(define number-elements
  (lambda (lst)
    (number-elements-from lst 0)))

;; vector-sum -> partial-vector-sum

;; type-check : Vectorof(Int) x Int -> Int
;; usage : if 0 <= n < length(v) , then (partial-vector-sum v n) = v0 + v1 + ... + v_n
(define partial-vector-sum
  (lambda (v n)
    (if (zero? n)
        (vector-ref v 0)
        (+ (vector-ref v n)
           (partial-vector-sum v (- n 1))))))

;; vector-sum : Listof(Int) -> Int
;; usage : (vector-sum v) = v0 + v1 + ... v_{n-1} n = length(v)
(define vector-sum
  (lambda (v)
    (let ((n (vector-length v)))
      (if (zero? n)
          0
          (partial-vector-sum v (- n 1))))))









