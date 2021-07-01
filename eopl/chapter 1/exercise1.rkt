#lang racket

;; Exercise 1.12
;;inlining technique

;; inlining-subst : Symbol x Symbol x Slist -> Slist

(define inlining-subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (let ((sexpr (car slist)))
          (cons (if (symbol? sexpr)
                    (if (eqv? old sexpr) new sexpr)
                    (inlining-subst new old sexpr))
                (inlining-subst new old (cdr slist)))))))


;; (inlining-subst 'a 'b '((b c) (b () d)))

;; Exercise 1.13
;; map for subst
(define (map-subst new old slist)
  (if (or (symbol? slist) (null? slist))
      (if (eqv? old slist) new slist)
      (map (lambda (sexpr) (map-subst new old sexpr)) slist)))

;;(map-subst 'a 'b '((b c) (b () d)))
;;(map-subst 'z 'd '((d (a (b (c d) d)) (b (d () (d ())))) (b (c (d (e f) (g d h))))))


;;Ex 1.15

;; duple : Int x SchemeVal -> Listof(SchemeVal)
;; usage : (duple n x) = (x x x ... x), x appears n times
(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons x
              (duple (- n 1) x)))))

;;(duple 4 '(ha ha))

;;Ex 1.16
;; invert : Listof(List(SchemeVal, SchemeVal)) -> Listof(List(SchemeVal, SchemeVal))
;; usage : (invert ((a1 b1) (a2 b2) ...)) = ((b1 a1) (b2 a2) ... (bn an)) i.e. reverse each 2-list in list
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (let ((slist (car lst)))
          (cons (list (cadr slist) (car slist))
                (invert (cdr lst)))))))

;; Ex 1.28
;; merge : Listof(Int) x Listof(Int) -> Listof(Int)
;; usage : (merge loi1 loi2), return a list of elements in loi1 and loi2 in ascending order
(define merge
  (lambda (loi1 loi2)
    (if (null? loi1)
        loi2
        (if (null? loi2)
            loi1
            (if (< (car loi1) (car loi2))
                (cons (car loi1)
                      (merge (cdr loi1) loi2))
                (cons (car loi2)
                      (merge loi1 (cdr loi2))))))))

;;(merge '(1 4) '(1 2 8))
;;(merge '(35 62 81 90 91) '(3 83 85 90))

;; Ex 1.29

;; slice : Listof(Int) x Int x Int x Int -> Listof(Int)


;; sort-by-split : Listof(Int) x Int -> Listof(Int)
;; usage : split the list with length and do merge sort




