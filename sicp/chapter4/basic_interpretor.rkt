#lang racket

(require r5rs)
;;; helper functions
(define (tag-list? tag exp)
  (if (not (pair? exp))
      false
      (eq? tag (car exp))))

;;; control flow
;;;;;; begin
(define (begin? expr) (tag-list? 'begin expr))
(define (begin-sequence expr) (cdr expr))
(define (make-begin actions)
  (cons 'begin actions))

;;;;;; sequence
(define (first-item sequence) (car sequence))
(define (rest-items sequence) (cdr sequence))
(define (last-item? sequence) (and (not (null? sequence)) (null? (cdr sequence))))

;;;;;; eval
(define (eval-sequence sequence env)
  (define (inner-loop actions)
    (cond ((null? actions) (void))
          ((last-item? actions) (my-eval (first-item actions) env))
          (else
           (my-eval (first-item actions)
                 env)
           (eval-sequence (rest-items actions)
                          env))))
  (inner-loop sequence))


;;;;;; if : (if (cond) then else)
(define (if? expr) (tag-list? 'if expr))
(define (if-cond expr) (cadr expr))
(define (if-then expr) (caddr expr))
(define (if-else expr)
  (if (not (null? (cdddr expr)))
      (cadddr expr)
      'false))

;;;;;; make if
(define (make-if cond then else)
  (cons 'if (cons cond (list then else))))


;;;;;; eval-if
(define (eval-if expr env)
  (if (my-eval (if-cond expr) env)
      (my-eval (if-then expr) env)
      (my-eval (if-else expr) env)))

;;;;;; unit test
(define (test-if-api)
  (define test-if '(if (> (+ x 3) (- y z)) (+ (x + 4) (- z 1)) (* (* x x) (fact y))))
  (if? test-if)
  (if-cond test-if)
  (if-then test-if)
  (if-else test-if))

;;;;;; (cond clauses)
;;;;;; clauses := clause clauses | end-clause
;;;;;; clause  := (clause-cond clause-actions)
;;;;;; end-clause := (else clause-action)
(define (cond? expr) (tag-list? 'cond expr))

(define (clause-cond clause) (car clause))
(define (clause-actions clause) (cdr clause))

(define (cond-clauses expr) (cdr expr))
(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))
(define (last-clause? clause) (eq? 'else (clause-cond clause)))

;;;;;; unit test
(define (test-cond)
  (define my-cond '(cond ((< x -2) (set! x (x + 100)) (* x 4))
                         ((> x 4)  (define y (x - 10)) (* y 20))
                         (else (+ x 100) (set! x (+ x 20)) x)))
  (define test-clauses (cond-clauses my-cond))
  (define test-clause  (car test-clauses))
  (clause-cond test-clause)
  (clause-actions test-clause))

;;;;;; eval cond
(define (cond->if expr)
  (expand-clauses (cond-clauses expr)))

;;;;;; convert cond to if
(define (expand-clauses clauses)
  (if (null? clauses)
      '(void)
      (let ((head (first-clause clauses))
            (tail (rest-clauses clauses)))
        (if (last-clause? head)
            (make-begin (clause-actions head))
            (make-if (clause-cond head)
                     (make-begin (clause-actions head))
                     (expand-clauses tail))))))

;;;;;; unit-test
(define (cond->if-test)
  (define convert-test1
    '(cond ((< x -100) (+ x 4) (set! x (* x (- x 100))) (* x (* x x x)))
           ((< x -10) (+ (* x 4) (- 3 7)))
           ((= x 0) 100000)
           (else (* x x))))
  (println (cond->if convert-test1)))

;;;;;; and
(define (and? expr) (tag-list? 'and expr))
(define (and-booleans expr) (cdr expr))

(define (first-boolean items) (car items))
(define (rest-booleans items) (cdr items))

;;;;;; eval-and
(define (eval-and expr env)
  (define (inner-loop clauses)
    (cond ((null? clauses) #t)
          ((not (my-eval (first-boolean clauses) env)) #f)
          (else (inner-loop (rest-booleans clauses)))))
  (inner-loop (and-booleans expr)))

;;;;;; or
(define (or? expr) (tag-list? 'or expr))
(define (or-booleans expr) (cdr expr))

;;;;;; eval-or
(define (eval-or expr env)
  (define (inner-loop clauses)
    (cond ((null? clauses) false)
          ((my-eval (first-boolean clauses) env) true)
          (else (inner-loop (rest-booleans clauses)))))
  (inner-loop (or-booleans expr)))

;;;dataflow
;;;;;; number, string
(define (self-evaluating? expr)
  (cond ((number? expr) true)
        ((string? expr) true)
        (else false)))

;;;;;; variable
(define (variable? expr) (symbol? expr))

;;;;;; (set! var val)
;;;;;; (set! var function)
(define (assignment? expr) (tag-list? 'set! expr))
(define (set-variable expr) (cadr expr))
(define (set-value expr) (caddr expr))

(define (eval-assignment expr env)
  (set-var-val! (set-variable expr)
                (my-eval (set-value expr)
                      env)
                env))

;;;;;; lambda
;;;;;; (lambda (x y z) lambda-body)
(define (lambda? expr) (tag-list? 'lambda expr))
(define (lambda-variables expr) (cadr expr))
(define (lambda-body expr) (cddr expr))

;;;;;; unit test
(define (test-lambda)
  (define my-lambda '(lambda (x y z) (set! x (+ x 1)) (set! y (* y 2)) (set! z (/ z 3)) (+ x y z)))
  (lambda? my-lambda)
  (lambda-variables my-lambda)
  (lambda-body my-lambda))

;;;;;; make lambda
(define (make-lambda vars body)
  (cons 'lambda (cons vars body)))

;;;;;; unit test
(define (make-lambda-test)
  (make-lambda '(x y z) '((set! x (+ x 1)) (set! y (* y 2)) (set! z (/ z 3)) (+ x y z))))

;;;;;; procedure
(define (compound-procedure? expr) (tag-list? 'procedure expr))
(define (procedure-arguments expr) (cadr expr))
(define (procedure-body expr) (cddr expr))

(define (make-procedure vars body)
  (cons 'procedure (cons vars body)))

;;;;;; define
;;;;;; (define var number-value)
;;;;;; (define (function x y z) function-body)
(define (define? expr) (tag-list? 'define expr))
(define (define-variable expr)
  (let ((head (cadr expr)))
    (if (list? head)
        (car head)
        head)))

(define (define-value expr)
  (let ((head (cadr expr)))
    (if (list? head)
        (make-lambda (cdr head) (cddr expr))
        (caddr expr))))

(define (eval-definition expr env)
  (define-var-val! (define-variable expr)
                   (my-eval (define-value expr)
                         env)
                   env))
    

;;;;;; unit-test
(define (test-define)
  (define test-define-var '(define x (+ (* 2 3)
                                     (- (* 4 5)
                                        (/ 6 7)))))
  (define? test-define-var)
  (define-variable test-define-var)
  (define-value test-define-var)

  (define test-define-func '(define (fact x)
                              (cond ((= x 0) 1)
                                    ((= x 1) 1)
                                    (else (* x (fact (- x 1)))))))
  (define? test-define-func)
  (define-variable test-define-func)
  (define-value test-define-func)
)

;;;;;; quote
;;;;;; (quote text)
(define (quoted? expr) (tag-list? 'quote expr))
(define (quoted-text expr) (cadr expr))

;;; symbol table
(define (symtab-variables symtab) (car symtab))
(define (symtab-values symtab) (cdr symtab))
(define (symtab-insert! var val symtab)
  (set-car! symtab (cons var (symtab-variables symtab)))
  (set-cdr! symtab (cons val (symtab-values symtab))))

;;; environment
(define (current-symtab env) (car env))
(define (enclosing-env env) (cdr env))
(define (extend-environment vars vals base-environment)
  (if (= (length vars) (length vals))
      (cons (cons vars vals) base-environment)
      (error "extend-environment : variables and values not match")))

(define (find-var-val var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-env env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (null? env)
        (error "find-var-val : undefined variable" var)
        (scan (symtab-variables (current-symtab env))
              (symtab-values (current-symtab env)))))
  (env-loop env))

(define (set-var-val! var val env)
  (define (symtab-loop vars vals)
    (cond ((null? vars) #f)
          ((eq? var (car vars)) (set-car! vals val) #t)
          (else (symtab-loop (cdr vars) (cdr vals)))))
   (let ((symtab (car env)))
    (let ((vars (car symtab))
          (vals (cdr symtab)))
      (let ((result (symtab-loop vars vals)))
    (cond ((not (false? result)) (void))
          ((null? (enclosing-env env)) (error "find-var-val : variable " var " is not defined"))
          (else (set-var-val! var val (enclosing-env env))))))))

(define (define-var-val! var val env)
  (let ((symtab (car env)))
    (define (symtab-loop vars vals)
      (cond ((null? vars)
             (symtab-insert! var val symtab))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (symtab-loop (cdr vars) (cdr vals)))))
    (symtab-loop (symtab-variables symtab)
                 (symtab-values symtab))))
  

;;;;;; unit test
(define (test-env)
  (define env1 (cons (cons '(+ - * / x y z w aha square)
                           (list + - * / 10 20 30 40 -100 '(lambda (n) (* n n))))
                     '()))
  (define env2 (extend-environment '(a b c)
                                   '(1 2 3)
                                   env1))
  (define env3 (extend-environment '(p q r s x y z)
                                   '(-100 -200 -300 -400 100 200 300)
                                   env2))
  (find-var-val 'x env3)
  (find-var-val 'a env3)
  (set-var-val! 'p 20 env3)
  (set-var-val! 'x 10000000 env3)
  (find-var-val 'w env3)
  (set-var-val! 'w -114514 env3)
  (find-var-val 'w env3)
  (define-var-val! 'wow 14 env3)
  (define-var-val! 'mili 114514 env3)
  (define-var-val! 'milet -114514 env3)
 )

;;;;;; function call
(define (application? expr) (list? expr))
(define (operator expr) (car expr))
(define (operands expr) (cdr expr))
(define (calculate-operands arguments env)
  (map (lambda (arg) (my-eval arg env)) arguments))

;;;;;; lisp-primitive
(define primitive-operation
  (list (cons '+ +)
        (cons '- -)
        (cons '* *)
        (cons '/ /)
        (cons 'remainder remainder)
        (cons '= =)
        (cons '> >)
        (cons '< <)
        (cons '>= >=)
        (cons '<= <=)
        (cons 'eq? eq?)
        (cons 'not not)
        (cons 'list? list?)
        (cons 'pair? pair?)
        (cons 'length length)
        (cons 'cons cons)
        (cons 'car car)
        (cons 'cdr cdr)
        (cons 'cadr cadr)
        (cons 'cddr cddr)
        (cons 'list list)
        (cons 'append append)
        (cons 'null? null?)
        (cons 'number? number?)
        (cons 'symbol? symbol?)
        (cons 'false? false?)
        (cons 'read  read)
        (cons 'display display)
        (cons 'displayln displayln)
        (cons 'sqrt    sqrt)))

(define (primitive? op) (tag-list? 'primitive op))
(define lisp-primitive
  (map (lambda (pair) (cons (car pair)
                            (cons 'primitive
                                  (cdr pair))))
       primitive-operation))

;;;;;; init-env
(define (setup-init-environment)
  (let  ((primitive-vars (map car lisp-primitive))
         (primitive-vals (map cdr lisp-primitive)))
    (let ((init-environment (extend-environment primitive-vars
                                                primitive-vals
                                                '())))
      (define-var-val! 'true true init-environment)
      (define-var-val! 'false false init-environment)
      init-environment)))

(define init-env (setup-init-environment))

;;;;;; my-eval function
(define (my-eval expr env)
  (cond ((self-evaluating? expr) expr)
        ((quoted? expr) (quoted-text expr))
        ((variable? expr) (find-var-val expr env))
        ((define? expr) (eval-definition expr env))
        ((assignment? expr) (eval-assignment expr env))
        ((lambda? expr)
         (make-procedure
          (lambda-variables expr)
          (lambda-body expr)))
        ((begin? expr)
         (eval-sequence (begin-sequence expr)
                        env))
        ((and? expr) (eval-and expr env))
        ((or? expr) (eval-or expr env))
        ((if? expr) (eval-if expr env))
        ((cond? expr)
         (eval-if (cond->if expr) env))
        ((application? expr)
         (my-apply (my-eval (operator expr) env)
                   (calculate-operands (operands expr) env)
                   env))
        (else
         (error "my-eval : unknown expression type " expr))))

;;;;;; my-apply function
;;;;;; two types : (1) primitive : built-in procedure if lisp, i.e. +, -, *, /
;;;;;;             (2) compound-procedure : function definitions
(define (my-apply operation operands env)
  (cond ((primitive? operation)
         (apply (cdr operation) operands))
        ((compound-procedure? operation)
         (eval-sequence
          (procedure-body operation)
          (extend-environment
           (procedure-arguments operation)
           operands
           env)))
        (else
         (error "my-apply : unknown type procedure " operation))))

;;; dirver
(define (user-print object)
  (cond ((compound-procedure? object)
         (displayln (list 'compound-procedure
                        (procedure-arguments object)
                        (procedure-body object))))
        ((eq? object (void)) (void))
        (else (displayln object))))

(define (driver-loop)
  (let ((input (read)))
    (if (eq? input eof)
        (void)
        (let ((output (my-eval input init-env)))
          (user-print output)
          (driver-loop)))))

;;; start
(driver-loop)

