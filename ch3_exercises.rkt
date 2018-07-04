#lang plai-typed

(require (typed-in racket [ormap : (('T -> boolean) (listof 'T) -> boolean)]))
;(require (only-in lang/htdp-beginner ...))
(require plai-typed/s-exp-match)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datatype definitions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)
          

; An Atom is one of: 
; – Number
; – String
; – Symbol

(define true-value?
  (lambda (x)
    (not (zero? x))))

#;(define closure
  (lambda (ids body env)
    (lambda (args)
      (eval-expression body (extend-env ids args env)))))

(define-type program
  (a-program (exp : expression)))

(define-type expression
  (lit-exp
   (datum : number))
  (var-exp
   (id : symbol))
  (primapp-exp
   (prim : primitive)
   (rands : (listof expression)))
  (if-exp
   (test-exp : expression)
   (true-exp : expression)
   (false-exp : expression))
  (let-exp
   (ids : (listof symbol))
   (rands : (listof expression))
   (body : expression))
  (proc-exp
   (ids : (listof symbol))
   (body : expression))
  (app-exp
   (rator : expression)
   (rands : (listof expression))))

(define-type primitive
  (add-prim) ; add-prim is a function if you do (add-prim) you are calling it
  (sub-prim)
  (mul-prim)
  (add1-prim)
  (sub1-prim))

; For environment passing interpreter
#;(define-type procval procval?
   (closure
    (ids (list-of symbol?))
    (body expression?)
    (env environment?)))

; For substitution based interpreter
#;(define-type proc proc?
  (lam (ids (list-of symbol?))
       (body expression?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Front End (Sexpr -> AST)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sexp -> Program
(define sexp-to-program
  (lambda (x)
    (a-program (sexp-to-exp x))))

; Sexp -> Expression (or error)
(define sexp-to-exp
  (lambda (x)
   (cond
      [(s-exp-number? x) (lit-exp (s-exp->number x))]
      [(s-exp-symbol? x) (var-exp (s-exp->symbol x))]
      [(s-exp-list? x) (primapp-exp (sexp-to-prim (first (s-exp->list x))) (sexp-to-list (rest (s-exp->list x))))]
      [else (error 'sexp-to-exp "not a valid sexpression")])))
        
        
(define sexp-to-prim
  (lambda ([x : s-expression])
    (let ([x (s-exp->symbol x)])
      (cond
        [(symbol=? '+ x) (add-prim)]
        [(symbol=? '- x) (sub-prim)]
        [(symbol=? '* x) (mul-prim)]
        [(symbol=? 'add1 x) (add1-prim)]
        [(symbol=? 'sub1 x) (sub1-prim)]
        [else (error 'sexp-to-prim (string-append (to-string x) " not a primitive operation"))]))))


; List-of Sexpr -> List-of Expression
(define sexp-to-list
  (lambda ([a : (listof s-expression)])
    (cond
      [(empty? a) empty]
      [else (cons (sexp-to-exp (first a)) (sexp-to-list (rest a)))])))

(test (sexp-to-program '(add1 (+ 3 x)))
                               (a-program
                                (primapp-exp
                                 (add1-prim)
                                 (list (primapp-exp
                                        (add-prim)
                                        (list (lit-exp 3)
                                              (var-exp 'x)))))))
    
(define apply-primitive
  (lambda (prim args)
    (type-case primitive prim
      (add-prim () (+ (first args) (second args)))
      (sub-prim () (- (first args) (second args)))
      (mul-prim () (* (first args) (second args)))
      (add1-prim () (+ (first args) 1))
      (sub1-prim () (- (first args) 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Examples:
;
;'(add1 (+ 3 x)) =>
; (a-program
;   (prim-app
;     (add1-prim)
;     (list (prim-app
;             (add-prim)
;             (list (lit-exp 3)
;                   (var-exp x)))
;
;'(add1 (add1 4))
;'(* (add1 4) (add1 5))
; 2
; 'x
; '(+ (- 3 x) (* (+ 2 x) (add1 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Substitution based interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define subst
  (lambda (exp sub-id val)
    (let ((subst-rands (lambda (rands) (map (lambda (rand) (subst rand sub-id val)) rands)))
          (redundant? (lambda (ids) (ormap (lambda (id) (symbol=? sub-id id)) ids))))
      (type-case expression exp
        (lit-exp (datum) exp)
        (var-exp (id) (if (equal? id sub-id)
                          val
                          exp))
        (primapp-exp (prim rands)
                     (primapp-exp prim (subst-rands rands)))
        (if-exp (test-exp true-exp false-exp)
                (if-exp (subst test-exp sub-id val)
                        (subst true-exp sub-id val)
                        (subst false-exp sub-id val)))
        (let-exp (ids rands body)
                 (if (redundant? ids)
                     exp
                     (let-exp ids (subst-rands rands) (subst body sub-id val))))
        (proc-exp (ids body)
                  (if (redundant? ids)
                      exp
                      (proc-exp ids (subst body sub-id val))))
        (app-exp (rator rands)
                 (app-exp (subst rator sub-id val) (subst-rands rands)))))))

(test (subst (lit-exp 5) 'x (lit-exp 3)) (lit-exp 5))
(test (subst (var-exp 'x) 'x (lit-exp 3)) (lit-exp 3))
(test (subst (var-exp 'x) 'y (lit-exp 3)) (var-exp 'x))
(test (subst (proc-exp (list 'x) (primapp-exp (add-prim) (list (var-exp 'x) (var-exp 'y)))) 'y (lit-exp 3))
              (proc-exp (list 'x) (primapp-exp (add-prim) (list (var-exp 'x) (lit-exp 3)))))
(test (subst (proc-exp (list 'x) (primapp-exp (add-prim) (list (var-exp 'x) (lit-exp 3)))) 'x (lit-exp 5))
              (proc-exp (list 'x) (primapp-exp (add-prim) (list (var-exp 'x) (lit-exp 3)))))
(test (subst (proc-exp (list 'f 'x) (app-exp (var-exp 'f) (list (var-exp 'x)))) 'f (proc-exp (list 'x) (var-exp 'x)))
      (proc-exp (list 'f 'x) (app-exp (var-exp 'f) (list (var-exp 'x)))))
(test (subst (proc-exp (list 'x) (var-exp 'y)) 'y (var-exp 'x))
      (proc-exp (list 'z) (var-exp 'x))) ;<-------------------------------------------------------------------------------------------------------------failing
(test (subst (proc-exp (list 'f 'x) (app-exp (var-exp 'g) (list (var-exp 'x)))) 'g (proc-exp (list 'x) (var-exp 'x)))
      (proc-exp (list 'f 'x) (app-exp (proc-exp (list 'x) (var-exp 'x)) (list (var-exp 'x)))))

    

#;(define eval-rands-subst
  ...)
                   

#;(define eval-program-subst
  (lambda (p)
    (type-case program p
      (a-program (body)
                 (eval-expression body)))))

#;(define eval-expression-subst
    (lambda (exp)
      (type-case expression exp
        (lit-exp (datum) datum)
        (var-exp (id) (eopl:error "free identifier ~s" id))
        (primapp-exp (prim rands)
                     (let ((args (eval-rands rands env)))
                       (apply-primitive prim args)))
        (if-exp (test-exp true-exp false-exp)
                (if (true-value? (eval-expression test-exp))
                    (eval-expression true-exp)
                    (eval-expression false-exp)))
        (let-exp (ids rands body)
                 (let ((args (eval-rands rands env)))
                   (eval-expression body (extend-env ids args env))))
        (proc-exp (ids body) (closure ids body env))
        (app-exp (rator rands)
                 (let ((proc (eval-expression rator env))
                       (args (eval-rands rands env)))
                   (if (procval? proc)
                       (apply-procval proc args)
                       (eopl:error "Attempt to apply non-procedure ~s" proc)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment passing interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(define eval-program
    (lambda (p)
      (type-case program p
        (a-program (body)
                   (eval-expression body (init-env))))))

#;(define eval-expression
    (lambda (exp env)
      (type-case expression exp
        (lit-exp (datum) datum)
        (var-exp (id) (apply-env env id))
        (primapp-exp (prim rands)
                     (let ((args (eval-rands rands env)))
                       (apply-primitive prim args)))
        (if-exp (test-exp true-exp false-exp)
                (if (true-value? (eval-expression test-exp))
                    (eval-expression true-exp)
                    (eval-expression false-exp)))
        (let-exp (ids rands body)
                 (let ((args (eval-rands rands env)))
                   (eval-expression body (extend-env ids args env))))
        (proc-exp (ids body) (closure ids body env))
        (app-exp (rator rands)
                 (let ((proc (eval-expression rator env))
                       (args (eval-rands rands env)))
                   (if (procval? proc)
                       (apply-procval proc args)
                       (eopl:error "Attempt to apply non-procedure ~s" proc)))))))

#;(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

#;(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))



#;(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     '(1 5 10)
     (empty-env))))

#;(define apply-env
  (lambda (env id)
    (cond
      [(null? env) (eopl:error "empty environment")]
      [else (if (equal? (first (first env)) id)
                (first (rest (first env)))
                (eopl:error "unbound variable"))])))

#;(define empty-env '())

#;(define extend-env
  (lambda (ids vals cur-env)
    (append (zip ids vals) cur-env)))

#;(define zip
  (lambda (ids vals)
    (cond
      [(and (null? ids) (null? vals)) empty-env]
      [else (if (equal? (length ids) (length vals))
                (cons '((first ids) (first vals)) (zip (rest ids) (rest vals)))
                (eopl:error "ids and vals don't match up"))])))
      


    

        
      