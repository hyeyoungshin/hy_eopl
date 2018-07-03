#lang eopl

(require rackunit)

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

(define-datatype program program?
  (a-program (exp expression?)))

(define-datatype expression expression?
  (lit-exp
   (datum number?))
  (var-exp
   (id symbol?))
  (primapp-exp
   (prim primitive?)
   (rands (list-of expression?))))

(define-datatype primitive primitive?
  (add-prim)
  (sub-prim)
  (mul-prim)
  (add1-prim)
  (sub1-prim))


; Sexp -> Program
(define sexp-to-program
  (lambda (x)
    (a-program (sexp-to-exp x))))

; Sexp -> Expression (or error)
(define sexp-to-exp
  (lambda (x)
    (cond
      [(number? x) (lit-exp x)]
      [(symbol? x) (var-exp x)]
      [(list? x) (primapp-exp (sexp-to-prim (car x)) (sexp-to-list (cdr x)))]
      [else eopl:error])))
        
        
(define sexp-to-prim
  (lambda (x)
    (cond
      [(eqv? x '+) (add-prim)]
      [(eqv? x '-) (sub-prim)]
      [(eqv? x '*) (mul-prim)]
      [(eqv? x 'add1) (add1-prim)]
      [(eqv? x 'sub1) (sub1-prim)]
      [else (eopl:error 'sexp-to-prim"~s not a primitive operation" x)])))


; List-of Sexpr -> List-of Expression
(define sexp-to-list
  (lambda (a)
    (cond
      [(null? a) '()]
      [else (cons (sexp-to-exp (car a)) (sexp-to-list (cdr a)))])))

(check-equal? (sexp-to-program '(add1 (+ 3 x)))
                               (a-program
                                (primapp-exp
                                 (add1-prim)
                                 (list (primapp-exp
                                        (add-prim)
                                        (list (lit-exp 3)
                                              (var-exp 'x)))))))
    
;'(add1 (+ 3 x)) =>
; (a-program
;   (prim-app
;     (add1-prim)
;     (list (prim-app
;             (add-prim)
;             (list (lit-exp 3)
;                   (var-exp x)))

;'(add1 (add1 4))
;'(* (add1 4) (add1 5))
; 2
; 'x
; '(+ (- 3 x) (* (+ 2 x) (add1 5))


    
(define eval-program
  (lambda (p)
    (cases program p
      (a-program (body)
                 (eval-expression body (init-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args))))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (sub-prim () (- (car args) (cadr args)))
      (mul-prim () (* (car args) (cadr args)))
      (add1-prim () (+ (car args) 1))
      (sub1-prim () (- (car args) 1)))))

(define init-env
  (lambda ()
    (extend-env
     '(x 5)
     '())))

(define apply-env
  (lambda (env id)
    (cond
      [(null? env) (eopl:error "empty environment")]
      [else (if (eqv? (car (car env)) id)
                (car (cdr (car env)))
                (eopl:error "unbound variable"))])))

(define extend-env
  (lambda (binding cur-env)
    (cons binding cur-env)))


    

        
      