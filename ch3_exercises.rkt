#lang eopl

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

; Sexp -> Expression
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
      [else eopl:error])))


; List-of Sexpr -> List-of Expression
(define sexp-to-list
  (lambda (a)
    (cond
      [(null? a) '()]
      [else (cons (sexp-to-exp (car a)) (sexp-to-list (cdr a)))])))
    


    

        
      