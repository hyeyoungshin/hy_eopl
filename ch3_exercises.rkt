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
      [(list? x) (SL-to-exp x)]
      [else eopl:error])))
        

; SL -> Expression
(define SL-to-exp
  (lambda (x)
    (cond
      [(null? x) eopl:error]
      [else (cons (primapp-exp (atom-to-prim (car x)) (SL-to-list (cdr x))))])))
        
(define atom-to-prim
  (lambda (x)
    (cond
      [(symbol? x) (cond
                     [(eqv? x '+) (add-prim)]
                     [(eqv? x '-) (sub-prim)]
                     [(eqv? x '*) (mul-prim)]
                     [(eqv? x 'add1) (add1-prim)]
                     [(eqv? x 'sub1) (sub1-prim)]
                     [else eopl:error])]
      [else eopl:error])))

; List-of Sexpr -> List-of Expression
(define SL-to-list
  (lambda (a)
    (cond
      [(null? a) '()]
      [else (cons (sexp-to-exp (car a)) (SL-to-list (cdr a)))])))
    


    

        
      