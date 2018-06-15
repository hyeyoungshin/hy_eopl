#lang eopl
  
(define-datatype program program?
  (a-program (exp expression?)))

(define-datatype  expression expression?
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


; list-of source-exp -> program
(define program-to-list
  (lambda (x)
    (cond
      [(number? x) (a-program (lit-exp x))]
      [(symbol? x) (a-program (var-exp x))]
      [(list? x) (a-program
                  (primapp-exp
                   (cond
                     [(eqv? (car x) '+) (add-prim)]
                     [(eqv? (car x) '-) (sub-prim)]
                     [(eqv? (car x) '*) (mul-prim)]
                     [(eqv? (car x) 'add1) (add1-prim)]
                     [(eqv? (car x) 'sub1) (sub1-prim)])
                   (program-to-list (cdr x))))]
      [else eopl:error])))

                 

        
      