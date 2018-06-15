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


; S-expr -> program
(define program-to-list
  (lambda (x)
    (cond
      [(number? x) (a-program (lit-exp x))]
      [(symbol? x) (a-program (var-exp x))]
      [(list? x) (a-program
                  (primapp-exp
                   (let ([fst (car x)]
                         [rest (parse-args (cdr x))])
                     (cond
                       [(eqv? fst '+) add-prim rest]
                       [(eqv? fst '-) sub-prim rest]
                       [(eqv? fst '*) mul-prim rest]
                       [(eqv? fst 'add1) add1-prim rest]
                       [(eqv? fst 'sub1) sub1-prim rest]))))]
      [else eopl:error])))

(define parse-args
  (lambda (xs)
    (let ([fst (car xs)]
          [rest (parse-args (cdr xs))])
      (cond
        [(number? fst) ((lit-exp fst) rest)]
        [(symbol? fst) (cond
                              [(eqv? fst '+) add-prim rest]
                              [(eqv? fst '-) sub-prim rest]
                              [(eqv? fst '*) mul-prim rest]
                              [(eqv? fst 'add1) add1-prim rest]
                              [(eqv? fst 'sub1) sub1-prim rest]
                              [else (var-exp fst) rest])]
        [else eopl:error]))))
                            

    

        
      