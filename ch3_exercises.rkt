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
                   (let ([fst (car x)]
                         [rands (parse-args (cdr x))])
                     (cond
                       [(eqv? fst '+) (primapp-exp (add-prim) rands)]
                       [(eqv? fst '-) (primapp-exp (sub-prim) rands)]
                       [(eqv? fst '*) (primapp-exp (mul-prim) rands)]
                       [(eqv? fst 'add1) (primapp-exp (add1-prim) rands)]
                       [(eqv? fst 'sub1) (primapp-exp (sub1-prim) rands)]
                       [else eopl:error])))]
      [else eopl:error])))

; SL -> List-of expression (or error)
(define parse-args
  (lambda (xs)
    (cond
      [(null? xs) '()]
      [(eqv? (length xs) 1) (parse-arg (car xs))]
      [else ; list of at least length 2
       (cond
         [(list? (car xs)) (append (primapp-exp (parse-args (car xs)) (parse-args (cdr xs))))]
         [else (cons (parse-arg (car xs)) (parse-args (cdr xs)))])])))




; Atom -> List-of expression (or error)
(define parse-arg
  (lambda (x)
    (cond
      [(number? x) '(lit-exp x)]
      [(symbol? x) (cond
                     [(eqv? x '+) '(add-prim)]
                     [(eqv? x '-) '(sub-prim)]
                     [(eqv? x '*) '(mul-prim)]
                     [(eqv? x 'add1) '(add1-prim)]
                     [(eqv? x 'sub1) '(sub1-prim)]
                     [else eopl:error])]
      [else eopl:error])))


(define arity-check
  (lambda (rst n)
    (let ([l (length rst)]) 
      (cond
        [(eqv? n l) rst]
        [else eopl:error]))))
      



    

        
      