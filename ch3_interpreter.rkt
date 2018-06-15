#lang eopl

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

(define eval-program
  (lambda (prog)
    (cases program prog
      (a-program (body)
                 (eval-expression body (init-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      ; apply-env looks up id in env and returns find the id's value
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      )))



(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (sub-prim () (- (car args) (cadr args)))
      (mul-prim () (* (car args) (cadr args)))
      (add1-prim () (+ (car args) (cadr args)))
      (sub1-prim () (+ (car args) (cadr args))))))

(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     '(1 5 10)
     (empty-env))))

(define scanner-spec-3-1
  '((white-sp
     (whitespace)                              skip)
     (comment
      ("%" (arbno (not #\newline)))            skip)
     (identifier
      (letter (arbno (or letter digit "?"))) symbol)
     (number
      (digit (arbno digit))                  number)))

(define grammar-3-1
  '((program
     (expression)
     a-program)
    (expression
     (number)
     lit-exp)
    (expression
     (id)
     var-exp)
    (expression
     (primitive "(" (separated-list expression ",") ")")
     primapp-exp)
    (primitive ("+")
               add-prim)
    (primitive ("-")
               sub-prim)
    (primitive ("*")
               mul-prim)
    (primitive ("add1")
               add1-prim)
    (primitive ("sub1")
               sub1-prim)))

(define scan&parse
  (sllgen:make-string-parser
   scanner-spec-3-1
   grammar-3-1))

(sllgen:make-define-datatypes scanner-spec-3-1 grammar-3-1)

(define run
  (lambda (string)
    (eval-program
     (scan&parse string))))

(scan&parse "add1 (2)")

              
  



