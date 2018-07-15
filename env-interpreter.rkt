#lang plai-typed

(require (typed-in racket [ormap : (('T -> boolean) (listof 'T) -> boolean)]))
(require (typed-in racket [andmap : (('T -> boolean) (listof 'T) -> boolean)]))
;(require (only-in lang/htdp-beginner ...))
(require plai-typed/s-exp-match)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datatype definitions:                             ;;
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
  #;(if-exp
   (test-exp : expression)
   (true-exp : expression)
   (false-exp : expression))
  (let-exp
   (id : symbol)
   (rand : expression)
   (body : expression))
  (proc-exp
   (id : symbol)
   (body : expression))
  (app-exp
   (rator : expression)
   (rand : expression)))

(define-type primitive
  (add-prim) ; add-prim is a function if you do (add-prim) you are calling it
  (sub-prim)
  (mul-prim)
  (add1-prim)
  (sub1-prim))

(define-type value
  (num (n : number))
  (fun (f : proc)))
  
  

; For environment passing interpreter
#;(define-type procval procval?
   (closure
    (ids (list-of symbol?))
    (body expression?)
    (env environment?)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Front End (Sexpr -> AST)                      ;;
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
;; Environment passing interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\

; program -> value
(define eval-program
  (lambda (p)
    (type-case program p
      (a-program (body)
                 (eval-expression body (init-env))))))

; expression listof value -> value
(define eval-expression : (expression (listof value) -> value)
    (lambda (exp env)
      (type-case expression exp
        (lit-exp (datum) (num datum))
        (var-exp (id) (apply-env env id))
        (primapp-exp (prim rands)
                     (let ((args (eval-rands rands env)))
                       (apply-primitive prim args)))
        #;(if-exp (test-exp true-exp false-exp)
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
                       (error 'eval-expression "Attempt to apply non-procedure ")))))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     '(1 5 10)
     (empty-env))))

; (listof value) symbol -> value
(define apply-env : ((listof value) symbol -> value)
  (lambda (env id)
    (cond
      [(null? env) (error 'apply-env "empty environment")]
      [else (if (equal? (first (first env)) id)
                (first (rest (first env)))
                (eopl:error "unbound variable"))])))

(define empty-env '())

(define extend-env
  (lambda (ids vals cur-env)
    (append (zip ids vals) cur-env)))

(define zip
  (lambda (ids vals)
    (cond
      [(and (null? ids) (null? vals)) empty-env]
      [else (if (equal? (length ids) (length vals))
                (cons '((first ids) (first vals)) (zip (rest ids) (rest vals)))
                (eopl:error "ids and vals don't match up"))])))
      
