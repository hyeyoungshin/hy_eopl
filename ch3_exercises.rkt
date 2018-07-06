#lang plai-typed

(require (typed-in racket [ormap : (('T -> boolean) (listof 'T) -> boolean)]))
(require (typed-in racket [andmap : (('T -> boolean) (listof 'T) -> boolean)]))
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

; For substitution based interpreter
(define-type proc 
  (lam (id : symbol)
       (body : expression)))

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

; expression -> bool
(define closed? : (expression -> boolean)
  (lambda (e)
    (type-case expression e
      (lit-exp (datum) true)
      (var-exp (id) false)
      (primapp-exp (prim rands) (andmap (lambda (r) (closed? r)) rands))
      (let-exp (id rand body) (closed? (subst body id rand)))
      (proc-exp (id body) (type-case expression body
                            (var-exp (x) (symbol=? id x))
                            (else (closed? body))))
      (app-exp (rator rand) (and (closed? rator) (closed? rand))))))
                
      

; expression symbol expression -> expression
(define subst
  (lambda (exp sub-id val-exp)
    (let ((subst-rands (lambda (rands) (map (lambda (rand) (subst rand sub-id val-exp)) rands)))
          (redundant? (lambda (ids) (ormap (lambda (id) (symbol=? sub-id id)) ids))))
      (if (closed? val-exp)
          (type-case expression exp
            (lit-exp (datum) exp)
            (var-exp (id) (if (equal? id sub-id)
                              val-exp
                              exp))
            (primapp-exp (prim rands)
                         (primapp-exp prim (subst-rands rands)))
            #;(if-exp (test-exp true-exp false-exp)
                      (if-exp (subst test-exp sub-id val-exp)
                              (subst true-exp sub-id val-exp)
                              (subst false-exp sub-id val-exp)))
            (let-exp (id rand body)
                     (if (symbol=? id sub-id)
                         exp
                         (let-exp id (subst rand sub-id val-exp) (subst body sub-id val-exp))))
            (proc-exp (id body)
                      (if (symbol=? id sub-id)
                          exp
                          (proc-exp id (subst body sub-id val-exp))))
            (app-exp (rator rand)
                     (app-exp (subst rator sub-id val-exp) (subst rand sub-id val-exp))))
          (error 'subst "can't substitute an open term")))))

(test (subst (lit-exp 5) 'x (lit-exp 3)) (lit-exp 5))
(test (subst (var-exp 'x) 'x (lit-exp 3)) (lit-exp 3))
(test (subst (var-exp 'x) 'y (lit-exp 3)) (var-exp 'x))
(test (subst (proc-exp 'x (primapp-exp (add-prim) (list (var-exp 'x) (var-exp 'y)))) 'y (lit-exp 3))
              (proc-exp 'x (primapp-exp (add-prim) (list (var-exp 'x) (lit-exp 3)))))
(test (subst (proc-exp 'x (primapp-exp (add-prim) (list (var-exp 'x) (lit-exp 3)))) 'x (lit-exp 5))
              (proc-exp 'x (primapp-exp (add-prim) (list (var-exp 'x) (lit-exp 3)))))
(test (subst (proc-exp 'f (proc-exp 'x (app-exp (var-exp 'f) (var-exp 'x)))) 'f (proc-exp 'x (var-exp 'x)))
      (proc-exp 'f (proc-exp 'x (app-exp (var-exp 'f) (var-exp 'x)))))
(test/exn (subst (proc-exp 'x (var-exp 'y)) 'y (var-exp 'x))
      "can't substitute an open term") ;<-------------------------------------------------------------------------------------------------------------failing

; Solution: 
; I make the interpreter not allow a free variable to be applied to a function.
; A free variable should not appear in function's argument position.
(test (subst (proc-exp 'f (proc-exp 'x (app-exp (var-exp 'g) (var-exp 'x)))) 'g (proc-exp 'x (var-exp 'x)))
      (proc-exp 'f (proc-exp 'x (app-exp (proc-exp 'x (var-exp 'x)) (var-exp 'x)))))

    
; listof expression -> value
(define eval-rands-subst
  (lambda ([rands : (listof expression)])
    (map (lambda (r) (eval-rand-subst r)) rands)))

; expression -> value
(define eval-rand-subst
  (lambda ([rand : expression])
    (eval-expression-subst rand)))

; primitive listof expression -> value
(define apply-primitive : (primitive (listof value) -> value)
  (lambda (prim args)
    (type-case primitive prim
      (add-prim () (num (+ (val-to-num (first args)) (val-to-num (second args)))))
      (sub-prim () (num (- (val-to-num (first args)) (val-to-num (second args)))))
      (mul-prim () (num (* (val-to-num (first args)) (val-to-num (second args)))))
      (add1-prim () (num (+ (val-to-num (first args)) 1)))
      (sub1-prim () (num (- (val-to-num (first args)) 1))))))
                   
; program -> value
(define eval-program-subst
  (lambda ([p : program])
    (type-case program p
      (a-program (body)
                 (eval-expression-subst body)))))

(define val-to-exp
  (lambda ([v : value])
    (type-case value v
      (num (n) (lit-exp n))
      (fun (f) (type-case proc f
                 (lam (id body) (proc-exp id body)))))))

(define val-to-num
  (lambda (v)
    (type-case value v
      (num (n) n)
      (fun (f) (error 'val-to-num "not a number")))))
  
; expression -> value or error
(define eval-expression-subst : (expression -> value)
  (lambda ([exp : expression])
    (type-case expression exp
      (lit-exp (datum) (num datum))
      (var-exp (id) (error 'eval-expression-subst (string-append (to-string id) "is a free identifier")))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands-subst rands)))
                     (apply-primitive prim args)))
      #;(if-exp (test-exp true-exp false-exp)
                (if (true-value? (eval-expression-subst test-exp))
                    (eval-expression-subst true-exp)
                    (eval-expression-subst false-exp)))
      (let-exp (id rand body)
               (let ((arg (val-to-exp (eval-expression-subst rand)))); call-by-value
                 (eval-expression-subst (subst body id arg))))
      (proc-exp (id body) (fun (lam id body)))
      (app-exp (rator rand)
               (let ((function (eval-expression-subst rator))
                     (arg (val-to-exp (eval-expression-subst rand))))
                 (type-case value function
                   (fun (f)
                        (type-case proc f
                          (lam (id body)
                               (eval-expression-subst (subst body id arg)))))
                   (num (n)
                        (error 'eval-expression-subst "Attempt to apply non-procedure"))))))))

(test 

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
      


    

        
      