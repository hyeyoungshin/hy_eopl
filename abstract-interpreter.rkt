#lang racket

;; ---------------------------------------------------------------------------------------------------
;; here is the abstract set up 

;; type AST
(struct a-program (an-expression))
(struct an-expression (const-exp bool-exp var-exp diff-exp if-exp let-exp proc-exp app-exp))

(struct const-exp (num))
(struct bool-exp (bool))
(struct var-exp (id))
(struct diff-exp (e1 e2))
(struct if-exp (test true-branch false-branch))
(struct let-exp (id exp body))
(struct proc-exp (var body))
(struct app-exp (rator rand))

;; type Value
(struct a-value (num-val bool-val proc-val))

(struct num-val (n))
(struct bool-val (b))
(struct proc-val (f))

(struct environment-representation (new extend lookup))
;; ER =
#; {type Environment;
         (environment-representation
          new : -> Environment;
          extend : Environment Symbol Value -> Environment;
          lookup : Environment Symbol -> Value)}

(struct closure-representation (make apply))
;; CR = environment-representation interpreter -> 
#; (type AST, type ER 
      -> {type Closure;
               (closure-representation
                make : AST Enviroment -> Closure
                apply : Closure Value -> Value)})

#; (ER (ER (AST ER.Environment -> Value) -> CR) -> [AST -> Value])
(define (make-interpreter an-environment-representation make-a-closure-representation) 

  (match-define (environment-representation new extend lookup)
    an-environment-representation) ; evaluates the argument passed as `an-environment-representation` and match the result with `new` `extend` `lookup`
  
  (match-define (closure-representation make apply)
    (make-a-closure-representation an-environment-representation
                                   ;; in principle: expression-interpreter
                                   ;; but to get the mutual recursion set up, we need this: 
                                   (lambda (expression environment)
                                     (expression-interpreter expression environment))))

  ;; AST -> Value 
  (define (program-interpreter program)
    (expression-interpreter program (new))) ;; <-- fix this 

  ;; AST an-environment-representation.Environment -> Value
  (define (expression-interpreter expression environment)
    0) ;; <--- and that, and it will all work 

  ;; - - -

  program-interpreter)

;; ---------------------------------------------------------------------------------------------------
;; here is a concrete instantiation 

;; ER 
(define environments-as-closures
  ;; type Environment = Symbol -> Value 
  (environment-representation
   (lambda () (lambda (x) (error 'lookup "not found: ~e" x)))
   (lambda (e x a) (lambda (y) (if (eq? x y) a (e y))))
   (lambda (e x) (e x))))

;; (ER INT -> CR)
(define (make-closures-as-structs an-environment-representation interpreter) ; defines a procedure named `make-closures-as-structs` which takes two arguments
  (match-define (environment-representation new extend lookup)               ; `an-environment-representation` and `interpreter`.
    an-environment-representation)
  
  (struct closure (parameter body env))
  ;; type Closure = (closure Symbol AST an-environment-representation.Environment 

  (closure-representation
   closure                      ; argument for the `make` parameter
   (lambda (a-closure argument) ; argument for the `apply` parameter
     (match-define (closure parameter body env) a-closure)
     (interpreter body (extend env parameter argument)))))

;; TODO
#;(define (make-closures-as-functions an-environment-representation interpreter)
  (match-define (environment-representation new extend lookup)
    an-environment-representation)
  (closure-representation
   ; how to make a closure procedurally here
   (lambda (a-closure argument)
     ; how to apply a procedural closure here
     )))

;; AST -> Value
(define one-interpreter (make-interpreter environments-as-closures make-closures-as-structs))

;; TODO
#;(define another-interpreter (make-interpreter environments-as-ribcage make-clusres-as-functions))

;; RUN Lola RUN 
(one-interpreter '((define (f x) 1) (f 0)))