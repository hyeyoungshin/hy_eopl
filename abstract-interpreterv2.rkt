#lang racket

#;(require expect/rackunit)

;; ---------------------------------------------------------------------------------------------------
;; here is the abstract set up 

;; type AST
#;(struct a-program (an-expression))
#;(struct an-expression (const-exp bool-exp var-exp diff-exp if-exp let-exp proc-exp app-exp))

(struct const-exp (num))
(struct bool-exp (bool))
(struct var-exp (id))
(struct diff-exp (e1 e2))
(struct if-exp (test true-branch false-branch))
(struct let-exp (id exp body))
(struct proc-exp (var body))
(struct app-exp (rator rand))

;; type Value
#;(struct a-value (num-val bool-val proc-val))

(struct num-val (num))
(struct bool-val (bool))
(struct proc-val (fun))

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
                make : Symbol Expression Enviroment -> Closure
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

  ;; a-program -> Value 
  (define (program-interpreter program)
    (expression-interpreter program (new))) ;; <-- fix this 

  ;; an-exoression an-environment-representation.Environment -> Value
  (define (expression-interpreter exp env)
    (match exp
      [(const-exp num) (num-val num)]
      [(bool-exp bool) (bool-val bool)]
      [(var-exp id) (lookup env id)]
      [(diff-exp e1 e2) (num-val (- (val->num (expression-interpreter e1 env)) (val->num (expression-interpreter e2 env))))]
      [(if-exp test true-branch false-branch)
       (if (val->bool (expression-interpreter test env))
           (expression-interpreter true-branch env)
           (expression-interpreter false-branch env))]
      [(let-exp id exp body) (let ((val (expression-interpreter exp env)))
                               (expression-interpreter body (extend env id val)))]
      [(proc-exp var body-exp) (proc-val (make var body-exp env))]
      [(app-exp rator rand)
       (let ((fun (expression-interpreter rator env))
             (arg (expression-interpreter rand env)))      ; <-- c-b-v 
         (match fun
           [(proc-val f) (apply f arg)]
           [_ (error "applying to a non-function")]))]))

  program-interpreter)

(define (val->num val)
  (match val
    [(num-val n) n]
    [_ (error "not a num-val")]))

(define (val->bool val)
  (match val
    [(bool-val b) b]
    [_ (error "not a bool-val")]))



;; ---------------------------------------------------------------------------------------------------
;; here is a concrete instantiation 

;; ER 
(define environments-as-closures
  ;; type Environment = Symbol -> Value 
  (environment-representation
   (lambda () (lambda (x) (error 'lookup "not found: ~e" x)))
   (lambda (e x a) (lambda (y) (if (eq? x y) a (e y))))
   (lambda (e x) (e x))))

(define environments-as-ribcage
  ;; type Environment = [Listof [List Symbol Value]]
  ; helper function
  ; returns the value in the list at the index 
  (environment-representation
   ;; -> Env
   (lambda () '())
   ;; Env Symbol Value -> Env
   (lambda (env syms vals) ; list of variagles list of values environment -> environment
     (cons (list syms vals) env))
   ;; Env Symbol -> Value 
   (lambda (env sym)
     (if (null? env)
         (error (string-append "no binding for "(symbol->string sym)))
         (let ((syms    (caar env)) ;; is of type Sym
               (vals    (cadr (car env))) ;; is of type Val
               (env-old (cdr env)))
           (if (eq? sym syms)
               vals
               [ (environment-representation-lookup environments-as-ribcage) env-old sym]
               ))))))
            

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


(define (make-closures-as-functions an-environment-representation interpreter)
  (match-define (environment-representation new extend lookup)
    an-environment-representation)

  (define (make var body-exp env)
    (lambda (arg-val)           ; argument is always in a value form
      (interpreter body-exp (extend env var arg-val))))
  
  (define (apply clo arg-val)
    [clo
     arg-val])
    
  (closure-representation
   make
   apply))

;; AST -> Value
(define one-interpreter          (make-interpreter environments-as-closures make-closures-as-structs))

(define another-interpreter      (make-interpreter environments-as-ribcage make-closures-as-functions))

(define weird-interpreter        (make-interpreter environments-as-closures make-closures-as-functions))

(define even-weirder-interpreter (make-interpreter environments-as-ribcage make-closures-as-structs))

(define (check-val val)
  (match val
    [(num-val n) n]
    [(bool-val b) b]
    [(proc-val f) f]
    [_ (error "not a value")]))

;; RUN Lola RUN
(check-val (one-interpreter (const-exp 0)))
(check-val (one-interpreter (bool-exp #t)))
(check-val (another-interpreter (const-exp 0)))
(check-val (one-interpreter (let-exp 'x (const-exp 2) (diff-exp (var-exp 'x) (const-exp 1)))))
(check-val (one-interpreter (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 42)))))


(check-val (one-interpreter (app-exp (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 42))) (const-exp 100)))) ; should be 100-42 = 58

(check-val (another-interpreter (app-exp (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 42))) (const-exp 100)))) ; should be 100-42 = 58, but 100
(check-val (another-interpreter (diff-exp (const-exp 4) (const-exp 1))))  ; should be 3
(check-val (another-interpreter (app-exp (proc-exp 'x (const-exp 4)) (const-exp 2)))) ; should be 4 (constant function), but 2
(check-val (weird-interpreter (app-exp (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 42))) (const-exp 100)))) ; should be 58, but 100
(check-val (even-weirder-interpreter (app-exp (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 42))) (const-exp 100)))) ; should be 58

(check-val (another-interpreter
            (let-exp 'x (const-exp 9)
                     (let-exp 'y (const-exp 1)
                              (var-exp 'x)))))
(require rackunit)
(define (make-testsuits interpreter msg)
  (check-equal? (check-val (interpreter (const-exp 0))) 0 msg)
  #;(check-equal? (check-val (interpreter (const-exp 0))) 6666 msg)
  (check-equal? (check-val (interpreter (let-exp 'x (const-exp 1)
                                                 (let-exp 'y (const-exp 2)
                                                          (let-exp 'x (const-exp 3)
                                                                   (var-exp 'x)))))) 3)
  (check-equal? (check-val (interpreter (let-exp 'f (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 1)))
                                                 (app-exp (var-exp 'f) (const-exp 42))))) 41)
  (check-equal? (check-val (interpreter (app-exp (app-exp (proc-exp 'x (var-exp 'x)) (proc-exp 'y (const-exp 10))) (const-exp 0)))) 10))

(make-testsuits one-interpreter "one")
(make-testsuits another-interpreter "another")
(make-testsuits weird-interpreter "weird")
(make-testsuits even-weirder-interpreter "weirder")