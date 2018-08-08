#lang plai-typed

(require (typed-in racket [ormap : (('T -> boolean) (listof 'T) -> boolean)]))
(require (typed-in racket [andmap : (('T -> boolean) (listof 'T) -> boolean)]))

(define-type environment
  (empty-env)
  (extend-env
  (sym : symbol)
  (val : expval)
  (env : environment)))


(define apply-env
  (lambda (env sym)
    (type-case environment env
      (empty-env ()
                 (error 'apply-env (string-append "No binding for " (to-string sym))))
      (extend-env (sym1 val env1)
                  (if (eq? sym sym1)
                      val
                      (apply-env env1 sym))))))
                  #;(let ((pos (list-find-position sym syms)))
                    (type-case value pos
                      (num (n) (list-ref vals n))
                      (else (apply-env env sym))))

#;(define list-ref : ((listof value) number -> value)
  (lambda (vals i)
    (cond
      ((eq? i 0) (first vals))
      (else (list-ref (rest vals) (- i 1))))))

#;(define list-find-position : (symbol (listof symbol) -> value)
  (lambda (sym los)
    (list-index (lambda (sym1) (eq? sym1 sym)) los)))

;; Procedural representation of environment
;; -empty-env
;; -extend-env
;; -apply-env
#;(define empty-env
  (lambda ()
    (lambda (sym)
      (error 'apply-env (string-append "No binding for " (to-string sym))))))

#;(define extend-env : ((listof symbol) (listof value) environment -> value)
  (lambda (syms vals env)
    (lambda (sym)
      (let ((pos (list-find-position sym syms)))
        (type-case value pos
          (num (n) (list-ref vals n))
          (else (apply-env env sym)))))))

#;(define apply-env : (environment symbol -> value)
  (lambda (env sym)
    (env sym)))






; if returns (num n), then n is the index of the symbol
; anything else means "symbol not found"
#;(define list-index
  (lambda (pred ls)
    (cond
      ((empty? ls) (truth false))
      ((pred (first ls)) (num 0))
      (else (let ((list-index-r (list-index pred (rest ls))))
              (type-case value list-index-r
                (num (n) (num (+ n 1)))
                (else list-index-r)))))))

;;;;;;;;;;;;;;;;;;;;  
(define-type program
  (a-program (exp : expression)))

(define-type expression
  (const-exp
   (num : number))
  (bool-exp
   (bool : boolean))
  (var-exp
   (id : symbol))
  (diff-exp
   (e1 : expression)
   (e2 : expression))
  #;(primapp-exp
   (prim : primitive)
   (rands : (listof expression)))
  (if-exp
   (test-exp : expression)
   (true-exp : expression)
   (false-exp : expression))
  (let-exp
   (id : symbol)
   (exp : expression)
   (body : expression))
  (proc-exp
   (var : symbol)
   (body : expression))
  (app-exp
   (rator : expression)
   (rand : expression)))

#;(define-type primitive
  (add-prim) ; add-prim is a function if you do (add-prim) you are calling it
  (sub-prim)
  (mul-prim)
  (add1-prim)
  (sub1-prim))

(define-type expval
  (num-val (n : number))
  (bool-val (b : boolean))
  (proc-val (f : proc)))

(define-type proc
  (procedure ; "closure" in the 2nd ed.
   (id : symbol)
   (body : expression)
   (env : environment)))

(define expval->num
  (lambda (val)
    (type-case expval val
      (num-val (n) n)
      (else (error 'expval->num "not a num-val")))))

(define expval->bool
  (lambda (val)
    (type-case expval val
      (bool-val (b) b)
      (else (error 'expval->num "not a bool-val")))))
   
(define apply-procedure : (proc expval -> expval)
  (lambda (f v)
    (type-case proc f
      (procedure (id body env) (eval-expression body (extend-env id v env))))))


#;(define true-value?
  (lambda (x)
    (type-case value x
      (num (n) (not (eq? n 0)))
      (else #f))))
    
#;(define val-to-num : (value -> number) 
  (lambda (v)
    (type-case value v
      (num (n) n)
      (else (error 'val-to-num "not a number")))))

#;(define apply-primitive : (primitive (listof value) -> value)
  (lambda (prim args)
    (type-case primitive prim
      (add-prim () (num (+ (val-to-num (first args)) (val-to-num (second args)))))
      (sub-prim () (num (- (val-to-num (first args)) (val-to-num (second args)))))
      (mul-prim () (num (* (val-to-num (first args)) (val-to-num (second args)))))
      (add1-prim () (num (+ (val-to-num (first args)) 1)))
      (sub1-prim () (num (- (val-to-num (first args)) 1))))))

(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'y (num-val 10)
       (empty-env))))))


(define eval-program : (program -> expval)
  (lambda (p)
    (type-case program p
      (a-program (body)
                 (eval-expression body (init-env))))))

(define eval-expression : (expression environment -> expval)
  (lambda (exp env)
    (type-case expression exp
      (const-exp (n) (num-val n))
      (bool-exp (b) (bool-val b))
      (var-exp (id) (apply-env env id))
      (diff-exp (e1 e2) (num-val (- (expval->num (eval-expression e1 env)) (expval->num (eval-expression e2 env)))))
      #;(primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (let ((test (eval-expression test-exp env))
                    (true-case (eval-expression true-exp env))
                    (false-case (eval-expression false-exp env)))
                (type-case expval test
                  (bool-val (b) (if b true-case false-case))
                  (else (error 'eval-expression "expecting a bool-exp in the test expression")))))
      (let-exp (id e body)
               (let ((arg (eval-expression e env)))
                 (eval-expression body (extend-env id arg env))))
      (proc-exp (id body) (proc-val (procedure id body env)))
      (app-exp (rator rand)
               (let ((fun (eval-expression rator env))
                     (arg (eval-expression rand env)))
                 (type-case expval fun
                   (proc-val (f) (apply-procedure f arg))
                   (else (error 'eval-expression (string-append "Attempt to apply non-procedure " (to-string 'fun))))))))))

(test (eval-program (a-program (var-exp 'i))) (num-val 1))
(test (eval-program (a-program (let-exp 'x (const-exp 42) (diff-exp (var-exp 'x) (const-exp 2))))) (num-val 40))
(test (eval-program (a-program (let-exp 'i (const-exp 42) (diff-exp (var-exp 'i) (const-exp 2))))) (num-val 40)) ; shadowing works because apply-env checks give priority to smbols added more recently

#;(define eval-rands : ((listof expression) environment -> (listof value))
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

#;(define eval-rand : (expression environment -> value)
  (lambda (rand env)
    (eval-expression rand env)))


#;(define make-interpreter : ((symbol expression environment -> proc) (symbol -> expval) -> (program -> expval)) 
  (lambda (clo env)
    (eval-program clo env)))


;; S-expression program examples:

#;'(let f = proc (y z) + (y (- z 5))
   in (f 2 28))

#;'(let x = 5
   in (let f = proc (y z) (+ y (- z x))
           x = 28
        in (f 2 x)))