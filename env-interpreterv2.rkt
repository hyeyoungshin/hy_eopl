#lang plai-typed

(require (typed-in racket [ormap : (('T -> boolean) (listof 'T) -> boolean)]))
(require (typed-in racket [andmap : (('T -> boolean) (listof 'T) -> boolean)]))

;(module+ env-interface plai-typed
  (define-type environment
  (empty-env)
  (extend-env
   (syms : (listof symbol))
   (vals : (listof value))
   (env : environment)))


(define apply-env
  (lambda (env sym)
    (type-case environment env
      (empty-env ()
                 (error 'apply-env (string-append "No binding for " (to-string sym))))
      (extend-env (syms vals env1)
                  (let ((pos (list-find-position sym syms)))
                    (type-case value pos
                      (num (n) (list-ref vals n))
                      (else (apply-env env sym))))))))

(define list-ref : ((listof value) number -> value)
  (lambda (vals i)
    (cond
      ((eq? i 0) (first vals))
      (else (list-ref (rest vals) (- i 1))))))

(define list-find-position : (symbol (listof symbol) -> value)
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
(define list-index
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
  (lit-exp
   (datum : number))
  (b-exp
   (b : boolean))
  (var-exp
   (id : symbol))
  (primapp-exp
   (prim : primitive)
   (rands : (listof expression)))
  (if-exp
   (test-exp : expression)
   (true-exp : expression)
   (false-exp : expression))
  (let-exp
   (ids : (listof symbol))
   (rands : (listof expression))
   (body : expression))
  (proc-exp
   (ids : (listof symbol))
   (body : expression))
  (app-exp
   (rator : expression)
   (randa : (listof expression))))

(define-type primitive
  (add-prim) ; add-prim is a function if you do (add-prim) you are calling it
  (sub-prim)
  (mul-prim)
  (add1-prim)
  (sub1-prim))

(define-type value
  (num (n : number))
  (truth (b : boolean))
  (fun (clo : procval)))

(define-type procval
  (closure
   (ids : (listof symbol))
   (body : expression)
   (env : environment)))

(define apply-procval : (procval (listof value) -> value)
  (lambda (proc args)
    (type-case procval proc
      (closure (ids body env) (eval-expression body (extend-env ids args env))))))


(define true-value?
  (lambda (x)
    (type-case value x
      (num (n) (not (eq? n 0)))
      (else #f))))
    


(define val-to-num : (value -> number)
  (lambda (v)
    (type-case value v
      (num (n) n)
      (else (error 'val-to-num "not a number")))))

(define apply-primitive : (primitive (listof value) -> value)
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
     (list 'i 'v 'x)
     (list (num 1) (num 5) (num 10))
     (empty-env))))

(define eval-program
  (lambda (pgm)
    (type-case program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))


; is it better to return something of value that I defined
; or something that is a value in the plai-typed language?
(define eval-expression : (expression environment -> value)
  (lambda (exp env)
    (type-case expression exp
      (lit-exp (datum) (num datum))
      (b-exp (b) (truth b))
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body (extend-env ids args env))))
      (proc-exp (ids body) (fun (closure ids body env)))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (type-case value proc
                   (fun (clo) (apply-procval clo args))
                   (else (error 'eval-expression (string-append "Attempt to apply non-procedure " (to-string proc))))))))))

(define eval-rands : ((listof expression) environment -> (listof value))
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand : (expression environment -> value)
  (lambda (rand env)
    (eval-expression rand env)))


(define make-interpreter : (closure environment -> (program -> value))
  (lambda (clo env)
    (eval-program clo env)))


;; S-expression program examples:

#;'(let f = proc (y z) + (y (- z 5))
   in (f 2 28))

#;'(let x = 5
   in (let f = proc (y z) (+ y (- z x))
           x = 28
        in (f 2 x)))