#lang plai-typed

(require (typed-in racket [ormap : (('T -> boolean) (listof 'T) -> boolean)]))
(require (typed-in racket [andmap : (('T -> boolean) (listof 'T) -> boolean)]))
;(require (only-in lang/htdp-beginner ...))
;(require plai-typed/s-exp-match)


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Closure                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In order for a procedure to retain the bindings that its free variables
;; had at the time it was created, it must be a closed package, independent
;; of the environment in which it is used.
;; Such a package is called "closure"
;; In order to be self-contained, a closure must contain the procedure body,
;; the list of formal parameters, and the bindings of its free variables.

;; Representation 1: Procedural (functional) representation
;;   Define closure to have a value that is a procedure that expects an
;;   argument list

#;(define closure
  (lambda (ids body env)
    (lambda (args)
      (eval-expression body (extend-env ids args env)))))

#;(define apply-procval
    (lambda (proc args)
      (proc args)))


;; Representation 2: Abstract syntax tree (structural) representation
    
(define-type procval
   (closure ;<------------------ tells how to build a procedure value
    (ids : (listof symbol))
    (body : expression)
    (env : environment)))

(define apply-procval ; <------- tells how to apply a procedure value
  (lambda (proc args)
    (type-case procval proc
      (closure (ids body env)
        (eval-expression body (extend-env ids args env))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  #;(if0-exp
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
   (rands : (listof expression))))

(define-type primitive
  (add-prim) ; add-prim is a function if you do (add-prim) you are calling it
  (sub-prim)
  (mul-prim)
  (add1-prim)
  (sub1-prim))

(define-type value
  (num (n : number))
  (fun (f : procval)))

; value -> expression
(define val-to-exp
  (lambda ([v : value])
    (type-case value v
      (num (n) (lit-exp n))
      (fun (f) (type-case procval f
                 (closure (ids body env) (proc-exp ids body)))))))

; value -> expression or error
(define val-to-num 
  (lambda (v)
    (type-case value v
      (num (n) n)
      (fun (f) (error 'val-to-num "not a number")))))

(define apply-primitive : (primitive (listof value) -> value)
  (lambda (prim args)
    (type-case primitive prim
      (add-prim () (num (+ (val-to-num (first args)) (val-to-num (second args)))))
      (sub-prim () (num (- (val-to-num (first args)) (val-to-num (second args)))))
      (mul-prim () (num (* (val-to-num (first args)) (val-to-num (second args)))))
      (add1-prim () (num (+ (val-to-num (first args)) 1)))
      (sub1-prim () (num (- (val-to-num (first args)) 1))))))

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
#;(test (sexp-to-program '(if0 (add1 3) 4 5))
      (a-program
       (if0-exp
        (add1-prim (lit-exp 3))
        (lit-exp 4)
        (lit-exp 5))))
        
    


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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     '(1 5 10)
     (empty-env))))

; program -> value
(define eval-program
  (lambda (p)
    (type-case program p
      (a-program (body)
                 (eval-expression body (init-env))))))

; expression (listof value) -> value
(define eval-expression : (expression (listof (listof symbol)) -> value)
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
        (proc-exp (ids body) (fun (closure ids body env)))
        (app-exp (rator rands)
                 (let ((proc (eval-expression rator env))
                       (args (eval-rands rands env)))
                   (type-case value proc
                     (num (n) (error 'eval-expression "Attempt to apply non-procedure "))
                     (fun (f) (apply-procval f args))))))))

(define eval-rands : ((listof expression) environment -> (listof value))
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand : (expression environment -> value)
  (lambda (rand env)
    (eval-expression rand env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Representations of Environment                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Representation 1: Procedural representation of environments

;; empty-env: 
;; When passed any symbol whatwoever, it indicates with an error message
;; that the given symbol is not in its domain
#;(define empty-env
  (lambda ()
    (lambda (sym)
      (error 'apply-env (string-append "No binding for " (to-string sym))))))

;; extend-env:
;; returns a new procedure that represents the extended environment
; (listof Symbols) (listof Vals) -> (Symbol -> Value)
#;(define extend-env
  (lambda (syms vals env)
    (lambda (sym)
      (let ((pos (list-find-position sym syms))) ; finds the position of sym in syms
        (type-case value pos
          (num (n) (list-ref vals n)) ; returns the value in vals at position n
          (else (apply-env env sym)))))))
 
#;(define apply-env
  (lambda (env sym)
    (env sym)))


; Symbol (listof Symbol) -> Value or error
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (symbol=? sym1 sym)) los)))

; (Symbol -> Bool) (listof Symbol) -> Value or error
(define list-index
  (lambda (pred ls)
    (cond
      ((empty? ls) (error 'list-index "empty environment"))
      ((pred (first ls)) (num 0))
      (else (let ((list-index-r (list-index pred (rest ls))))
              (type-case value list-index-r
                (num (n) (num (+ n 1)))
                (fun (f) (error 'list-index "position not a number"))))))))

; (listof Value) Number -> Value
(define list-ref
  (lambda (vals i)
    (cond
      ((eq? i 0) (first vals))
      (else (list-ref (rest vals) (- i 1))))))
    

;; example
#;(define dxy-env
  (extend-env '(d x) '((num 6) (num 7))
              (extend-env '(y) '((num 8))
                          (empty-env))))

#;(test (apply-env dxy-env 'x) (num 7))
 
;; Representation 2 : AST
#;(define-type environment
  (empty-env-record)
  (extended-env-record
   (syms : (listof symbol))
   (vals : (listof value))
   (env : environment)))

#;(define empty-env
  (lambda ()
    (empty-env-record)))

#;(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

#;(define apply-env
  (lambda (env sym)
    (type-case environment env
      (empty-env-record () (error 'apply-env (string-append "No binding for " (to-string sym))))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (type-case value pos
                               (num (n) (list-ref vals pos))
                               (else apply-env env sym)))))))



#;(test (environment-to-list dxy-env) (extended-env-record (d x) (6 7)
                                                         (extended-env-record (y) (8)
                                                                              (empty-env-record))))


;; Representation 3. Ribcage
;; The environment is represented as a list of lists called ribs;
;; the car of each rib is a list of symbols and the cadr of each ribs
;; is the corresponding list of values
(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (syms vals env)
    (cons (list syms vals) env)))

(define apply-env
  (lambda (env sym)
    (if (empty? env)
        (error 'apply-env (string-append "No bindings for " (to-string sym)))
        (let ((syms (first (first env)))
              (vals (first (rest (first env))))  ;(cadr (car env)) returns vals (cdr (car env)) returns (vals), list containing vals, instead of vals
              (env (rest env)))
          (let ((pos (list-find-position sym syms)))
            (type-case value pos
              (num (n) (list-ref vals n))
              (else (apply-env env sym))))))))
               

; (listof value) symbol -> value
#;(define apply-env : ((listof value) symbol -> value)
  (lambda (env id)
    (cond
      [(null? env) (error 'apply-env "empty environment")]
      [else (if (equal? (first (first env)) id)
                (first (rest (first env)))
                (eopl:error "unbound variable"))])))


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
      
