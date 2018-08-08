#lang plai-typed

(require (typed-in racket [ormap : (('T -> boolean) (listof 'T) -> boolean)]))
(require (typed-in racket [andmap : (('T -> boolean) (listof 'T) -> boolean)]))

;; data definition for environment
(define-type environment
  (empty-env)
  (extend-env
    (sym : symbol)
    (val : expval)
    (env : environment))
  (extend-env*
   (syms : (listof symbol))
   (vals : (listof expval))
   (env : environment)))


(define apply-env
  (lambda (env sym)
    (type-case environment env
      (empty-env ()
                 (error 'apply-env (string-append "No binding for " (to-string sym))))
      (extend-env (sym1 val env1)
                  (if (eq? sym sym1)
                      val
                      (apply-env env1 sym)))
      (extend-env* (syms vals env1)
                   (let ((pos (list-find-position sym syms)))
                     (type-case expval pos
                       (num-val (n) (list-ref vals n)) 
                       (else (apply-env env sym))))))))


;; the interface bundle 
(define-type representationof-environment
  (env-interface
    (empty-env : (-> environment))
    (extend-env : (symbol expval environment -> environment))
    (apply-env : (environment symbol -> expval))))


;; procedural implementation of environment
#;(define empty-env
  (lambda ()
    (lambda (sym)
      (error 'apply-env (string-append "No binding for " (to-string sym))))))

#;(define extend-env : ((listof symbol) (listof value) environment -> expval)
  (lambda (syms vals env)
    (lambda (sym)
      (let ((pos (list-find-position sym syms)))
        (type-case expval pos
          (num (n) (list-ref vals n))
          (else (apply-env env sym)))))))

#;(define apply-env : (environment symbol -> value)
  (lambda (env sym)
    (env sym)))



;; ribcage implementation of environment
#;(define empty-env
  (lambda () '()))

#;(define extend-env*
  (lambda (syms vals env)
    (let ((rib (pair syms vals)))
      (cons (pair rib env)))))

#;(define apply-env
  (lambda (env sym)
    (type-case environment env
      (empty-env () (error 'apply-env (string-append "No binding for " (to-string sym))))
      (extend-env (sym1 val env1)
                  (if (eq? sym sym1)
                      val
                      (apply-env env1 sym)))
      (extend-env* (syms vals env1)
                   (let ((pos (list-find-position sym syms)))
                     (type-case value pos
                       (num (n) (list-ref vals n)) 
                       (else (apply-env env sym))))))))

(define list-find-position : (symbol (listof symbol) -> expval)
  (lambda (sym los)
    (list-index (lambda (sym1) (symbol=? sym1 sym)) los)))

(define list-index : ((symbol -> boolean) (listof symbol) -> expval)
  (lambda (compare ls)
    (cond
      ((empty? ls) (error 'list-index "empty environment"))
      ((compare (first ls)) (num-val 0))
      (else (list-index compare (rest ls))))))
              
(define list-ref : ((listof expval) number -> expval)
  (lambda (vals i)
    (cond
      ((eq? i 0) (first vals))
      (else (list-ref (rest vals) (- i 1))))))




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

(define-type expval
  (num-val (n : number))
  (bool-val (b : boolean))
  (proc-val (f : proc)))

(define-type proc
  (procedure                  ; called "closure" in the 2nd ed.
   (id : symbol)
   (body : expression)
   (env : environment)))

(define apply-procedure : (proc expval -> expval)
  (lambda (f v)
    (type-case proc f
      (procedure (id body env) (eval-expression body (extend-env id v env))))))

(define-type closure-interface
  (clo-interface
   (procedure : (symbol expression environment -> proc))
   (apply-procedure : (proc expval -> expval))))

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
                 (eval-expression body (lambda ()
                                         (extend-env
                                          'i (num-val 1)
                                          (extend-env
                                           'v (num-val 5)
                                           (extend-env
                                            'y (num-val 10)
                                            (empty-env))))))))))

(define eval-expression : (expression environment -> expval)
  (lambda (exp env)
    (type-case expression exp
      (const-exp (n) (num-val n))
      (bool-exp (b) (bool-val b))
      (var-exp (id) (apply-env env id))
      (diff-exp (e1 e2) (num-val (- (expval->num (eval-expression e1 env)) (expval->num (eval-expression e2 env)))))
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
(test (eval-program (a-program (let-exp 'f (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 2))) (app-exp (var-exp 'f) (const-exp 3))))) (num-val 1))



#;(define-type representationof ...)
 
(define make-interpreter : (representationof-closure representationof-environment -> (program -> expval)) 
  (lambda (clo env)
    (type-case
        (env-interface (empty extend apply)
                       (type-case
                           (clo-interface (pro apply) (lambda (p)
                                                        (type-case program p
                                                          (a-program (body)
                                                                     (eval-expression body (lambda ()
                                                                                             (extend
                                                                                              'i (num-val 1)
                                                                                              (extend
                                                                                               'v (num-val 5)
                                                                                               (extend
                                                                                                'y (num-val 10)
                                                                                                (empty-env)))))))))))))))
                       
                       

