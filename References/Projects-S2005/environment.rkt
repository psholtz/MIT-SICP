;;
;; environment.scm - 6.001 Spring 2005
;; implementation of meval environment model
;;

; double bubbles
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? exp)
  (tagged-list? exp 'procedure))
(define (procedure-parameters p) (second p))
(define (procedure-body p) (third p))
(define (procedure-environment p) (fourth p))

; environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; bindings
(define (make-binding var val)
  (list var val))
(define binding-variable car)
(define binding-value cadr)
(define binding-search (association-procedure eq? car))
(define (set-binding-value! binding val)
  (set-car! (cdr binding) val))

; frames
(define (make-frame variables values)
  (cons 'frame (map make-binding variables values)))
(define (frame-variables frame) (map binding-variable (cdr frame)))
(define (frame-values frame) (map binding-value (cdr frame)))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (make-binding var val) (cdr frame))))
(define (find-in-frame var frame)
  (binding-search var (cdr frame)))

; drop a frame
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many args supplied" vars vals)
          (error "Too few args supplied" vars vals))))

; name rule
(define (lookup-variable-value var env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- LOOKUP" var)
      (let* ((frame (first-frame env))
	     (binding (find-in-frame var frame)))
	(if binding
	    (binding-value binding)
	    (lookup-variable-value var (enclosing-environment env))))))

(define (set-variable-value! var val env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- LOOKUP" var)
      (let* ((frame (first-frame env))
	     (binding (find-in-frame var frame)))
	(if binding
	    (set-binding-value! binding val)
	    (set-variable-value! var val (enclosing-environment env))))))

(define (define-variable! var val env)
  (warn-if-defined-in-regular-scheme var)
  (let* ((frame (first-frame env))
	 (binding (find-in-frame var frame)))
    (if binding
	(set-binding-value! binding val)
	(add-binding-to-frame! var val frame))))

; primitives procedures - hooks to MITScheme procs
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (primitive-procedures)
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
	(list 'set-car! set-car!)
	(list 'set-cdr! set-cdr!)
        (list 'null? null?)
        (list '+ +)
	(list '- -)
	(list '< <)
        (list '> >)
        (list '= =)
	(list 'display display)
	(list 'not not)
        ; ... more primitives
        ))

(define (primitive-procedure-names) (map car (primitive-procedures)))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) (primitive-procedures)))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

; used to initialize the environment
(define (setup-environment)
  (let ((initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment))
	(oldwarn *meval-warn-define*))
    (set! *meval-warn-define* #f)
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    (set! *meval-warn-define* oldwarn)
    initial-env))

(define the-global-environment (setup-environment))

(define (refresh-global-environment)
  (set! the-global-environment (setup-environment))
  'done)