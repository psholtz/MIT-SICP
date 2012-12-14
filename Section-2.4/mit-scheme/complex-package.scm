;;
;; Define the full complex number package:
;;

;; Start by defining the table operations that are required:
(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records)) (car records))
	(else
	  (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; Also define the tagging procedures:
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

;; Define the rectangular package:
(define (install-rectangular-package)
  ;; Internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; Interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Define the polar package:
(define (install-polar-package)
  ;; Internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))

  ;; Interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Generic procedure application:
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; Finally, include arithmetic routines so that we can work with the data:
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		              (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
		              (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		          (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		          (- (angle z1) (angle z2))))

;;
;; There are several things about these procedures that are worth noting.
;;
;; In the first place, not that we store the four selectors "real-part", 
;; "imag-part", "magnitude" and "angle" in the table under the token
;; using a list structure (i.e., '(rectangular), while the two
;; constructors are stored using an ordinary token (i.e., 'rectangular).
;;
;; The reason for this is the way "apply-generic" maps the procedure
;; "type-tag" onto its arguments. Consider, for instance:
;;
(define z1 (make-from-rect-imag 2 3))
;; ==> (rectangular 2 . 3)
(define z2 (make-from-mag-ang 4 5))
;; ==> (polar 4 . 5)

(map type-tag (list z1))
;; ==> (rectangular)
(map type-tag (list z2))
;; ==> (polar)
(map type-tag (list z1 z2))
;; ==> (rectangular polar)

;;
;; Consequently, since the four selectors are accessed using the "apply-generic"
;; procedure, which uses such a mapping, it's simpler to store the four selectors
;; in the symbol table using the list representation of the token. 
;; 

;;
;; Another point, we are using the "rectangular" representation 
;; to handle addition and subtraction, and the "polar" representation
;; to handle addition and multiplication. Let's verify this:
;;
(map type-tag (list 
	       (add-complex z1 z2)
	       (sub-complex z1 z2)
	       (mul-complex z1 z2)
	       (div-complex z1 z2)))
;; ==> (rectangular rectangular polar polar)

;;
;; Let's also run the unit test suggested in the text:
;;
(define (checker1 z)
  (let ((test (make-from-real-imag (real-part z) (imag-part z1))))
    (and (= (real-part z) (real-part test))
	 (= (imag-part z) (imag-part test)))))

(checker1 z1)
;; ==> #t

;;
;; Let's run the same unit test for the "mag-ang" representation:
;;
(define (checker2 z)
  (let ((test (make-from-mag-ang (magnitude z) (angle z))))
    (and (= (magnitude z) (magnitude test))
	 (= (angle z) (angle test)))))

(checker2 z2)
;; ==> #t

;;
;; Run some additional unit tests:
;;
(define c1 (make-from-mag-ang 1 0))
;; ==> (polar 1 . 0)

(real-part c1)
;; ==> 1
(imag-part c2)
;; ==> 0

(define pi 3.14159)
(define c2 (make-from-mag-ang 1 (/ pi 2)))
;; ==> (polar 1 . 1.570795)

(real-part c2)
;; ==> 0
(imag-part c2)
;; ==> 1

(add-complex c1 c2)
;; ==> (rectangular 1 1)

(define c3 (make-from-real-imag 2 1))
;; => (rectangular 2 1)

(magnitude c3)
;; ==> 2.236068
(angle c3)
;; ==> 0.463648