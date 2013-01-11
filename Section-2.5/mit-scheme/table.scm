;;
;; Code for handling a two-dimensional table (from Chapter 3)
;;
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

;; Operations Table
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; Coercion Table
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

;; Type-Raising/Coercion Table
(define (raise-integer->rational n)
  (make-rational n 1))
(define (raise-rational->scheme-number r)
  (let ((n (car r))
	(d (cdr r)))
    (make-scheme-number (exact->inexact (/ n d)))))
(define (raise-scheme-number->complex n)
  (make-complex-from-real-imag n 0))

(put 'raise '(integer) raise-integer->rational)
(put 'raise '(rational) raise-rational->scheme-number)
(put 'raise '(scheme-number) raise-scheme-number->complex)