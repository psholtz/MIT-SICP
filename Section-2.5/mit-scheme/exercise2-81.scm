;;
;; Exercise 2.81
;;
;; [WORKING]
;;

;;
;; First, let's define a "coercion" table:
;;
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

;;
;; Let's also define the coercion procedure defined in the text 
;; and install it in the table:
;;
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

;;
;; And let's import the definition of "apply-generic" from the text:
;;
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;;
;; Finally, let's define some numbers and see if we can get them to coerce:
;;
(define arg1 (make-scheme-number 1))
;; ==> 1
(define arg2 (make-complex-from-real-imag 2 3))
;; ==> (complex . rectangular 2 . 3)

;;
;; Let's see if it works:
;;
(add arg1 arg2)
;; ==> (complex . rectangular 3 . 3)
(add arg2 arg1)
;; ==> (complex . rectangular 3 . 3)

;;
;; It works, and it works both ways(!)
;;
;; Let's try some other operations:
;;
(sub arg1 arg2)
;; ==> (complex rectangular -1 . -3)
(sub arg2 arg1)
;; ==> (complex rectangular 1 . 3)

(real-part (mul arg1 arg2))
;; ==> 2 
(imag-part (mul arg1 arg2))
;; ==> 3

(real-part (mul arg2 arg1))
;; ==> 2
(imag-part (mul arg2 arg1))
;; ==> 3

(real-part (div arg2 arg1))
;; ==> 2
(imag-part (div arg2 arg1))
;; ==> 3

;;
;; So thus far, everything appears to work just fine, without making Louis' changes.
;;
;; Let's see how these procedures work when combining arguments of the same type:
;;
(add arg1 arg1)
;; ==> 2
(sub arg1 arg1)
;; ==> 0
(mul arg1 arg1)
;; ==> 1
(div arg1 arg1)
;; ==> 1

(add arg2 arg2)
;; ==> (complex rectangular 4 . 6)
(sub arg2 arg2)
;; ==> (complex rectangular 0 . 0)

;;
;; So here too, everything seems to work just fine as well.
;;
;; In that sense, perhaps we can address part (b) first:
;;

;;
;; (b) Is Louis correct that something had to be done about coercion with arguments 
;; of the same type, or does "apply-generic" work correctly as is?
;;

;;
;; The answer is no, Louis is not correct. 
;;
;; Nothing has to be done with coercion of arguments of the same type, let's see why:
;;
;; Consider evaluation of the "(add arg1 arg1)" expression above:
;;
(add arg1 arg1)
(add 1 1)
(apply-generic 'add 1 1)
;; type-tags <== (map type-tag '(1 1))
;; type-tags <== '(scheme-number scheme-number)
;; proc <== (get 'add type-tags)
;; proc <== (get 'add '(scheme-number scheme-number))
;; proc <== (lambda (x y) (tag (+ x y)))
(apply proc (map contents '(1 1)))
;; (map contents '(1 1)) <== '(1 1)
(apply (lambda (x y) (tag (+ x y))) '(1 1))
(apply (lambda (x y) (attach-tag 'scheme-number (+ x y))) '(1 1))
(attach-tag 'scheme-number (+ 1 1))
(attach-tag 'scheme-number 2)
2

;; 
;; Similarly, let's consider the evaluation of the "(add arg2 arg2)" expression above:
;;
(add arg2 arg2)
(add '(complex rectangular 2 . 3) '(complex rectangular 2 . 3))
(apply-generic 'add '(complex rectangular 2 . 3) '(complex rectangular 2 . 3))
;; type-tags <== (map type-tag '(complex rectangular 2 . 3) '(complex rectangular 2 . 3))
;; type-tags <== '(complex complex)
;; proc <== (get 'add '(complex complex))
;; proc <== (lambda (z1 z2) (tag (add-complex z1 z2)))
(apply proc (map contents '(complex rectangular 2 . 3) '(complex rectangular 2 . 3)))
;; (map contents '(complex rectangular 2 . 3) '(complex rectangular 2 . 3)) <== '((rectangular 2 . 3) (rectangular 2 . 3))
(apply (lambda (z1 z2) (tag (add-complex z1 z2))) '((rectangular 2 . 3) (rectangular 2 . 3)))
(apply 
 (lambda (z1 z2)
   (attach-tag 'complex ((lambda (x y)
			   (attach-tag 'rectangular (cons (+ (real-part x) (real-part y))
							  (+ (imag-part x) (imag-part y)))))
			 z1 z2)))
 '((rectangular 2 . 3) (rectangular 2 . 3)))
'(complex rectangular 4 . 6)

;;
;; So again, the procedure works fine as is.
;;
;; The reason is that the procedures for handling two arguments of the same type are already 
;; added to the operations table.
;;
