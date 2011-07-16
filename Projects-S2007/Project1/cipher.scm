(load "rsa.scm")

;; 
;; Problem 1
;;

(modulo 13 8)
;; 5

(remainder 13 8)
;; 5

(modulo -13 8)
;; 3

(remainder -13 8)
;; -5

(modulo -13 -8)
;; -5

(remainder -13 -8)
;; -5

;;
;; TODO: ANSWER PART 1
;;

;; 
;; The modulo procedure is the best choice for implementing modular arithmetic.
;;

(define +mod
  (lambda (a b n)
    (modulo (+ a b) n)))

(define -mod
  (lambda (a b n)
    (modulo (- a b) n)))

(define *mod
  (lambda (a b n)
    (modulo (* a b) n)))

;; 
;; Problem 2
;;
(define exptmod
  (lambda (a b n)
    (cond ((= b 0) 1)
	  ((even? b)
	   (modulo (square (exptmod a (/ b 2) n))
		   n))
	  (else
	   (modulo (* a (exptmod a (- b 1) n))
		   n)))))


;;
;; Problem 3
;;
(define (expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (expt b (/ n 2))))
	(else
	 (* b (expt b (- n 1))))))

(define random-k-digit-number
  (lambda (k)
    (if (= k 1)
	(random 10)
	(let ((next-k (- k 1)))
	  (+ (* (expt 10 next-k) (random 10)) (random-k-digit-number (- k 1)))))))

(define count-digits
  (lambda (n)
    (if (< (/ n 10) 1)
	1
	(+ 1 (count-digits (/ n 10))))))

(define big-random
  (lambda (n)
    (let ((sample (random-k-digit-number (count-digits n))))
      (if (< sample n)
	  sample
	  (big-random n)))))

;;
;; Problem 4
;;
(define prime-test-iterations 20)

(define prime?
  (lambda (p)
    (fast-prime? p prime-test-iterations)))

(define fast-prime?
  (lambda (n times)
    (cond ((= times 0) true)
	  ((fermat-test n) (fast-prime? n (- times 1)))
	  (else false))))

(define fermat-test
  (lambda (n)
    (define try-it
      (lambda (a)
	(= (exptmod a n n) a)))
    (cond ((= n 0) #f)
	  ((= n 1) #f)
	  (else
	   (try-it (big-random n))))))

;;
;; Problem 5
;;
(define random-prime
  (lambda (n)
    (let ((p (big-random n))) ;; check the bounds again
      (if (prime? p)
	  p
	  (random-prime n)))))

;; 
;; Problem 6
;;
(define ax+by=1
  (lambda (a b)
    (let ((q (quotient a b))
	  (r (remainder a b)))
      (if (= r 1)
	  (list 1 (* -1 q))
	  (let ((answer (ax+by=1 b r)))
	    (list (cadr answer) (- (car answer) (* q (cadr answer)))))))))

(define inverse-mod
  (lambda (e n)
    (if (not (= (gcd e n) 1))
	'error
	(let ((answer (ax+by=1 e n)))
	  (let ((inv (car answer)))
	    (if (< inv 0)
		(+ inv n)
		inv))))))

;;
;; Problem 7
;;
(define make-key
  (lambda (e n)
    (list e n)))

(define get-modulus
  (lambda (k)
    (if (pair? k)
	(cadr k)
	'())))

(define get-exponent
  (lambda (k)
    (if (pair? k)
	(car k)
	'())))

;;
;; returns a list of two keys:
;; (a) public key will be first;
;; (b) private key will be next;
;; 
(define random-keypair
  (lambda (m)
    (define find-random-keypair
      (lambda ()
	(let ((p (random-prime m))
	      (q (random-prime m)))
	  (let ((n (* p q))
		(k (* (- p 1) (- q 1))))
	    (let ((e (big-random n)))
	      (if (and (> n m) (= (gcd e k) 1))
		  (let ((d (inverse-mod e k)))
		    (list n e d))
		  (find-random-keypair)))))))

    (let ((answer (find-random-keypair)))
      (let ((n (car answer))
	    (e (cadr answer))
	    (d (caddr answer)))
	(list
	 (make-key e n)
	 (make-key d n))))))

(define rsa
  (lambda (key message)
    (exptmod message (get-exponent key) (get-modulus key))))


(define encrypt
  (lambda (public-key string)
    (rsa public-key (string->integer string))))

(define decrypt
  (lambda (private-key encrypted-message)
    (integer->string (rsa private-key encrypted-message))))