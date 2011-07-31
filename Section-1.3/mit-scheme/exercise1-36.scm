;;
;; Exercise 1.36
;;
;; Modify "fixed-point" so that it prints the sequence of approximations it generates, using the "newline"
;; and "display" primitives shown in exercise 1.22. Then find a solution to x^x = 1000 by finding a fixed 
;; point of x |--> log(1000)/log(x). (Use Scheme's primitive "log" procedure, which computes natural logarithms).
;; Compare the number of steps this takes with and without average damping. (Note that you cannot start 
;; "fixed-point" with a guess of 1, as this would cause division by log(1)=0).
;;

;; 
;; Define the modified "fixed-point" procedure:
;;
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess number)
    (let ((next (f guess)))
      (newline)
      (display "Guess number ")
      (display number)
      (display ": ")
      (display guess)
      (if (close-enough? guess next)
	  next
	  (try next (+ number 1)))))
  (try first-guess 1))

;;
;; Use it to calculate phi:
;;
(define phi (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))
;; ==> 1.
;; ==> 2.
;; ==> 1.5
;; ==> 1.6666666666666665
;; ==> 1.6
;; ==> 1.625
;; ==> 1.6153846153846154
;; ==> 1.619047619047619
;; ==> 1.6176470588235294
;; ==> 1.6181818181818182
;; ==> 1.6179775280898876
;; ==> 1.6180555555555556
;; ==> 1.6180257510729614
;; ==> 1.6180371352785146
;; ==> phi

;;
;; Use it to calculate x as given in the text:
;;
(define x (fixed-point (lambda (y) (/ (log 1000) (log y))) 2.0))
;; ==> 2.
;; ==> 9.965784284662087
;; ==> 3.004472209841214
;; ==> 6.279195757507157
;; ==> 3.759850702401539
;; ==> 5.215843784925895
;; ==> 4.182207192401397
;; ==> 4.8277650983445906
;; ==> 4.387593384662677
;; ==> 4.671250085763899
;; ==> 4.481403616895052
;; ==> 4.6053657460929
;; ==> 4.5230849678718865
;; ==> 4.577114682047341
;; ==> 4.541382480151454
;; ==> 4.564903245230833
;; ==> 4.549372679303342
;; ==> 4.559606491913287
;; ==> 4.552853875788271
;; ==> 4.557305529748263
;; ==> 4.554369064436181
;; ==> 4.556305311532999
;; ==> 4.555028263573554
;; ==> 4.555870396702851
;; ==> 4.555315001192079
;; ==> 4.5556812635433275
;; ==> 4.555439715736846
;; ==> 4.555599009998291
;; ==> 4.555493957531389
;; ==> 4.555563237292884
;; ==> 4.555517548417651
;; ==> 4.555547679306398
;; ==> 4.555527808516254
;; ==> 4.555540912917957

;;
;; It can be shown that 4.55554 ^ 4.55554 = 1000.0, so it looks like we have a 
;; good approximation to the correct answer.
;;

;;
;; This implementation did not use average damping.
;;
;; Without avereage damping, it requires 34 iterations to arrive at an answer.
;;

;;
;; Let's modify our definition of x so that it uses average damping:
;;
(define (average x y) (/ (+ x y) 2.0))

(define x (fixed-point (lambda (y) (average y (/ (log 1000) (log y)))) 2.0))
;; ==> 2.
;; ==> 5.9828921423310435
;; ==> 4.922168721308343
;; ==> 4.628224318195455
;; ==> 4.568346513136242
;; ==> 4.5577305909237005
;; ==> 4.555909809045131
;; ==> 4.555599411610624
;; ==> 4.5555465521473675

;;
;; With average damping it only requires 9 iterations to arrive at the same answer.
;; 