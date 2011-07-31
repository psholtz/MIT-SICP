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
;; ==> Guess number 1: 1.
;; ==> Guess number 2: 2.
;; ==> Guess number 3: 1.5
;; ==> Guess number 4: 1.6666666666666665
;; ==> Guess number 5: 1.6
;; ==> Guess number 6: 1.625
;; ==> Guess number 7: 1.6153846153846154
;; ==> Guess number 8: 1.619047619047619
;; ==> Guess number 9: 1.6176470588235294
;; ==> Guess number 10: 1.6181818181818182
;; ==> Guess number 11: 1.6179775280898876
;; ==> Guess number 12: 1.6180555555555556
;; ==> Guess number 13: 1.6180257510729614
;; ==> Guess number 14: 1.6180371352785146
;; ==> phi 

;;
;; Use it to calculate x as given in the text:
;;
(define x (fixed-point (lambda (y) (/ (log 1000) (log y))) 2.0))
;; ==> Guess number 1: 2.
;; ==> Guess number 2: 9.965784284662087
;; ==> Guess number 3: 3.004472209841214
;; ==> Guess number 4: 6.279195757507157
;; ==> Guess number 5: 3.759850702401539
;; ==> Guess number 6: 5.215843784925895
;; ==> Guess number 7: 4.182207192401397
;; ==> Guess number 8: 4.8277650983445906
;; ==> Guess number 9: 4.387593384662677
;; ==> Guess number 10: 4.671250085763899
;; ==> Guess number 11: 4.481403616895052
;; ==> Guess number 12: 4.6053657460929
;; ==> Guess number 13: 4.5230849678718865
;; ==> Guess number 14: 4.577114682047341
;; ==> Guess number 15: 4.541382480151454
;; ==> Guess number 16: 4.564903245230833
;; ==> Guess number 17: 4.549372679303342
;; ==> Guess number 18: 4.559606491913287
;; ==> Guess number 19: 4.552853875788271
;; ==> Guess number 20: 4.557305529748263
;; ==> Guess number 21: 4.554369064436181
;; ==> Guess number 22: 4.556305311532999
;; ==> Guess number 23: 4.555028263573554
;; ==> Guess number 24: 4.555870396702851
;; ==> Guess number 25: 4.555315001192079
;; ==> Guess number 26: 4.5556812635433275
;; ==> Guess number 27: 4.555439715736846
;; ==> Guess number 28: 4.555599009998291
;; ==> Guess number 29: 4.555493957531389
;; ==> Guess number 30: 4.555563237292884
;; ==> Guess number 31: 4.555517548417651
;; ==> Guess number 32: 4.555547679306398
;; ==> Guess number 33: 4.555527808516254
;; ==> Guess number 34: 4.555540912917957
;; ==> x

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
;; ==> Guess number 1: 2.
;; ==> Guess number 2: 5.9828921423310435
;; ==> Guess number 3: 4.922168721308343
;; ==> Guess number 4: 4.628224318195455
;; ==> Guess number 5: 4.568346513136242
;; ==> Guess number 6: 4.5577305909237005
;; ==> Guess number 7: 4.555909809045131
;; ==> Guess number 8: 4.555599411610624
;; ==> Guess number 9: 4.5555465521473675
;; ==> x

;;
;; With average damping it only requires 9 iterations to arrive at the same answer.
;; 