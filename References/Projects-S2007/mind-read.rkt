;; always-true, always-false, guess-last, guess-not-last,
;; alternating-true-false are five simple functions that map a list
;; of #t/#f elements to #t or #f

(define always-true (lambda (x) #t))

(define always-false (lambda (x) #f))

(define guess-last (lambda (x) (if (null? x) #t (car x))))

(define guess-not-last (lambda (x) (if (null? x) #t (not (car x)))))

(define alternating-true-false (lambda (x) (even? (length x)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (myread) prompts the player for input. The player can enter
;; t, f, or e: (myread) respectively returns #t, #f or 'e in these
;; cases.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (myread)
   (display "ENTER t, f, or e to end the game") 
   (newline)
   (let ((x (read)))
     (cond ((eq? x 'e) x)
           ((eq? x 't) #t)
           ((eq? x 'f) #f)
           (else (display "Please enter t, f, or e!!")
                 (newline)
                 (myread)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (simple-game func) implements a very simple version of a
;; mind-reading game. The game is played in a series of rounds:
;; at each round, the player is prompted for an input, i.e.
;; t, f, or e. If the player types e the game ends. The code
;; keeps track of the players previous inputs through the "ctxt"
;; variable in the simple-game-h function. At each point it tries
;; to predict the player's next input by applying "func" to the
;; "ctxt" variable. The code keeps track of the number of errors
;; made by the program, and the total number of rounds, through
;; the "numerrors" and "total" variables respectively.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simple-game func) (simple-game-h '() func 0 0))

(define (simple-game-h ctxt func numerrors total)
  (display "ROUNDS: ") (display total) (newline) 
  (display "YOUR SCORE: ") (display numerrors) (newline)
  (display "MY SCORE: ") (display (- total numerrors)) (newline)
  (display "HISTORY: ") (display ctxt) (newline)
  (let ((in (myread)))
    (if (eq? in 'e)
        (begin (display "FINAL NUMBER OF ROUNDS: ") (display total) (newline) 
                   (display "YOUR FINAL SCORE: ") (display numerrors) (newline) 
                   (display "MY FINAL SCORE: ") (display (- total numerrors)) (newline)                    
                   numerrors)
        (let ((p (func ctxt)))
              (display "You said: ") (display in) 
              (display ", I predicted: ") (display p) 
              (if (eq? p in)
                     (begin (display " I win! :-o") (newline)
                            (simple-game-h (cons in ctxt)                                   
                                           func
                                           numerrors (+ total 1)))
                     (begin (display " I lose :-(") (newline)
                            (simple-game-h (cons in ctxt)
                                           func
                                           (+ numerrors 1) (+ total 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (batch-simple-game sequence func) implements a non-interactive version
;; of (simple-game func). Instead of prompting the player for input,
;; the list "sequence" defines a list of inputs. For example, 
;;
;; (batch-simple-game '(#t #f #t #f) func)
;;
;; is equivalent to playing (simple-game func) and entering the sequence
;; "t" "f" "t" "f" "e" to the prompts
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (batch-simple-game sequence f) (batch-simple-game-h '() sequence f 0))

(define (batch-simple-game-h ctxt remaining f numerrors)
        (if (null? remaining)
            numerrors
            (let ((p (f ctxt)))
              (if (eq? p (car remaining))
                     (batch-simple-game-h (cons (car remaining) ctxt)
					  (cdr remaining)
					  f
					  numerrors)
                     (batch-simple-game-h (cons (car remaining) ctxt)
					  (cdr remaining)
					  f
					  (+ numerrors 1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (nones n) returns a list of n 1's: e.g. (nones 5) returns (1 1 1 1 1)
;; This is used to set the initial weights in the learning
;; algorithms
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define nones (lambda (n) (if (= n 0) '() (cons 1 (nones (- n 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (learning-game funcs) implements a learning version of the mind-reading
;; game. funcs is a list of functions, for example we could enter
;;
;; (learning-game (list always-true always-false guess-last guess-not-last))
;; 
;; learning-game makes use of a helper procedure, learning-game-h.
;; As in simple-game, the argument "ctxt" stores a list of previous
;; inputs from the user; "numerrors" keeps track of the total number
;; of errors made by the mind-reader; "total" keeps track of the
;; total number of rounds. An additional argument, "weights", is a list
;; of numbers, storing one weight for each of the functions in "funcs".
;;
;; At each round, the function makes a prediction by the call to
;;
;; (prediction funcs weight ctxt)
;;
;; this calculates a weighted vote of the funcs applied to ctxt.
;;
;; The weights are then updated for the next round in the call to
;;
;; (update-weights funcs weights ctxt in)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (learning-game funcs)
  (learning-game-h '() funcs (nones (length funcs)) 0 0))

(define (learning-game-h ctxt funcs weights numerrors total)
  (display "ROUNDS: ") (display total) (newline) 
  (display "YOUR SCORE: ") (display numerrors) (newline)
  (display "MY SCORE: ") (display (- total numerrors)) (newline)
  (display "HISTORY: ") (display ctxt) (newline)
  (let ((in (myread)))
    (if (eq? in 'e)
        (begin (display "FINAL NUMBER OF ROUNDS: ") (display total) (newline) 
                   (display "YOUR FINAL SCORE: ") (display numerrors) (newline) 
                   (display "MY FINAL SCORE: ") (display (- total numerrors)) (newline)                    
                   numerrors)
        (let ((p (prediction funcs weights ctxt))
                  (newweights (update-weights funcs weights ctxt in)))
              (display "You said: ") (display in) 
              (display ", I predicted: ") (display p) 
              (if (eq? p in)
                     (begin (display " I win!!") (newline)
                            (learning-game-h (cons in ctxt)                                   
                                           funcs
                                           newweights
                                           numerrors (+ total 1)))
                     (begin (display " I lose :-(") (newline)
                            (learning-game-h (cons in ctxt)
                                           funcs
                                           newweights
                                           (+ numerrors 1) (+ total 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (batch-learning-game sequence func) implements a non-interactive version
;; of (learning-game func). Instead of prompting the player for input,
;; the list "sequence" defines a list of inputs. For example, 
;;
;; (batch-learning-game '(#t #f #t #f) func)
;;
;; is equivalent to playing (learning-game func) and entering the sequence
;; "t" "f" "t" "f" "e" to the prompts
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (batch-learning-game sequence funcs)
  (batch-learning-game-h '() sequence funcs (nones (length funcs)) 0 0))

(define (batch-learning-game-h ctxt remaining funcs weights numerrors total)
    (if (null? remaining)
            (begin (display "FINAL NUMBER OF ROUNDS: ") 
                   (display total) (newline) 
                   (display "YOUR FINAL SCORE: ") 
                   (display numerrors) (newline) 
                   (display "MY FINAL SCORE: ") 
                   (display (- total numerrors)) (newline)                    
                   numerrors)
            (let ((p (prediction funcs weights ctxt))
                  (newweights
		   (update-weights funcs weights ctxt (car remaining))))
              (if (eq? p (car remaining))
                    (batch-learning-game-h (cons (car remaining) ctxt)
					   (cdr remaining)
					   funcs
					   newweights
					   numerrors (+ total 1))
                    (batch-learning-game-h (cons (car remaining) ctxt)
					   (cdr remaining)
					   funcs
					   newweights
					   (+ numerrors 1) (+ total 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (bad-random-choice f1 f2 p) is Ben's (incorrect) attempt at a function
;; that implements the behavior described in problem 4.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bad-random-choice f1 f2 p)
   (let ((q (random-fraction)))
      (if (<= p q)
          (lambda (x) (f1 x))
          (lambda (x) (f2 x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; random-fraction is a function that returns a random number between 0 and 1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define random-fraction
   (lambda () (/ (random 10000) 10000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; allfuncs is the list of functions in problem 8
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define allfuncs
;  (list always-true always-false guess-last guess-not-last 
;        (skip-most-recent always-true) (skip-most-recent always-false)
;        (skip-most-recent guess-last) (skip-most-recent guess-not-last)
;        (skip-most-recent (skip-most-recent always-true)) 
;        (skip-most-recent (skip-most-recent always-false))
;        (skip-most-recent (skip-most-recent guess-last)) 
;        (skip-most-recent (skip-most-recent guess-not-last)) 
;        (lastn 3) (lastn 5) (lastn 7) 
;        (negation (lastn 3)) (negation (lastn 5)) (negation (lastn 7))))
