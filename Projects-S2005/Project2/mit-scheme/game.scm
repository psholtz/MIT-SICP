(load "prisoner.scm")

;; ++++++++++++++++++++++++++++++++++++++++++++++++++ 
;; Problem 1
;; 
;; Definition of "extract-entry"
;; ++++++++++++++++++++++++++++++++++++++++++++++++++ 

;;
;; The *game-association-list* is defined as follows:
;;
(define *game-association-list*
  (list (list (list "c" "c") (list 3 3))
	(list (list "c" "d") (list 0 5))
	(list (list "d" "c") (list 5 0))
	(list (list "d" "d") (list 1 1))))

;;
;; We can extract a specific entry in this list by using the "list-ref" procedure. 
;;
;; For example:
;;
(list-ref *game-association-list* 0)
;; ==> (("c" "c") (3 3))
(list-ref *game-association-list* 1)
;; ==> (("c" "d") (0 5))

;;
;; and so on. To extract the entry associated with a specific play, we need to extract 
;; the "car" of the entry, and make sure that both elements of this "car" correspond 
;; to both elements of the argument play. 
;;
;; We define our "extract-entry" procedure as follows:
;;
(define (extract-entry play *game-association-list*)
  ;; 
  ;; Returns "true" if the play matches the entry:
  ;;
  (define (compare play entry)
    (let ((test (car entry)))
      (and (string=? (car play) (car test))
	   (string=? (cadr play) (cadr test)))))

  (let
      ;; 
      ;; Get references to each entry in the *game-association-list*:
      ;;
      ((first (list-ref *game-association-list* 0))
       (second (list-ref *game-association-list* 1))
       (third (list-ref *game-association-list* 2))
       (fourth (list-ref *game-association-list* 3)))
   
    ;; 
    ;; If we find a match, return that specific entry:
    ;;
    (cond 
     ((compare play first) first)
     ((compare play second) second)
     ((compare play third) third)
     ((compare play fourth) fourth)
     (else '()))))

;;
;; We can test our procedure as follows:
;;
(extract-entry (make-play "c" "c") *game-association-list*)
;; ==> (("c" "c") (3 3))
(extract-entry (make-play "c" "d") *game-association-list*)
;; ==> (("c" "d") (0 5))
(extract-entry (make-play "d" "c") *game-association-list*)
;; ==> (("d" "c") (5 0))
(extract-entry (make-play "d" "d") *game-association-list*)
;; ==> (("d" "d") (1 1))
(extract-entry (make-play "x" "x") *game-association-list*)
;; ==> ()

;;
;; Similarly, since "get-point-list" is defined as:
;;
(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

(get-point-list (make-play "c" "c"))
;; ==> (3 3)
(get-point-list (make-play "c" "d"))
;; ==> (0 5)
(get-point-list (make-play "d" "c"))
;; ==> (5 0)
(get-point-list (make-play "d" "d"))
;; ==> (1 1)

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
;; Problem 2
;;
;; Use "play-loop" to play games between the five strategies.
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

;;
;; For reference, the five strategies are defined as:
;;

;; Always "defect"
(define (NASTY my-history other-history)
  "d")

;; Always "cooperate"
(define (PATSY my-history other-history)
  "c")

;; "Defect" or "cooperate" with 50-50 chance
(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (EGALITARIAN my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else
	    (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
	(cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

;; "Cooperate" on first round, otherwise return "eye for eye"
(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))
       
;;
;; NASTY is a highly "dominant" strategy. It never "loses" outright, at worst tying only 
;; when it plays against itself. Otherwise, NASTY is able to beat all the other strategies.
;;
;; When NASTY plays against the following opponents, we obtain the following results:
;;

;;
;;           -------------------------------------------------------------------------
;;           |    NASTY   |    PATSY   |   SPASTIC   |  EGALITARIAN  |  EYE-FOR-EYE  |
;;------------------------------------------------------------------------------------
;;   NASTY   |    Ties    |    Wins    |    Wins     |     Wins      |     Wins      | 
;;           | 1.0 points | 5.0 points | 2.88 points |  1.04 points  |  1.04 points  | 
;;------------------------------------------------------------------------------------
;;

;; 
;; Note that in all these plays, SPASTIC is a stochastic strategy and will generate 
;; slightly different point values which vary from round to round.
;;

;;
;; PATSY never wins, and it loses badly against NASTY and SPASTIC. However, it ties with
;; itself, EGALITARIAN and EYE-FOR-EYE.
;;
;; When PATSY plays against the following opponents, we obtain the following results:
;;

;;
;;           ----------------------------------------------------------------------------
;;           |    NASTY    |    PATSY   |    SPASTIC   |  EGALITARIAN  |   EYE-FOR-EYE  |
;;---------------------------------------------------------------------------------------
;;   PATSY   |    Loses    |    Ties    |    Loses     |     Ties      |      Ties      |
;;           |  0.0 points | 3.0 points |  1.29 points |  3.0 points   |   3.0 points   |
;;---------------------------------------------------------------------------------------
;;

;;
;; Despite being ostensibly "random", the SPASTIC strategy fares quite well. When playing
;; against itself, the results are (essentially) a draw, where it wins or loses by a slight
;; random margin. Similarly, the results against EYE-FOR-EYE are usually neutral (i.e., a tie)
;; with an occasional very slighty win on the side of SPASTIC. SPASTIC wins decisively against
;; PATSY, and loses against NASTY.
;;
;; The most interesting behavior is when SPASTIC plays EGALITARIAN. Usually (roughly 70% of 
;; the time), SPASTIC will win decisively, but occassionally (roughly 30% of the time) it 
;; will lose (quite badly).
;; 
;; Note that SPASTIC is a stochastic strategy and will generate slightly different point 
;; values which vary from round to round. Specifically, when playing against itself, 
;; SPASTIC will either win or lose, but only by a very narrow margin, both opponents 
;; obtaining nearly the same number of points. This state of affairs is labeled a
;; "stochastic tie".
;;
;; When SPASTIC plays against the following opponents, we obtain the following results:
;;

;;
;;           ---------------------------------------------------------------------------------------------
;;           |     NASTY     |     PATSY    |       SPASTIC      |   EGALITARIAN   |     EYE-FOR-EYE     |
;;--------------------------------------------------------------------------------------------------------
;;  SPASTIC  |     Loses     |     Wins     |  "Stochastic Tie"  |    70% Wins     |  Tie or Slight Win  |
;;           |  0.527 points |  4.1 points  |     2.32 points    |   2.77 points   |     2.29 points     |
;;           |               |              |                    |    30% Loss     |                     |
;;           |               |              |                    |  1.22 points    |                     |
;;--------------------------------------------------------------------------------------------------------
;;

;;
;; EGALITARIAN almost always "ties", or, even when "winning" or "losing", does so only 
;; by a very narrow margin. The most interesting behavior comes with SPASTIC, where 
;; 50% of the time, EGALITARIAN wins with a score of 2.47, whereas 50% of the time, 
;; EGALITARIAN loses with a score of 1.87. 
;;
;; When EGALITARIAN plays against the following opponents, we obtain the following results:
;;

;;
;;              ------------------------------------------------------------------------------- 
;;              |     NASTY     |    PATSY     |   SPASTIC     |  EGALITARIAN |  EYE-FOR-EYE  |
;;--------------------------------------------------------------------------------------------- 
;;  EGALITARIAN |  Slight Loss  |     Tie      |   50% Wins    |     Tie      |      Tie      |
;;              |  0.99 points  |  3.0 points  |  2.47 points  |  3.0 points  |   3.0 points  |
;;              |               |              |   50% Loses   |              |               |
;;              |               |              |  1.87 points  |              |               |
;;--------------------------------------------------------------------------------------------- 
;;    

;;
;; Like EGALITARIAN, EYE-FOR-EYE also almost always "ties", or, even when "winning" or "losing",
;; does so only by a very narrow margin. EYE-FOR-EYE will tie PATSY, EGALITARIAN and EYE-FOR-EYE
;; (itself). It will tie or slightly lose to SPASTIC, and will slightly lose to NASTY.
;;

;;
;;              ------------------------------------------------------------------------------------ 
;;              |     NASTY     |   PATSY    |       SPASTIC       |  EGALITARIAN  |  EYE-FOR-EYE  |
;;--------------------------------------------------------------------------------------------------
;;  EYE-FOR-EYE |  Slight Loss  |    Tie     |  Tie or Slight Loss |      Tie      |      Tie      |
;;              |  0.99 points  | 3.0 points |    2.23 points      |   3.0 points  |  3.0 points   |
;;-------------------------------------------------------------------------------------------------- 
;;

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
;; Problem 3
;;
;; Explore more efficient ways to code EGALITARIAN.
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;
;; For reference, the original definition of EGALITARIAN was given as:
;;
(define (EGALITARIAN my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else
	   (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
	(cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

;;
;; For any one particular game play, the code here makes:
;;
;;  (1) a "linear" walk down the "other-history" list;
;;  (2) a second "linear" walk down the "other-history" list; 
;;
;; So for each game play, if the "other-history" is of size k, 
;; the procedure executes in O(2*k) time.
;; 
;; In other words, when the history is of length 1, the play 
;; executes in time 2*1. When the history is of length 2, the 
;; play executes in time 2*2. When the history is of length k, 
;; the play executes in time 2*k. 
;;
;; We are executing a total of n plays. That means the total 
;; time to execute all n plays is:
;;
;; T(n) = 2 * ( 1 + 2 + ... + n )
;; T(n) = 2 * n * ( n + 1 ) / 2
;; T(n) = n * (n+1)
;;
;; In O-notation, this procedure will execute in O(n^2) time.
;;

;;
;; Alyssa's new definition of EGALITARIAN is given as:
;;
(define (EGALITARIAN my-history other-history)
  (define (majority-loop cs ds hist)
    (cond ((empty-history? hist) (if (> ds cs) "d" "c"))
	  ((string=? (most-recent-play hist) "c")
	   (majority-loop (+ 1 cs) ds (rest-of-plays hist)))
	  (else
	   (majority-loop cs (+ 1 ds) (rest-of-plays hist)))))
  (majority-loop 0 0 other-history))

;;
;; Using this procedure, for any one particular game play, we 
;; still make a "linear" walk down the length of the "other-history"
;; list, but for each game play, we only make one linear walk down 
;; this list, not two. 
;;
;;
;; Hence, for each game play, if the "history" is of size k, the 
;; procedure executes in O(k) time.
;;
;; In other words, we expect the procedure to execute roughly twice
;; as fast as the previous EGALITARIAN procedure.
;;
;; We are executing a total of n plays. That means the total 
;; time to execute all n plays is:
;;
;; T(n) = 1 + 2 + .. + n
;; T(n) = n * (n+1) / 2
;;
;; In O-notation, this procedure will execute in O(n^2) time.
;;
;; In other words, in O-notation, this procedure will executes in 
;; roughly the same order of magnitude time as the previous 
;; procedure: it scales as n^2, where n is the number of game plays.
;; However, there is some (considerable) savings in this procedure, 
;; owing to the fact that each game play executes in roughly 1/2
;; the time that it took using the first procedure.
;;

;;
;; As a test, let's implement a "timed-play-loop" procedure that
;; (a) runs more play sets; and (b) prints out timing statistics, 
;; so we can see whether program execution actually performs the 
;; way we would predict.
;;
(define (timed-play-loop strat0 strat1 times)

  ;;
  ;; Play-loop-iter procedure for executing the game play an arbitrary number of times
  ;;
  (define (timed-play-loop-iter count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
	  (else
	   (let ((result0 (strat0 history0 history1))
		 (result1 (strat1 history1 history0)))
	     (timed-play-loop-iter (+ count 1)
				   (extend-history result0 history0)
				   (extend-history result1 history1)
				   limit)))))

  ;;
  ;; Bracket execution loop for timing purposes
  ;;
  (let ((start (real-time-clock)))
    (timed-play-loop-iter 0 the-empty-history the-empty-history times)
    (let ((finish (real-time-clock)))
      (display "Timing: ")
      (display (- finish start)))))

;;
;; Let's build a matrix of the time it takes to execute 500 game plays 
;; (roughly 5 times more plays than in a normal round, using "play-loop"), 
;; and see which procedures are the slowest.
;;
;; The entries in the matrix correspond to the time (ticks) required for the 
;; 500 game plays to execute. The following matrix uses the "original"
;; definition of the EGALITARIAN procedure:
;;

;;
;;                 ----------------------------------------------------------------- 
;;                 |  NASTY  |  PATSY  |  SPASTIC  |  EGALITARIAN  |  EYE-FOR-EYE  |
;; --------------------------------------------------------------------------------- 
;; |     NASTY     |   15    |    13   |     40    |      415      |       15      |
;; --------------------------------------------------------------------------------- 
;; |     PATSY     |   13    |    10   |     11    |      407      |       10      |
;; --------------------------------------------------------------------------------- 
;; |    SPASTIC    |   40    |    11   |     14    |      439      |       13      |
;; ---------------------------------------------------------------------------------  
;; |  EGALITARIAN  |   415   |   407   |    439    |      801      |      408      |
;; --------------------------------------------------------------------------------- 
;; |  EYE-FOR-EYE  |   15    |    10   |     13    |      408      |       10      |
;; ---------------------------------------------------------------------------------
;; 

;;
;; Clear EGALITARIAN takes much longer to execute than do the other strategies, requiring
;; roughly 400 ticks to execute the 500 plays. Notice also that when playing against itself,
;; EGALITARIAN requires roughly 2 * 400 = 800 ticks to execute the 500 plays. 
;;

;;
;; We anticipate that the new definition of the EGALITARIAN procedure will run roughly 
;; twice as quickly. Using the new definition, we obtain the following performance matrix:
;;

;;
;;                 ----------------------------------------------------------------- 
;;                 |  NASTY  |  PATSY  |  SPASTIC  |  EGALITARIAN  |  EYE-FOR-EYE  |
;; --------------------------------------------------------------------------------- 
;; |     NASTY     |    15   |    13   |     14    |      250      |       17      |
;; --------------------------------------------------------------------------------- 
;; |     PATSY     |    13   |     9   |     12    |      243      |       10      |
;; --------------------------------------------------------------------------------- 
;; |    SPASTIC    |    14   |    12   |     13    |      253      |       14      |
;; --------------------------------------------------------------------------------- 
;; |  EGALITARIAN  |   250   |   243   |    253    |      476      |      241      |
;; --------------------------------------------------------------------------------- 
;; |  EYE-FOR-EYE  |    17   |    10   |     14    |      241      |       10      |
;; --------------------------------------------------------------------------------- 
;;

;;
;; As anticipated, the performance is still slow (i.e., O(n^2)), although the new 
;; procedure performs roughly twice as efficiently as the original procedure (as 
;; we anticipated).
;;

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Problem 4
;;
;; Write a new "eye-for-two-eyes" strategy.
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

;;
;; For reference, the original EYE-FOR-EYE strategy is defined as:
;;
(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))

;;
;; EYE-FOR-TWO-EYES will cooperate if either of the two most recent
;; plays of the opponent where cooperate.
;;
(define (EYE-FOR-TWO-EYES my-history other-history)
  (if (empty-history? other-history)
      "c"
      (let ((result1 (most-recent-play other-history)))
	(if (empty-history? (rest-of-plays other-history))
	    "c"
	    (let ((result2 (most-recent-play (rest-of-plays other-history))))
	      (if (or (string=? result1 "c") (string=? result2 "c"))
		  "c"
		  "d"))))))

;;
;; Let's run some unit tests, to make sure the procedure works the way we want.
;; For this game strategy, "our" history does not enter into the calculation. 
;; We can pad our strategy with a random number "x":
;;
(define temp-my-0 the-empty-history)
(define temp-my-1 (list "x"))
(define temp-my-2 (list "x" "x"))
(define temp-my-3 (list "x" "x" "x"))
(define temp-my-4 (list "x" "x" "x" "x"))

;;
;; Run the actual unit tests:
;;
(EYE-FOR-TWO-EYES temp-my-0 the-empty-history)
;; ==> "c"

(EYE-FOR-TWO-EYES temp-my-1 (list "c"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-1 (list "d"))
;; ==> "c"

(EYE-FOR-TWO-EYES temp-my-2 (list "c" "c"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-2 (list "c" "d"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-2 (list "d" "c"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-2 (list "d" "d"))
;; ==> "d"

(EYE-FOR-TWO-EYES temp-my-3 (list "c" "c" "c"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-3 (list "c" "c" "d"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-3 (list "c" "d" "c"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-3 (list "c" "d" "d"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-3 (list "d" "c" "c"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-3 (list "d" "c" "d"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-3 (list "d" "d" "c"))
;; ==> "d" 
(EYE-FOR-TWO-EYES temp-my-3 (list "d" "d" "d"))
;; ==> "d"

(EYE-FOR-TWO-EYES temp-my-4 (list "c" "c" "c" "c"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-4 (list "c" "c" "c" "d"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-4 (list "c" "c" "d" "c"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-4 (list "c" "c" "d" "d"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-4 (list "c" "d" "c" "c"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-4 (list "c" "d" "c" "d"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-4 (list "c" "d" "d" "c"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-4 (list "c" "d" "d" "d"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-4 (list "d" "c" "c" "c"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-4 (list "d" "c" "c" "d"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-4 (list "d" "c" "d" "c"))
;; ==> "c"
(EYE-FOR-TWO-EYES temp-my-4 (list "d" "c" "d" "d"))
;; ==> "c" 
(EYE-FOR-TWO-EYES temp-my-4 (list "d" "d" "c" "c"))
;; ==> "d"
(EYE-FOR-TWO-EYES temp-my-4 (list "d" "d" "c" "d"))
;; ==> "d"
(EYE-FOR-TWO-EYES temp-my-4 (list "d" "d" "d" "c"))
;; ==> "d"
(EYE-FOR-TWO-EYES temp-my-4 (list "d" "d" "d" "d"))
;; ==> "d"

;;
;; Looks like the strategy works as advertised. 
;;
;; Let's run the strategy against the other strategies, and compile the results:
;;

;;
;; -------------------------------
;;       Loses against NASTY
;; -------------------------------
;;  NASTY:  1.09 points
;;  EYE-FOR-TWO-EYES: 0.98 points
;; -------------------------------
;;
;;
;; ------------------------------- 
;;       Ties against PATSY 
;; ------------------------------- 
;;  PATSY: 3.0 points
;;  EYE-FOR-TWO-EYES: 3.0 points 
;; ------------------------------- 
;;
;;
;; ------------------------------- 
;;      Loses against SPASTIC 
;; ------------------------------- 
;;  SPASTIC: 3.0 points
;;  EYE-FOR-TWO-EYES: 1.78 points 
;; ------------------------------- 
;;
;;
;; ------------------------------- 
;;    Ties against EGALITARIAN 
;; ------------------------------- 
;;  EGALITARIAN: 3.0 points
;;  EYE-FOR-TWO-EYES: 3.0 points
;; -------------------------------
;;
;; 
;; -------------------------------
;;    Ties against EYE-FOR-EYE
;; ------------------------------- 
;;  EYE-FOR-EYE: 3.0 points
;;  EYE-FOR-TWO-EYES: 3.0 points
;; ------------------------------- 
;;
;;
;; -------------------------------
;;  Ties against EYE-FOR-TWO-EYES
;; -------------------------------
;;  EYE-FOR-TWO-EYES: 3.0 points
;;  EYE-FOR-TWO-EYES: 3.0 points
;; ------------------------------- 
;;

;;
;; EYE-FOR-TWO-EYES is a very "neutral" strategy. Usually it ties, and it never wins. 
;; It loses slighlty against NASTY, and loses slightly worse against SPASTIC. In this
;; way it's very similar to its close-cousin EYE-FOR-EYE, which similarly loses (slightly) 
;; against NASTY, ties or loses (slightly) against SPASTIC, and ties all the other strategies.
;;

;;
;; Next let's run timing results for the EYE-FOR-TWO-EYES procedure. 
;;
;; For the EGALITARIAN procedure, we will use the second (faster) definition.
;;

;;                     ----------------------------------------------------------------------------------------
;;                     |  NASTY  |   PATSY   |  SPASTIC  |  EGALITARIAN  |  EYE-FOR-EYE  |  EYE-FOR-TWO-EYES  |
;; ------------------------------------------------------------------------------------------------------------
;;   EYE-FOR-TWO-EYES  |   19    |    14     |     14    |      256      |      11       |        12          |
;; ------------------------------------------------------------------------------------------------------------
;;

;;
;; In terms of perforamnce, EYE-FOR-TWO-EYES executes about as fast as all the other structures, with relatively
;; fast performance against most strategies, except for EGALITARIAN, where the performance is slightly slower.
;;

;; ++++++++++++++++++++++++++++++++++++++++++++++ 
;; Problem 5
;;
;; Write a procedure "make-eye-for-n-eyes".
;; ++++++++++++++++++++++++++++++++++++++++++++++ 

;;
;; This procedure should take a number as input and return the appropriate "eye-for-eye"-like strategy.
;; For example, (make-eye-for-n-eyes 2) should return a strategy equivalent to "eye-for-two-eyes". Use 
;; this procedure to create a new strategy and test it against the other strategies. Describe the 
;; observed behavior.
;;
(define (make-eye-for-n-eyes n)
  ;;
  ;; We need to return a two-argument procedure.
  ;;
  (lambda (my-history other-history)
    ;;
    ;; Extract current play, returning "c" if there are no more plays.
    ;;
    (define (current-play history)
      (if (empty-history? history)
	  "c"
	  (most-recent-play history)))

    ;;
    ;; Define iterative procedure for making "n" eyes.
    ;;
    (define (make-eye-for-n-eyes-iter k history)
      (cond ((= k 1) (current-play history))
	    (else
	     (if 
	      (or 
	       (string=? "c" (current-play history))
	       (string=? "c" (make-eye-for-n-eyes-iter (- k 1) (rest-of-plays history))))
	      "c"
	      "d"))))
    
    ;;
    ;; Invoke the iterative procedure.
    ;;
    (make-eye-for-n-eyes-iter n other-history)))

;;
;; Let's define strategies for 1, 2 and 3 eyes, and see if they perform the way we anticipate:
;;
(define one-eye (make-eye-for-n-eyes 1))
(define two-eye (make-eye-for-n-eyes 2))
(define three-eye (make-eye-for-n-eyes 3))

;;
;; Run the tests for "one-eye":
;;
(one-eye temp-my-1 (list "c"))
;; ==> "c"
(one-eye temp-my-1 (list "d"))
;; ==> "d"

(one-eye temp-my-2 (list "c" "c"))
;; ==> "c"
(one-eye temp-my-2 (list "c" "d"))
;; ==> "c"
(one-eye temp-my-2 (list "d" "c"))
;; ==> "d"
(one-eye temp-my-2 (list "d" "d"))
;; ==> "d"

(one-eye temp-my-3 (list "c" "c" "c"))
;; ==> "c"
(one-eye temp-my-3 (list "c" "c" "d"))
;; ==> "c"
(one-eye temp-my-3 (list "c" "d" "c"))
;; ==> "c"
(one-eye temp-my-3 (list "c" "d" "d"))
;; ==> "c"
(one-eye temp-my-3 (list "d" "c" "c"))
;; ==> "d"
(one-eye temp-my-3 (list "d" "c" "d"))
;; ==> "d"
(one-eye temp-my-3 (list "d" "d" "c"))
;; ==> "d"
(one-eye temp-my-3 (list "d" "d" "d"))
;; ==> "d"

;;
;; "one-eye" performs as we would expect "eye-for-eye" to perform.
;;

;;
;; Run the tests for "two-eye":
;;
(two-eye temp-my-1 (list "c"))
;; ==> "c"
(two-eye temp-my-1 (list "d"))
;; ==> "c"

(two-eye temp-my-2 (list "c" "c"))
;; ==> "c"
(two-eye temp-my-2 (list "c" "d"))
;; ==> "c"
(two-eye temp-my-2 (list "d" "c"))
;; ==> "c"
(two-eye temp-my-2 (list "d" "d"))
;; ==> "d"

(two-eye temp-my-3 (list "c" "c" "c"))
;; ==> "c"
(two-eye temp-my-3 (list "c" "c" "d"))
;; ==> "c"
(two-eye temp-my-3 (list "c" "d" "c"))
;; ==> "c" 
(two-eye temp-my-3 (list "c" "d" "d"))
;; ==> "c"
(two-eye temp-my-3 (list "d" "c" "c"))
;; ==> "c"
(two-eye temp-my-3 (list "d" "c" "d"))
;; ==> "c"
(two-eye temp-my-3 (list "d" "d" "c"))
;; ==> "d"
(two-eye temp-my-3 (list "d" "d" "c"))
;; ==> "d"

;;
;; "two-eye" performs as we would expect "eye-for-two-eyes" to perform.
;;

;;
;; Run the tests for "three-eye":
;;
(three-eye temp-my-1 (list "c"))
;; ==> "c"
(three-eye temp-my-1 (list "d"))
;; ==> "c"

(three-eye temp-my-2 (list "c" "c"))
;; ==> "c"
(three-eye temp-my-2 (list "c" "d"))
;; ==> "c"
(three-eye temp-my-2 (list "d" "c"))
;; ==> "c"
(three-eye temp-my-2 (list "d" "d"))
;; ==> "c"

(three-eye temp-my-3 (list "c" "c" "c"))
;; ==> "c"
(three-eye temp-my-3 (list "c" "c" "d"))
;; ==> "c"
(three-eye temp-my-3 (list "c" "d" "c"))
;; ==> "c"
(three-eye temp-my-3 (list "c" "d" "d"))
;; ==> "c"
(three-eye temp-my-3 (list "d" "c" "c"))
;; ==> "c"
(three-eye temp-my-3 (list "d" "c" "d"))
;; ==> "c"
(three-eye temp-my-3 (list "d" "d" "c"))
;; ==> "c"
(three-eye temp-my-3 (list "d" "d" "d"))
;; ==> "d"

(three-eye temp-my-4 (list "c" "c" "c" "c"))
;; ==> "c"
(three-eye temp-my-4 (list "c" "c" "c" "d"))
;; ==> "c"
(three-eye temp-my-4 (list "c" "c" "d" "c"))
;; ==> "c"
(three-eye temp-my-4 (list "c" "c" "d" "d"))
;; ==> "c"
(three-eye temp-my-4 (list "c" "d" "c" "c"))
;; ==> "c"
(three-eye temp-my-4 (list "c" "d" "c" "d"))
;; ==> "c"
(three-eye temp-my-4 (list "c" "d" "d" "c"))
;; ==> "c"
(three-eye temp-my-4 (list "c" "d" "d" "d"))
;; ==> "c"
(three-eye temp-my-4 (list "d" "c" "c" "c"))
;; ==> "c"
(three-eye temp-my-4 (list "d" "c" "c" "d"))
;; ==> "c"
(three-eye temp-my-4 (list "d" "c" "d" "c"))
;; ==> "c"
(three-eye temp-my-4 (list "d" "c" "d" "d"))
;; ==> "c"
(three-eye temp-my-4 (list "d" "d" "c" "c"))
;; ==> "c"
(three-eye temp-my-4 (list "d" "d" "c" "d"))
;; ==> "c"
(three-eye temp-my-4 (list "d" "d" "d" "c"))
;; ==> "d"
(three-eye temp-my-4 (list "d" "d" "d" "d"))
;; ==> "d"

;;
;; "three-eye" performs as we would expect "eye-for-three-eyes" to perform.
;;

;;
;; How should "eye-for-n-eyes" perform as n increases? 
;; 
;; At n=1, we would expect perform identical to EYE-FOR-EYE.
;;
;; As n increases, we would expect the performance of "eye-for-n-eyes" to asymptotically
;; approach the performance of PATSY. Let's define "eye-for-n-eyes" strategies up to n=5, 
;; and then n=20 and n=100, and see how these compare and contrast with EYE-FOR-EYE and PATSY:
;;
(define four-eye (make-eye-for-n-eyes 4))
(define five-eye (make-eye-for-n-eyes 5))
(define twenty-eye (make-eye-for-n-eyes 20))
(define hundred-eye (make-eye-for-n-eyes 100))

;;
;;               ------------------------------------------------------------------------------ 
;;               |     NASTY    |     PATSY    |    SPASTIC   |  EGALITARIAN  |  EYE-FOR-EYE  |
;; -------------------------------------------------------------------------------------------- 
;;   EYE-FOR-EYE |     Loses    |     Ties     |     Loses    |      Ties     |     Ties      |
;;               |  0.99 points |  3.0 points  |  2.16 points |   3.0 points  |   3.0 points  |
;; -------------------------------------------------------------------------------------------- 
;;      ONE      |     Loses    |     Ties     |     Loses    |      Ties     |     Ties      |
;;               |  0.99 points |  3.0 points  |  2.14 points |   3.0 points  |   3.0 points  |
;; -------------------------------------------------------------------------------------------- 
;;      TWO      |     Loses    |     Ties     |     Loses    |      Ties     |     Ties      |
;;               |  0.98 points |  3.0 points  |  1.94 points |   3.0 points  |   3.0 points  |
;; -------------------------------------------------------------------------------------------- 
;;     THREE     |     Loses    |     Ties     |     Loses    |      Ties     |     Ties      |
;;               |  0.97 points |  3.0 points  |  1.74 points |   3.0 points  |   3.0 points  |
;; -------------------------------------------------------------------------------------------- 
;;     FOUR      |     Loses    |     Ties     |     Loses    |      Ties     |     Ties      |
;;               |  0.96 points |  3.0 points  |  1.57 points |   3.0 points  |   3.0 points  |
;; --------------------------------------------------------------------------------------------
;;     FIVE      |    Loses     |     Ties     |     Loses    |      Ties     |     Ties      |
;;               |  0.95 points |  3.0 points  |  1.40 points |   3.0 points  |   3.0 points  |
;; --------------------------------------------------------------------------------------------
;;    TWENTY     |    Loses     |     Ties     |     Loses    |      Ties     |     Ties      |
;;               |  0.80 points |  3.0 points  |  1.34 points |   3.0 points  |   3.0 points  |
;; -------------------------------------------------------------------------------------------- 
;;   HUNDRED     |    Loses     |     Ties     |     Loses    |      Ties     |     Ties      |
;;               |  0.0 points  |  3.0 points  |  1.38 points |   3.0 points  |   3.0 points  |
;; -------------------------------------------------------------------------------------------- 
;;     PATSY     |    Loses     |     Ties     |     Loses    |      Ties     |     Ties      |
;;               |  0.0 points  |  3.0 points  |  1.41 points |   3.0 points  |   3.0 points  |
;; --------------------------------------------------------------------------------------------
;;
;; Note again, that SPASTIC is a stochastic strategy and will not generate strictly a strictly 
;; "linear" responsive the way that the other strategies might.
;;
;; These strategies all perform along a "linear" progression as expected. With small "n" we see
;; behavior (roughly) similar to EYE-FOR-EYE. With large "n" we see behavior (roughly) similar to 
;; PATSY.
;;

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Problem 6
;; 
;; Write a "make-rotating-strategy" procedure.
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;
;; I don't know how to do this without using mutators and local assignment.
;; The implementation that follows uses these techniques:
;;
(define (make-rotating-strategy strat0 strat1 freq0 freq1)
  ;;
  ;; We need to monitor how many strategy is executed:
  ;;
  (define (make-monitored f)
    (let ((count 0))
      ;;
      ;; We need to return a two-argument procedure, where
      ;; the arguments are "my-history" and "other-history".
      ;; Depending on where we are in the "count", this will 
      ;; determine which strategy is executed.
      ;;
      (define (mf m1 m2)
	(let ((total (remainder count (+ freq0 freq1))))
	  (set! count (+ count 1))
	  (cond ((< total freq0) (f strat0 m1 m2))
		(else (f strat1 m1 m2)))))
      mf))

  ;;
  ;; Return the monitored game strategy:
  ;;
  (make-monitored
   (lambda (strategy my-history other-history)
     (strategy my-history other-history))))

