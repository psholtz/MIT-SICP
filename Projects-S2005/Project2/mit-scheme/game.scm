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