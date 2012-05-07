(load-file "prisoner.el")

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
;; First, modify the max recursion depth, since emacs is pretty feeble this way:
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
(setq max-lisp-eval-depth 1000)
(setq max-specpdl-size 1800)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++ 
;; Problem 1
;; 
;; Definition of "extract-entry"
;; ++++++++++++++++++++++++++++++++++++++++++++++++++ 

;;
;; The *game-association-list* is defined as follows:
;;
(setq *game-association-list*
      (list (list (list "c" "c") (list 3 3))
	    (list (list "c" "d") (list 0 5))
	    (list (list "d" "c") (list 5 0))
	    (list (list "d" "d") (list 1 1))))

;;
;; We can extract a specific entry in this list by using the "list-ref" procedure. 
;;
;; For example:
;;
(nth 0 *game-association-list*)
;; ==> (("c" "c") (3 3))
(nth 1 *game-association-list*)
;; ==> (("c" "d") (0 5))

;;
;; and so on. To extract the entry associated with a specific play, we need to extract 
;; the "car" of the entry, and make sure that both elements of this "car" correspond 
;; to both elements of the argument play. 
;;
;; We define our "extract-entry" procedure as follows:
;;
(defun extract-entry (play *list*)
  ;; 
  ;; Returns "true" if the play matches the entry:
  ;;
  (defun compare (play entry)
    (let ((test (car entry)))
      (and (string= (car play) (car test))
	   (string= (cadr play) (cadr test)))))

  (let
      ;; 
      ;; Get references to each entry in the *game-association-list*:
      ;;
      ((first (nth 0 *list*))
       (second (nth 1 *list*))
       (third (nth 2 *list*))
       (fourth (nth 3 *list*)))

    ;; 
    ;; If we find a match, return that specific entry:
    ;;
    (cond
     ((compare play first) first)
     ((compare play second) second)
     ((compare play third) third)
     ((compare play fourth) fourth)
     (t '()))))

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
;; ==> nil

;;
;; Similarly, since "get-point-list" is defined as:
;;
(defun get-point-list (game)
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
;; I'm not going to reproduce all the game play statistics here, but
;; the results basically mirror those in the reference MIT-SCHEME implementation.
;; Refer to the reference MIT-SCHEME implementation for further details.
;;

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Problem 3
;; 
;; Explore more efficient ways to code EGALITARIAN.
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

;;
;; For reference, the original definition of EGALITARIAN was given as:
;;
(defun EGALITARIAN (my-history other-history)
  (defun count-instances-of (test hist)
    (cond ((empty-history? hist) 0)
	  ((string= (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (t
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
(defun EGALITARIAN (my-history other-history)
  (defun majority-loop (cs ds hist)
    (cond ((empty-history? hist) (if (> ds cs) "d" "c"))
	  ((string= (most-recent-play hist) "c")
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
;; Define a "real-time-clock" procedure to give a reading of time:
;;
(defun real-time-clock ()
  (let ((q (current-time)))
    (+ (car (cdr q))
       (/ (car (cdr (cdr q))) 10000000.0))))

;;
;; As a test, let's implement a "timed-play-loop" procedure that
;; (a) runs more play sets; and (b) prints out timing statistics, 
;; so we can see whether program execution actually performs the 
;; way we would predict.
;;
(defun timed-play-loop (strat0 strat1 times)

  ;;
  ;; Play-loop-iter procedure for executing the game play an arbitrary number of times
  ;;
  (defun timed-play-loop-iter (count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
	  (t
	   (let ((result0 (funcall strat0 history0 history1))
		 (result1 (funcall strat1 history1 history0)))
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
      (princ "Timing: ")
      (princ (number-to-string (- finish start))))))