;; 
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun play-loop (strat0 strat1)
  (defun play-loop-iter (strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
	  (t
	   (let ((result0 (strat0 history0 history1))
		 (result1 (strat1 history1 history0)))
	     (play-loop-iter strat0 
			     strat1 
			     (+ count 1) 
			     (extend-history result0 history0) 
			     (extend-history result1 history1)
			     limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history (+ 90 (random 21))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-out-results (history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)
    (princ "Player 1 Score:  ")
    (princ (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (princ "Player 2 Score:  ")
    (princ (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)))

(defun get-scores (history0 history1)
  (defun get-scores-helper (history0 history1 score0 score1)
    (cond ((empty-history? history0)
	   (list score0 score1))
	  (t
	   (let ((game (make-play (most-recent-play history0)
				  (most-recent-play history1))))
	     (get-scores-helper (rest-of-plays history0)
				(rest-of-plays history1)
				(+ (get-player-points 0 game) score0)
				(+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(defun get-player-points (num game)
  (list-ref (get-point--list game) num))

(setq *game-association-list*
  ;; format is that first sublist identifies the players' choices 
  ;; with "c" for cooperate and "d" for defect; and that second sublist 
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

(defun get-point-list (game)
  (cadr (extract-entry game *game-association-list*)))

;; note that you will need to write extract-entry

;;
;; Have to use odd way of naming functions in Emacs
;;
(defun make-play (&rest x)
  (car (funcall 'list x)))

(setq the-empty-history '())

(defun extend-history (a b)
  (funcall 'cons a b))
(defun empty-history? (x)
  (funcall 'null x))
		      
(defun most-recent-play (x)
  (funcall 'car x))
(defun rest-of-plays (x)
  (funcall 'cdr x))

;;
;; A sampler of strategies
;;
(defun NASTY (my-history other-history)
  "d")

(defun PATSY (my-history other-history)
  "c")

(defun SPASTIC (my-history other-history)
  (if (= (random 2) 0)
      "c"
    "d"))

;; [working]
;;(defun EGALITARIAN (my-history other-history)
;;  (defun count-instances-of (test hist)
;;    (cond ((empty-history? hist) 0)
;;	  ((

(defun EYE-FOR-EYE (my-history other-history)
  (if (empty-history? my-history)
      "c"
    (most-recent-play other-history)))