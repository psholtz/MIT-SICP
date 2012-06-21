(load "prisoner3.scm")

;; +++++++++++++++++++++++++++++++++++++++++++++ 
;; Problem 9
;; 
;; []
;; +++++++++++++++++++++++++++++++++++++++++++++

;;
;; Define "play-loop" for handling three strategies:
;;
(define (play-loop strat0 strat1 strat2)
  (define (play-loop-iter count history0 history1 history2 limit)
    (cond ((= count limit) (print-out-results history0 history1 history2 limit))
	  (else
	   (let ((result0 (strat0 history0 history1 history2))
		 (result1 (strat1 history1 history0 history2))
		 (result2 (strat2 history2 history0 history1)))
	     (play-loop-iter (+ count 1)
			     (extend-history result0 history0)
			     (extend-history result1 history1)
			     (extend-history result2 history2)
			     limit)))))
  (play-loop-iter 0 the-empty-history the-empty-history the-empty-history (+ 90 (random 21))))

;;
;; Define "print-out-results" for handling three strategies:
;;
(define (print-out-results history0 history1 history2 number-of-games)
  (let ((scores (get-scores history0 history1 history2)))
    (newline)
    (display "Player 1 Score: ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score: ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)
    (display "Player 3 Score: ")
    (display (* 1.0 (/ (caddr scores) number-of-games)))
    (newline)))

;;
;; Define "get-scores" for handling three strategies:
;;
(define (get-scores history0 history1 history2)
  (define (get-scores-helper hist1 hist2 hist3 score0 score1 score2)
    (cond ((empty-history? history0)
	   (list score0 score1 score2))
	  (else
	   (let ((game (make-play 
			(most-recent-play hist0)
			(most-recent-play hist1)
			(most-recent-play hist2))))
	     (get-scores-helper 
	      (rest-of-plays hist0)
	      (rest-of-plays hist1)
	      (rest-of-plays hist2)
	      (+ (get-player-points 0 game) score0)
	      (+ (get-player-points 1 game) score1)
	      (+ (get-player-points 2 game) score2))))))
  (get-scores-helper history1 history2 history3 0 0 0))

;; possible:
(define (extract-entry) '())