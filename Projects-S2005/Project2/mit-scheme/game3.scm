;;(load "prisoner3.scm")

;; +++++++++++++++++++++++++++++++++++++++++++++++++
;; Problem 9
;; 
;; Revise the scheme code to handle three players.
;; +++++++++++++++++++++++++++++++++++++++++++++++++

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

;;
;; Define "extract-entry" for handling three strategies:
;;
(define (extract-entry play *list*)
  ;;
  ;; Return "true" if the play matches the entry:
  ;;
  (define (compare play entry)
    (let ((test (car entry)))
      (and (string=? (car play) (car test))
	   (string=? (cadr play) (cadr test))
	   (string=? (caddr play) (caddr test)))))

  (let
      ;; 
      ;; Get references to each entry in the *game-association-list*:
      ;;
      ((first (list-ref *list* 0))
       (second (list-ref *list* 1))
       (third (list-ref *list* 2))
       (fourth (list-ref *list* 3))
       (fifth (list-ref *list* 4))
       (sixth (list-ref *list* 5))
       (seventh (list-ref *list* 6))
       (eighth (list-ref *list* 7)))

    ;;
    ;; If we find a match, return that specific entry:
    ;;
    (cond 
     ((compare play first) first)
     ((compare play second) second)
     ((compare play third) third)
     ((compare play fourth) fourth)
     ((compare play fifth) fifth)
     ((compare play sixth) sixth)
     ((compare play seventh) seventh)
     ((compare play eighth) eighth)
     (else '()))))

;;
;; We can test our procedure as follows:
;;
(extract-entry (make-play "c" "c" "c") *game-association-list*)
;; ==> (("c" "c" "c") (4 4 4)) 
(extract-entry (make-play "c" "c" "d") *game-association-list*)
;; ==> (("c" "c" "d") (2 2 5))
(extract-entry (make-play "c" "d" "c") *game-association-list*)
;; ==> (("c" "d" "c") (2 5 2))
(extract-entry (make-play "c" "d" "d") *game-association-list*)
;; ==> (("c" "d" "d") (0 3 3))
(extract-entry (make-play "d" "c" "c") *game-association-list*)
;; ==> (("d" "c" "c") (5 2 2))
(extract-entry (make-play "d" "c" "d") *game-association-list*)
;; ==> (("d" "c" "d") (3 0 3))
(extract-entry (make-play "d" "d" "c") *game-association-list*)
;; ==> (("d" "d" "c") (3 3 0))
(extract-entry (make-play "d" "d" "d") *game-association-list*)
;; ==> (("d" "d" "d") (1 1 1))
(extract-entry (make-play "x" "x" "x") *game-association-list*)
;; ==> ()

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++ 
;; Problem 10
;;
;; Write a range of three player strategies.
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++

;;
;; Write strategies "PATSY-3", "NASTY-3" and "SPASTIC-3":
;;

;;
;; These three procedures are straightforward, as the
;; results returned do not depend on the state of either
;; of the other argument histories.
;;
(define (PATSY-3 my-history other-history-1 other-history-2)
  "c")

(define (NASTY-3 my-history other-history-1 other-history-2)
  "d")

(define (SPASTIC-3 my-history other-history-1 other-history-2)
  (if (= (random 2) 0)
      "c"
      "d"))

;;
;; Write strategies "TOUGH-EYE-FOR-EYE" and "SOFT-EYE-FOR-EYE":
;;
(define (TOUGH-EYE-FOR-EYE my-history other-history-1 other-history-2)
  (if (empty-history? my-history)
      "c"
      (let ((p1 (most-recent-play other-history-1))
	    (p2 (most-recent-play other-history-2)))
	(or (string=? p1 "d")
	    (string=? p2 "d"))
	"d" "c")))

(define (SOFT-EYE-FOR-EYE my-history other-history-1 other-history-2)
  (if (empty-history? my-history)
      "c"
      (let ((p1 (most-recent-play other-history-1))
	    (p2 (most-recent-play other-history-2)))
	(and (string=? p1 "d")
	     (string=? p2 "d"))
	"d" "c")))
