;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Code to use in the three-person game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define *game-association-list*
  (list (list (list "c" "c" "c") (list 4 4 4))
        (list (list "c" "c" "d") (list 2 2 5))
        (list (list "c" "d" "c") (list 2 5 2))
        (list (list "d" "c" "c") (list 5 2 2))
        (list (list "c" "d" "d") (list 0 3 3))
        (list (list "d" "c" "d") (list 3 0 3))
        (list (list "d" "d" "c") (list 3 3 0))
        (list (list "d" "d" "d") (list 1 1 1))))


;; in expected-values: #f = don't care 
;;                      X = actual-value needs to be #f or X 
(define (test-entry expected-values actual-values) 
   (cond ((null? expected-values) (null? actual-values)) 
         ((null? actual-values) #f) 
         ((or (not (car expected-values)) 
              (not (car actual-values)) 
              (= (car expected-values) (car actual-values))) 
          (test-entry (cdr expected-values) (cdr actual-values))) 
         (else #f))) 

(define (is-he-a-fool? hist0 hist1 hist2) 
   (test-entry (list 1 1 1) 
               (get-probability-of-c 
                (make-history-summary hist0 hist1 hist2))))

(define (could-he-be-a-fool? hist0 hist1 hist2)
  (test-entry (list 1 1 1)
              (map (lambda (elt) 
                      (cond ((null? elt) 1)
                            ((= elt 1) 1)  
                            (else 0)))
                   (get-probability-of-c (make-history-summary hist0 
                                                               hist1
                                                               hist2)))))

;;
;; Import these from the "old" prisoner code:
;;
(define (get-player-points num game)
  (list-ref (get-point-list game) num))
(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

(define make-play list)
(define the-empty-history '())
(define extend-history cons)
(define empty-history? null?)
(define most-recent-play car)
(define rest-of-plays cdr)

;;
;; Add the two player strategies, for completeness (and subsequent problems)
;;
(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

;;
;; Use the optimized version of EGALITARIAN:
;;
(define (EGALITARIAN my-history other-history)
  (define (majority-loop cs ds hist)
    (cond ((empty-history? hist) (if (> ds cs) "d" "c"))
	  ((string=? (most-recent-play hist) "c")
	   (majority-loop (+ 1 cs) ds (rest-of-plays hist)))
	  (else
	   (majority-loop cs (+ 1 ds) (rest-of-plays hist)))))
  (majority-loop 0 0 other-history))

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

(define ONE-EYE (make-eye-for-n-eyes 1))
(define TWO-EYE (make-eye-for-n-eyes 2))
(define THREE-EYE (make-eye-for-n-eyes 3))
(define FOUR-EYE (make-eye-for-n-eyes 4))
(define FIVE-EYE (make-eye-for-n-eyes 5))
(define TWENTY-EYE (make-eye-for-n-eyes 20))
(define HUNDRED-EYE (make-eye-for-n-eyes 100))
