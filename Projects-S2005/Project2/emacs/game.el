(load-file "prisoner.el")

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

;
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