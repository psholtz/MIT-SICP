
;;
;; Exercise 2.42
;;

;;
;;  +---+---+---+---+---+---+---+---+
;;  |   |   |   |   |   | X |   |   |
;;  +---+---+---+---+---+---+---+---+
;;  |   |   | X |   |   |   |   |   |
;;  +---+---+---+---+---+---+---+---+
;;  | X |   |   |   |   |   |   |   |
;;  +---+---+---+---+---+---+---+---+
;;  |   |   |   |   |   |   | X |   |
;;  +---+---+---+---+---+---+---+---+
;;  |   |   |   |   | X |   |   |   |
;;  +---+---+---+---+---+---+---+---+
;;  |   |   |   |   |   |   |   | X |
;;  +---+---+---+---+---+---+---+---+
;;  |   | X |   |   |   |   |   |   |
;;  +---+---+---+---+---+---+---+---+
;;  |   |   |   | X |   |   |   |   |
;;  +---+---+---+---+---+---+---+---+
;;


;;
;; The "eight-queens puzzle" asks how to place eight queens on a chessboard so that no 
;; queen is in check from any other (i.e,. no two queens are in the same row, column 
;; or diagonal). One possible solution is shown above. One way to solve the puzzle is to work
;; across the board, placing a queen in each column. Once we have placed k-1 queens, we must place
;; the kth queen in a position where it does not check any of the queens already on the 
;; board. We can formulate this approach recursively: Assume that we have already generated the 
;; sequence of all possible ways to place k-1 queens in the first k-1 columns of the board. 
;; For each of these ways, generate an extended set of positions by placing a queen in each
;; row of the kth column. Now filter these, keeping only the positions for which the queen
;; in the kth column is safe with respect to the other queens. This produces the sequence
;; of all ways to place k queens in the first k columns. By continuing this process, we will 
;; produce not only one solution, but all solutions to the puzzle.
;;

;;
;; We implement this solution as a procedure "queens", which returns a sequence of all solutions
;; to the problem of placing n queens on an n x n chessboard. "queens" has an internal procedure
;; "queen-cols" that returns the sequence of all ways to place queens in the first k columns of
;; the board.
;;
;; (define (queens board-size)
;;  (define (queen-cols k)
;;   (if (= k 0)
;;       (list empty-board)
;;       (filter 
;;        (lambda (positions) (safe? k positions))
;;        (flatmap
;;         (lambda (rest-of-queens)
;;          (map (lambda (new-row)
;;                (adjoin-position new-row k rest-of-queens))
;;               (enumerate-interval 1 board-size)))
;;        (queen-cols (- k 1))))))
;;  (queen-cols board-size))
;;

;;
;; First define the supporting procedures required by "queens".
;;
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;;
;; Define "queens" procedure, as given in problem statement:
;;
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

;;
;; Working out the problem with pen and paper, it's not hard 
;; to see that: (1) there is exactly 1 solution for a 1x1 
;; board; (2) there are no solutions for a 2x2 board or 
;; a 3x3 board; and (3) there are two solutiosn for a 4x4 board, 
;; which can be indicated as below:
;;

;;
;;   +---+---+---+---+    +---+---+---+---+
;;   |   | X |   |   |    |   |   | X |   |
;;   +---+---+---+---+    +---+---+---+---+
;;   |   |   |   | X |    | X |   |   |   |
;;   +---+---+---+---+    +---+---+---+---+
;;   | X |   |   |   |    |   |   |   | X |
;;   +---+---+---+---+    +---+---+---+---+
;;   |   |   | X |   |    |   | X |   |   |
;;   +---+---+---+---+    +---+---+---+---+
;;

;;
;; These actually "are" the "same" solution only reflected
;; through 180 degrees; there's probably some kind of deep
;; mathematical group symmetry operation going on here, but 
;; it's not worth it to delve into it too deeply here.
;;

;;
;; First we need to choose a data structure representation 
;; for the solution. Let us agree that one "solution" to the 
;; queens problem will consist of a list of "rows", indexed 
;; by "column", indicating the rows where it is safe to place a 
;; queen. This list will be k-elements long, where k is the 
;; board size (since we need to accomodate k columns). 
;;
;; Naturally, since there may be more than one solution for a 
;; given board size, we will return a list of such lists. 
;;
;; So the solution we expect for the 1x1 board will be:
;; 
;;  ((1))
;;
;; while the solution for the 4x4 board, corresponding to the 
;; two solutions indicated above, respectively, would be:
;;
;;  ((3 1 4 2) (2 4 1 3))
;;
;; The example given above, in the problem statement, for the 
;; 8x8 board would correspond to the list:
;;
;;  (3 7 2 8 5 1 4 6)
;;
;; Since this would be just one solution, out of many, the response
;; returned by the procedure will look something like:
;;
;;  (... (3 7 2 8 5 1 4 6) ...)
;;
;; and so on...
;;

;;
;; Naturally the "empty board" will be indicated by the empty list:
;;
(define empty-board '())

;;
;; Next we need to decide on a data representation that we can use 
;; to implement "adjoin-position" and "safe?". Let's work with the 
;; 4x4 board example.
;;
;; Again, after a little pen-and-paper work, it isn't difficult to 
;; discern that the "safe" ways to place 3 queens in the first 3 
;; columns of a 4x4 board are as follows (expressing this information
;; in the "embedded list" data structure model we described above):
;;
;;  ((1 4 2) (2 4 1) (3 1 4) (4 1 3))
;;
;; For each of these "potential solutions" we now want to add an 
;; additional element, representing all values of the next column 
;; to test. So in our exampe, there will be 4 such columns, which 
;; means we need to generate the following set of values to "test"
;; whether or not they are "safe?" for queen placement:
;;
;;  ((1 4 2 1) (1 4 2 2) (1 4 2 3) (1 4 2 4)
;;   (2 4 1 1) (2 4 1 2) (2 4 1 3) (2 4 1 4)
;;   (3 1 4 1) (3 1 4 2) (3 1 4 3) (3 1 4 4)
;;   (4 1 3 1) (4 1 3 2) (4 1 3 3) (4 1 3 4))
;;
;; Indeed, this corresponds to "augmenting" our data set by "growing"
;; the chessboard out to the right and downwards. It could be 
;; implemented by defining "adjoin-position" as simply as the following:
;;
;;  (define (adjoin-position new-row k rest-of-queens)
;;   (append rest-of-queens (list new-row)))
;;
;; However, "append" is an expensive operation. "cons" is much cheaper
;; than "append". Is there a way to construct the same data structure
;; using just "cons"? Suppose we try to grow the chessboard out to the 
;; left, instead of to the right? Then the augmented data structures 
;; we are searching for would be generated by "pre-pending" the sequence
;; (1..4) to each element of the above list, as follows:
;;
;;  ((1 1 4 2) (2 1 4 2) (3 1 4 2) (4 1 4 2)
;;   (1 2 4 1) (2 2 4 1) (3 2 4 1) (4 2 4 1)
;;   (1 3 1 4) (2 3 1 4) (3 3 1 4) (4 3 1 4)
;;   (1 4 1 3) (2 4 1 3) (3 4 1 3) (4 4 1 3))
;;
;; This is, I think, where the "mathematical group symmetry" of this 
;; problem arises (i.e., that the board can be grown out in either 
;; direction), but again, we won't dwell on that here, and instead 
;; we'll define "adjoin-position" as follows:
;;
(define (adjoin-position new-row column rest-of-queens)
  (cons new-row rest-of-queens))

;;
;; Note that because of how we defined our data structures, 
;; we don't actually need to pass the column parameter into 
;; this procedure call.
;;

;;
;; There are three things we need to check for in the "safe?" procedure:
;;
;;  (1) Columns: we need to make sure there is no column w/ more than one queen in it
;;  (2) Rows: we need to make sure that there is no row w/ more than one queen in it
;;  (3) Diagonals: we need to make sure there is no diagonal w/ more than one queen in it
;;
;; The column-safety is an artefact of the data structure we have choosen: there 
;; is no possible way to have more than one queen in one column, given the way 
;; we are counting. Checking for the row-safety should, similarly, be relatively 
;; straightforward: just make sure that the "newly added" row number does not 
;; re-occur anywhere else in the list data structure. The check for diagonal-safety
;; can be implemented recursively: we define a procedure to walk down the list 
;; structure from the first row-element next to the newly added row. At each iteration
;; we keep track of the number of steps down the list structure we are. If this 
;; number - added to or subtracted from - the newly added row number equals the 
;; row number, we know that we are in conflict along the diagonal. 
;;
(define (safe? column positions)
  (define (safe-iter? new-row remaining-rows row-offset)
    (if (null? remaining-rows)
	#t
	(let ((current-row (car remaining-rows)))
	  (if (or
	       (= new-row current-row)
	       (= new-row (+ current-row row-offset))
	       (= new-row (- current-row row-offset)))
	      #f
	      (safe-iter? new-row (cdr remaining-rows) (+ row-offset 1))))))
  (safe-iter? (car positions) (cdr positions) 1))

;;
;; Let's run some unit tests:
;;
(queens 1)
;; ==> ((1))

(queens 2)
;; ==> ()

(queens 3)
;; ==> ()

(queens 4)
;; ==> ((3 1 4 2) (2 4 1 3))

;;
;; For the larger board sizes, let's just count the number of solutions:
;;
(length (queens 5))
;; ==> 10
(length (queens 6))
;; ==> 4
(length (queens 7))
;; ==> 40
(length (queens 8))
;; ==> 92

;;
;; These are the answers we anticipated getting.
;;