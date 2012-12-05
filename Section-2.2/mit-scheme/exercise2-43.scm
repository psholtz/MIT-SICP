;;
;; Exercise 2.43
;;
;; Louis Reasoner is having a terrible time doing exercise 2.42. His "queens" procedure seems to work, 
;; but it runs extremely slowly. (Louis never manages to wait long enough for it to solve even the 
;; 6x6 case). When Louis asks Eva Lu Ator for help, she points out that he has interchanged the order
;; of the nested mappings in the "flatmap", writing it as:
;; 
;;  (flatmap
;;   (lambda (new-row)
;;    (map (lambda (rest-of-queens)
;;          (adjoin-position new-row k rest-of-queens))
;;         (queen-cols (- k 1))))
;;   (enumerate-interval 1 board-size))          
;;      
;; Explain why this interchange makes the program run slowly. Estimate how long it will take Louis'
;; program to solve the 8-queens puzzle, assuming that the program in Exercise 2.42 solevs the puzzle
;; in time T.
;;

;;
;; Supporting procedures:
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
;; Good to have "map" available also, in case we need to refer to it:
;;
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
	    (map proc (cdr items)))))

;;
;; Let's start by looking at the application of "flatmap" from Exercise 2.42:
;;
(flatmap
 (lambda (rest-of-queens)
   (map (lambda (new-row)
	  (adjoin-position new-row k rest-of-queens))
	(enumerate-interval 1 board-size)))
 (queen-cols (- k 1)))

;;
;; ==================
;; ORIGINAL PROCEDURE
;; ================== 
;;
;; In this application, flatmap only invokes the "queen-cols" procedure once, 
;; to generate the data set it will apply its argument procedure to. Each 
;; invocation of "queen-cols", however, takes the "filter" branch, and thus results
;; in a recursive call back to "flatmap" unless we are invoking "queen-cols" with k=0, 
;; in which case the recursion terminates with the empty board.
;;
;; This means that if we invoke the "queens" procedure with "board-size", the 
;; "queen-cols" procedure will be invoked a total of "board-size+1" times, although
;; only "board-size" of those invocation will result in a call to "flatmap". 
;;              
;; The number of times that the argument procedure to "flatmap" is invoked at each
;; iteration will depend on the number of elements in (queen-cols (- k 1)) at that 
;; iteration point. Whatever this number is, call it n(k) (i.e., it depends on the 
;; iteration k), we will have n(k) invocations of "enumerate-interval" and n(k) * board-size
;; invocations of "adjoin-position". Both of these procedures execute extremely 
;; rapidly: "enumerate-interval" simply counts from 1 up to board-size (let's use 
;; a board-size of 8), and adjoin-position is "syntactic sugar" for "cons", which 
;; also is an extremely fast operation.
;;
;; So to sum up, for 1 invocation of "flatmap", we have the following number of 
;; invocations of the each sub-procedure:
;;
;;  QUEEN-COLS: 1 
;;  ENUMERATE-INTERVAL: n(k)
;;  ADJOIN-POSITION: n(k) * 8
;;
;; where n(k) is the value of (queen-cols (- k 1)).
;;

;;
;; Now let's look at Louis Reasoner's application of flatmap:
;;
(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
	  (adjoin-position new-row k rest-of-queens))
	(queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

;;
;; ========================== 
;; LOUIS REASONER'S PROCEDURE
;; ========================== 
;;
;; In this application, flatmap will invoke "enumerate-interval" only once,
;;
;; The argument method used by flatmap, however, will be invoked "board-size" 
;; number of times (let's use 8, so 8 times). For each on of these invocations 
;; (again, let's call this number n(k)), we will invoke "adjoin-position" once.
;; So the total number of invocations for "adjoin-position" will be n(k)*8.
;;
;; Adding up the totals for this invocation of "flatmap", we have:
;;
;;  ENUMERATE-INTERVAL: 1
;;  QUEEN-COLS: 8
;;  ADJOIN-POSITION: n(k) * 8
;;
;; where n(k) is the value of (queen-cols (- k 1)).
;;

;;
;; So the difference between the two procedures is that they essentially "switch"
;; the order/cost of evaluating "enumerate-interval" and "queen-cols". Since "queen-cols"
;; is BY FAR the most time-consuming operation in this call stack ("adjoin-position" and 
;; "enumerate-interval" being extremely quick, linear-time-or-less operations for the 
;; purposes of this application), the time it takes to evaluate "queen-cols" is going to 
;; dominate the time it takes for the entire procedure to complete.
;;
;; And in Louis Reasoner's procedure, "queen-cols" is being invoked 8 times at each 
;; iteration step, while in the original procedure, "queen-cols" was invoked only once
;; at each iteration step. This is the reason why Louis' procedure runs so much slower.
;;

;;
;; I wrote a small Python script to count the number of invocations made to "queen-cols"
;; when using Louis Reasoner's application of the flatmap procedure. In this table, n 
;; represents the board size, and varies from n=2 to n=8:
;;
;; -----------------------
;; |  N  |  Total calls  |
;; -----------------------
;; |  2  |  7            |
;; |  3  |  40           |
;; |  4  |  341          |
;; |  5  |  3,906        |
;; |  6  |  55,987       |
;; |  7  |  960,800      |
;; |  8  |  19,173,961   |
;; ----------------------- 
;;
;; Indeed, the numbers skyrocket very quickly with increasing n.
;;
;; Working it out with a pen and paper, or else following the call graphs in Python 
;; leads us to understand what is happening: the procedure "queen-cols" is being called
;; 1 + 2 + 4 for n=2, it is being called 1 + 3 + 9 + 27 times for n=3, and so on. For 
;; n=8, it is being called 1 + 8 + 8^2 + ... + 8^8 times. 
;;
;; In fact, we can generate a quick procedure to give us the number of times "queen-cols"
;; will be invoked, given n:
;;
(define (f n)
  (define (f-iter k total)
    (let ((value (expt n k)))
      (if (= k n)
	  (+ total value)
	  (f-iter (+ k 1) (+ total value)))))
  (f-iter 0 0))

;;
;; Invoking this procedure with the various "n" will regenerate the table listed above.
;;
;; To answer the question of how long it takes Louis Reasoner's program to run, if 
;; the "normal" program runs in time T, we will assume that the "bulk" of the computational 
;; time is spent evaluating the "queen-cols" procedure. The other two procedures involved, 
;; "enumerate-interval" and "adjoin-position", execute in near constant time for n = 8. 
;;
;; Thus, in Louis Reasoner's program, "queen-cols" is invoked (f 8) ==> 19.173,961 times.
;;
;; In the "normal" execution of the program, "queen-cols" is invoked 9 times. 
;;
;; We obtain the ratio of the performance difference:
;;
(define a (/ (f 8) 9.0))
a
;; ==> 2,130,440

;;
;; In other words, Louis Reasoner's program may run up to 2 million times slower(!!)
;;
;; We can express this as a power of 8 (the number of squares on the board):
;;
(/ (log a) (log 8))
;; ==> 7.008

;;
;; Or in other words, Louis Reasoner's program runs nearly 8^7 times slower than the "normal" version(!!)
;;

;;
;; Taking the following calculation into consideration, we see that if the normal version 
;; of the program executes in about 100 microseconds -- not unreasonable on today's computing hardware --
;; Louis' version of the program will take about 213 seconds, or about 3 1/2 minutes, to complete:
;;

(* a (expt 10 -4.0))
;; ==> 213.0
(/ (* a (expt 10 -4.0)) 60.0)
;; ==> 3.55

;;
;; This is (very roughly) the level of performance we saw in the program (i.e., it took several 
;; minutes to completely for n = 8).
;;