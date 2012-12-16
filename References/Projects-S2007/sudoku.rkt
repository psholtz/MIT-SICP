;;; sudoku system

;;; basic idea is to create a tagged list of values for each cell in 
;;; the grid.  These need to be shared across rows (represented as lists),
;;; columns (also represented as lists), and regions

; Size of the grid.  Must be a perfect square (4, 9, 16, etc.)
; Initialized to 4 to make debugging simple.
(define *size* 4)

; Size of a region.  For example, a 9x9 Sudoku grid has 3x3 regions.
; We make this a procedure rather than a variable so that the value of region-size
; always reflects the current value of *size*.
(define (region-size) (sqrt *size*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Useful list procedures

(define (generate-interval a b)
  ; create list of integers between a and b
  ; type: int,int -> List<int>
  (if (> a b)
      '()
      (cons a (generate-interval (+ a 1) b))))

(define (first-n lst n)
  ; return the first n elements of lst, as a list
  ; assumes length of lst is at least n
  ; type: List<A> -> List<A>
  (if (= n 0)
      '()
      (cons (car lst) (first-n (cdr lst) (- n 1)))))

(define (but-first-n lst n)
  ; return the rest of lst after removing the first n elements
  ; assumes length of lst is at least n
  ; type: List<A> -> List<A>
  (if (= n 0)
      lst
      (but-first-n (cdr lst) (- n 1))))

; contract: for any list L and any n >= (length L),
;    L = (append (first-n L n) (but-first-n L n))


;;;;;;;;;;;;;;;;;;;
;;; Cells

; Cell = Pair<tag-symbol, List<int>>

(define (make-cell)
  ; makes an instance of a cell, with values from 1 to *size*, with tag
  ; type: -> Cell
  (cons 'cell (generate-interval 1 *size*)))

; accessors

; get possible values for the cell
(define (cell-values cell) (cdr cell))


; mutators

; set possible values for the cell
(define (set-cell-values! cell values)
    (set-cdr! cell values))


;; to start building the grid, create a row of cells of desired size

;;;;;;;;;;;;;;;;;;;
;;; Rows

; Row = List<Cell>

(define (make-row n)
  ; makes a row of n cells
  ; type: int -> Row
  (if (= n 0)
      '()
      (cons (make-cell) (make-row (- n 1)))))

;; to continue building the grid, create desired number of rows of desired size

(define (make-rows n)
  ; make a list of n rows
  ; type: int -> List<Row>
  (if (= n 0)
      '()
      (cons (make-row *size*) (make-rows (- n 1)))))

;;;;;;;;;;;;;;;;;;;
;;; Columns

; Column = List<Cell>

;; given a grid represented as a list of rows, need to collect
;; elements of each row into a column 

;; YOU WILL NEED TO FILL IN make-column (Problem 1)

(define (make-column n rows)
  ; collect the nth element of each row into a column
  ; n specifies which element to collect, starting from 0
  ; type: int,List<Row> -> Column
  (map FILL-IN-HERE rows))

(define (make-columns rows)
  ; given a list of rows, make a list of columns from it
  ; each column should share cells with each row
  ; type: List<Row> -> List<Column>
  (define (helper n rows)
    (if (= n *size*)
        '()
        (cons (make-column n rows) (helper (+ n 1) rows))))
  (helper 0 rows))

;;;;;;;;;;;;;;;;;;;
;;; Regions

; Region = List<Cell>

(define (make-regions rows)
  ; given a list of rows, make a list of regions from it.
  ; type: List<Row> -> List<Region>
  (if (null? rows)
      '()
      (append (make-some-regions (first-n rows (region-size)))
              (make-regions (but-first-n rows (region-size))))))

;; YOU WILL NEED TO FILL IN make-some-regions for Problem 2

(define (make-some-regions some-rows)
  ; takes a few rows of the grid (a number of rows equal to (region-size))
  ; and returns their regions.
  ; type: List<Row> -> List<Region>

  FILL-IN-HERE
)



;;;;;;;;;;;;;;;;;;;
;;; Grid

; Grid = List: tag, List<Row>, List<Column>, List<Region>

(define (make-grid)
  (let ((rows (make-rows *size*)))
    (list 'grid rows (make-columns rows) (make-regions rows))))

;; here are some accessors for a game grid

(define (get-rows grid)
  (second grid))

(define (get-columns grid)
  (third grid))

(define (get-regions grid)
  (fourth grid))

(define (get-cell grid r c)
  ; gets the cell at row r, column c in the grid.
  ; (counting from 0).
  ; type: Grid,int,int -> Cell
  (let* ((rows (get-rows grid))
          (row (list-ref rows r))
          (cell (list-ref row c)))
    cell))
  
;; mutator for game grid
(define (set-value! grid r c val)
  (set-cell-values! (get-cell grid r c) (list val)))

;; special mutator for testing game grid construction
(define (set-value-row! rows r c val)
  (let ((row (list-ref rows r)))
    (let ((cell (list-ref row c)))
      (set-cell-values! cell (list val)))))

;; display function for game grid
(define (display-grid grid)
  ;; display the state of the grid.
  ;; if there is a unique answer, it prints it, otherwise it prints a dot
  ;; type: Grid -> void
  (define (print-dashed-line) 
    (printf (make-string (+ (* 2 *size*) (* 2 (region-size)) 1) #\-)) 
    (newline))

  (define (printable-value cell) 
    (let ((values (cell-values cell)))
      (if (= (length values) 1) (first values) ".")))
  
  (define (print-values cells)
    (for-each (lambda (cell) (printf "~a " (printable-value cell))) cells))

  (define (print-row row)
    (printf "| ")
    (if (null? row)
        (newline)
        (begin (print-values (first-n row (region-size)))
               (print-row (but-first-n row (region-size))))))
  
  (define (print-rows rows)
    (print-dashed-line)
    (if (not (null? rows))
        (begin (for-each print-row (first-n rows (region-size)))
               (print-rows (but-first-n rows (region-size))))))

  ;; main body of display-grid: print the rows of the grid
  (newline)
  (print-rows (get-rows grid)))


;;;;;;;;;;;;;;;;;;;
;;; Initial values
  
; Initial values are the values already filled into the grid at the start of the game.

; InitialValues = List< row,col,val >
;    each element in this list is a triple <r,c,val>
;    where r and c are row and column indexes (counting from 0)
;      and val is the number found in that cell

; initial values for a 4x4 grid
(define *initial-4x4-values* 
  '((0 0 3)       (0 2 2)
           (1 1 1)
    (2 0 4)
           (3 1 3)
                         (3 3 2)))

; initial values for a 9x9 grid
(define *initial-9x9-values* 
  '(        (0 1 6)       (0 3 1)       (0 5 4)      (0 7 5)
                   (1 2 8)(1 3 3)       (1 5 5)(1 6 6)
      (2 0 2)                                              (2 8 1)
      (3 0 8)             (3 3 4)       (3 5 7)            (3 8 6)
                   (4 2 6)                     (4 6 3)
      (5 0 7)             (5 3 9)       (5 5 1)            (5 8 4)
      (6 0 5)                                              (6 8 2)
                   (7 2 7)(7 3 2)       (7 5 6)(7 6 9)
            (8 1 4)       (8 3 5)       (8 5 8)      (8 7 7)))


(define (set-initial-values! grid initial-values)
  ; sets the initial values into a grid
  ; assumes grid has just been created
  ; type: Grid, InitialValues -> void
  (for-each (lambda (entry) (set-value! grid (first entry) (second entry) (third entry)))
       initial-values))


;;;;;;;;;;;;;;;;;;;
;;; User interface

(define (play-simple initial-values)
  ; Play Sudoku with the user.
  ; type: InitialValues -> void
  (let ((grid (make-grid)))
    (set-initial-values! grid initial-values)
    (display-grid grid)
    ;; go to driver loop
    (basic-driver-loop grid)
    ))

(define (basic-driver-loop grid)
  ; Get one command from the user and do it on the grid.
  ; type: Grid -> void
  (define (do-quit) 
    ;; user is ready to quit
    (void)
    ;; the procedure void is used to return a non-printing value
    )
  
  (define (do-set)
    ;; user wants to set a value in the grid
    (newline)
    (printf "What row?")
    (let ((r (read)))
      (newline)
      (printf "What column?")
      (let ((c (read)))
        (newline)
        (printf "What value?")
        (let ((v (read)))
          (set-value! grid r c v)
          (printf "~nHere is your change ~n")
          (display-grid grid)
          (basic-driver-loop grid)))))
  
  (define (do-unknown input)
    ;; unrecognized command
    (printf "~n Don't recognize character ~a. Try again." input)
    (basic-driver-loop grid))
  
  (printf "~nEnter a command, one of (q s)~n")
  (let ((input (read)))
    (cond ((eq? input 'q) (do-quit))
          ((eq? input 's) (do-set))
          (else (do-unknown input)))))
