;;
;; Exercise 2.74
;;
;; [WORKING]
;;

;; 
;; First we require a database:
;;
;; ======== 
;; DATABASE
;; ======== 
(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records)) (car records))
	(else
	   (assoc key (cdr records)))))

;; procedures for 1-d table
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	false)))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value) (cdr table)))))
  'ok)
(define (make-table-1d)
  (list '*table*))

;; procedures for 2-d table
(define (make-table-2d)
  ;; procedure for defining 2-dimensional tables
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define *operations* (make-table-2d))
(define get (*operations* 'lookup-proc))
(define put (*operations* 'insert-proc!))

;;
;; We also require tagging support:
;;
;; ===============
;; TAGGING LIBRARY
;; ===============
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum))) 

;;
;; (a) Implement for headquarters a get-record procedure that retrieves a specified employee's record 
;;     from a specified personnel file. The procedure should be applicable to any division's file. 
;;     Explain how the individual divisions' files should be structured. In particular, what type 
;;     information must be supplied?
;; 

;;
;; Let's implement constructor/selectors for each division's personnel file:
;;
(define (make-personnel-file division personnel-file)
  (attach-tag division personnel-file))
(define (division file)
  (type-tag file))
(define (personnel-file file)
  (contents file))

;;
;; In this model, the "personnel-file" argument to the constructor
;; will be a table (or other data structure) of employee records. 
;; Each department is free to implement this table/data structure
;;  however it desires.
;;			     

;;
;; We can now implement the "get-record" procedure in a generic 
;; manner as follows:
;;
(define (get-record employee file)
  ((get 'get-record (division file)) employee (personnel-file file)))

;;
;; The *operations* table in which we have stored the procedure is 
;; 2-dimensional, and will return a procedure which itself takes
;; 2 arguments: (a) the employee whose record we want to look up;
;; and (b) a data structure (i.e., table, list, tree, etc) of 
;; personnel records for that particular department.
;;

;;
;; Suppose for the sake of discussion we have two divisions in the 
;; company: engineering and marketing. We will tag our records either 
;; with the token "engineering" or "marketing" accordingly:
;;
(define (engineering-personnel-file? file)
  (eq? (division file) 'engineering))
(define (marketing-personnel-file? file)
  (eq? (division file) 'marketing))

;;
;; Suppose that the engineering department, being sophisticated, 
;; wants to use a table to store their employee records. We will 
;; use a 1d table to store the record itself (i.e., name, title 
;; and salary), and we will use a second 1d table to store the 
;; collection of records.
;;
(define (generate-engineering-records)
  (let ((*engineering-records* (make-table-1d)))
    ;; method for updating the records
    (define (append-record name title salary)
      (define *record* (make-table-1d))
      (insert! 'name name *record*)
      (insert! 'title title *record*)
      (insert! 'salary salary *record*)
      (insert! name *record* *engineering-records*))

    ;; append the employee records
    (append-record 'jones '(c++ programmer) 105000)
    (append-record 'brown '(java programmer) 105000)
    (append-record 'dashwood '(php programmer) 92000)
    *engineering-records*))

(define *engineering-file* (make-personnel-file 'engineering (generate-engineering-records)))

(division *engineering-file*)
;; ==> 'engineering
(personnel-file *engineering-file*)
;; ==> #[complicated data structure]

;;
;; Suppose on the contrary, that the marketing department wants to 
;; implement their data model in a more "simple" manner. For marketing, 
;; a personnel file will consists of records which are simply list
;; structures, as follows: (1) name; (2) title; (3) salary. 
;;
;; So for instance, a record from the marketing department might 
;; look like:
;;
;;  '(smith '(vp marketing) 130000)
;;
;; A very different structure from that used by engineering!
;;
;; Let's design a procedure to install these records.
;;
(define (generate-marketing-records)
  (let ((*marketing-records* 
	 '((smith (vp marketing) 130000)
	   (anderson creative 70000)
	   (wilson advertising 85000))))
    *marketing-records*))

(define *marketing-file* (make-personnel-file 'marketing (generate-marketing-records)))

(division *marketing-file*)
;; ==> marketing
(personnel-file *marketing-file)
;; ==> ((smith (vp marketing) 130000) (anderson creative 70000) (wilson advertising 85000))

;;
;; Now let's define the procedures for installing these modules at headquarters (so to speak):
;;
(define (install-engineering-records)
  (define (get-record employee file)
    (lookup employee file))
  (put 'get-record 'engineering get-record)
  'done)

(define (install-marketing-records)
  (define (get-record employee file)
    (define (get-record-iter working)
      (if (null? working)
	  #f
	  (let ((record (car working)))
	    (if (eq? (car record) employee)
		record
		(get-record-iter (cdr working))))))
    (get-record-iter file))
  (put 'get-record 'marketing get-record)
  'done)

(install-engineering-records)
;; ==> 'done
(install-marketing-records)
;; ==> 'done

;;
;; Let's see how well this works!
;;
;; First look up names in the engineering division:
;;
(get-record 'jones *engineering-file*)
;; ==> #[data structure for jones]
(get-record 'brown *engineering-file*)
;; ==> #[data structure for brown]
(get-record 'dashwood *engineering-file*)
;; ==> #[data structure for dashwood]
(get-record 'smith *engineering-file*)
;; ==> #f

;;
;; Now look up names in the marketing division:
;;
(get-record 'smith *marketing-file*)
;; ==> (smith (vp marketing 130000)
(get-record 'anderson *marketing-file*)
;; ==> (anderson creative 70000)
(get-record 'wilson *marketing-file*)
;; ==> (wilson advertising 85000)
(get-record 'jones *marketing-file*)
;; ==> #f

;;
;; Seems to work pretty well!
;;

;;
;; (b) Implement for headquarters a get-salary procedure that returns the salary information 
;; from a given employee's record from any division's personnel file. How should the record 
;; be structured in order to make this operation work?
;;

;;
;; We need to make two changes:
;;
;;  (1) We need to update the "generate-records" procedure to tag each
;;      individual record with the appropriate generic label.
;;  (2) We need to update the "install-records" procedure to include
;;      the "get-salary" procedure.
;;

;;
;; Update the "generate-engineering-records" procedure:
;;
(define (generate-engineering-records)
  (let ((*engineering-records* (make-table-1d)))
    ;; method for updating the records
    (define (append-record name title salary)
      (define *record* (make-table-1d))
      (insert! 'name name *record*)
      (insert! 'title title *record*)
      (insert! 'salary salary *record*)
      (insert! name                                  ;; <== add a generic label to each record
	       (attach-tag 'engineering *record*)
	       *engineering-records*))

    ;; append the employee records
    (append-record 'jones '(c++ programmer) '105000)
    (append-record 'brown '(java programmer) '105000)
    (append-record 'dashwood '(php programmer) '92000)
    *engineering-records*))

(define *engineering-file* (make-personnel-file 'engineering (generate-engineering-records)))

;;
;; Engineering files now contain the generic label, which is what we want:
;;
(define record1 (get-record 'brown *engineering-file*))
(division record1)
;; ==> engineering
(contents record1)
;; ==> (*table* (salary . 105000) (title java programmer) (name . brown))

;;
;; Updating the installation procedure is straightforward (just add the "get-salary" procedure):
;;
(define (install-engineering-records)
  ;; define the procedures:
  (define (get-record employee file)
    (lookup employee file))
  (define (get-salary record)
    (lookup 'salary record))

  ;; install the procedures:
  (put 'get-record 'engineering get-record)
  (put 'get-salary 'engineering get-salary)
  'done)

;;
;; Changes to make to the "generate-marketing-records" procedure
;; (add a generic label to each record using map):
;;
(define (generate-marketing-records)
  (define (make-record name title salary)
    (list name title salary))
  (let ((record1 (make-record 'smith '(vp marketing) 130000))
	(record2 (make-record 'anderson 'creative 70000))
	(record3 (make-record 'wilson 'advertising 85000)))
    (let ((records (list record1 record2 record3)))
      (map (lambda (x) (attach-tag 'marketing x)) records))))

(define *marketing-file* (make-personnel-file 'marketing (generate-marketing-records)))

;;
;; Updating the installation package is straightforward:
;;
(define (install-marketing-records)
  ;; define the procedures:
  (define (get-record employee file)
    (define (get-record-iter working)
      (if (null? working)
	  #f
	  (let ((tagged-record (car working)))
	    (if (eq? (cadr tagged-record) employee)    ;; <== name is now in second position, generic label in first
		tagged-record
		(get-record-iter (cdr working))))))
    (get-record-iter file))
  (define (get-salary record)
    (caddr record))
  
  ;; install the procedures:
  (put 'get-record 'marketing get-record)
  (put 'get-salary 'marketing get-salary)
  'done)

;;
;; We need to reinstall these records:
;;
(install-engineering-records)
;; ==> 'done
(install-marketing-records)
;; ==> 'done

;;
;; Finally, define the generic "get-salary" file:
;;
(define (get-salary record)
  ((get 'get-salary (division record)) (contents record)))
