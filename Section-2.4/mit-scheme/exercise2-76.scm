;;
;; Exercise 2.76
;;
;; [WORKING]
;;

;; ===========================
;; HANDLING GENERIC OPERATIONS
;; =========================== 

;; 
;; The question asks us to consider three ways of handling generic operations: 
;; 
;;  1. Explicit Dispatch (i.e., Dispatching on Type)
;;  2. Data-Directed Style
;;  3. Message-Passing Style
;; 
;; The first two techniques are similar in the sense that both could be described
;; as "intelligent operations", where the knowledge of and the logic pertaining to
;; the generic type set is maintained in methods/procedures/functions described
;; to accept generic types as arguments. Specifically, (i) the procedure is invoked
;; with a generic type as argument; (ii) the procedure parses the specific type 
;; of the argument; and (iii) based on the specific type, different actions are 
;; undertaken by the procedure.
;;
;; These two techniques differ from one another in a manner that will be described 
;; in greater detail below.
;;
;; The third technique, Message-Passing Style, could be described instead as more
;; of an "intelligent object" style of design, where the knowledge of and logic 
;; pertaining to the generic type set is maintained in the generic object itself. 
;; In this model, the data object itself receives the requested operation name as 
;; message, and handles that message correctly according to its specific type. 
;;
;; This third style of handling generic operations has evolved into what is now
;; known as "object-oriented programming" (OO).
;;
;; The similarities (and differences) between these two methods can be visualized
;; by a table like the following:
;;

;;
;;              +-----------------+-----------------------+
;;              |      POLAR      |      RECTANGULAR      |
;;  +-----------+-----------------+-----------------------+
;;  | real-part | real-part-polar | real-part-rectangular |
;;  +-----------+-----------------+-----------------------+
;;  | imag-part | imag-part-polar | imag-part-rectangular |
;;  +-----------+-----------------+-----------------------+
;;  | magnitude | magnitude-polar | magnitude-rectangular |
;;  +-----------+-----------------+-----------------------+
;;  |   angle   |   angle-polar   |   angle-rectangular   |
;;  +-----------+-----------------+-----------------------+
;; 

;;
;; This table describes four operations/procedures that we would like to apply 
;; objects generically, that is, without regard to the underlying type or way 
;; in which the underlying data is represented: "real-part", "imag-part", 
;; "magnitude" and "angle". These four operations are listed on the left hand 
;; column.
;;
;; The table furthermore lists two generic types to which these procedures/operations
;; are to be applied: both data models are meant to represent complex numbers, 
;; but in one model, complex numbers are represented using polar coordinates, and 
;; in the second model, they are represented using rectangular coordinates. These
;; two types of data models are listed across the top horizontal axis.
;;
;; The eight entries in the table correspond to the eight "procedures" or "logic units"
;; that we must program and access in order to handle all possible cases which might arise.
;;
;; The first way of handling generic operations, i.e., EXPLICIT DISPATCH, or 
;; DISPATCHING ON TYPE, segments this table horizontally, in that each of the four 
;; procedures is programmed with the logic needed to accept generic types, parse
;; the type, and then dispatch to the appropriate sub-procedure as necessary.
;;
;; The third way of handling generic operations, i.e., MESSAGE-PASSING STYLE, segments
;; this table vertically, in that the objec that represents a polar complex number 
;; receives one of four possible messages (i.e., "real-part", "imag-part", "magnitude", 
;; or "angle"), and dispatching to the appropriate sub-procedure is handled in the 
;; object itself.
;;
;; In the second way of handling generic operations, i.e., DATA-DIRECTED STYLE, the 
;; dispatch logic is still contained in procedures, as in the first style, but the 
;; entire table itself is abstracted out into a separate piece of logic, usually as
;; something that can be represented or used as a database (i.e., a table of some sort).
;;

;; =============
;; PROS AND CONS
;; ============= 

;; 
;; Now, to answer the question.
;;

;;
;; The relative strengths and weaknesses of the three methods are as follows:
;;


;;
;; Segment table horizontally ..
;; Segment table vertically ..
;;

;; (1)
;; -->(a) must know about all the different types. 
;;        if you want to add a new type, you have to reprogram each method
;;    (b) naming: no two procedures can have the same name. 
;; As described in the next, it is not additive. 

;; (2) --> data-directed 
;
;; It is moved to a database,
;; This is additive. 

;;
;; A programming system is "additive" if it can be incorporated into a larger system 
;; without needing to be redesigned or reimplemented.
;;