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
;; The first way of dealing with generic operations, i.e., EXPLICIT DISPATCH, or
;; DISPATCHING ON TYPE, is a powerful way of obtaining modularity in a software
;; system. However, it suffers from at least two noticable defects:
;;
;;  1. Each generic procedure must know everything about each generic type. 
;;     That is, whenever a new type is added to the system, EVERY generic 
;;     procedure must be updated to "recognize" and "dispatch" on this type
;;     appropriately. 
;;
;;  2. Although individual sub-procedures can be designed separately, we must
;;     take steps to ensure that no naming conflicts arise among these 
;;     sub-procedures.
;;
;; As described in the text, the issue is that this way of handling generic operations
;; is not "additive"; A programming system is "additive" if it can be incorporated into 
;; a larger system without needing to be redesigned or reimplemented.
;;
;; One way to impart "additivity" into this design is to use the second generic
;; programming model, i.e., DATA-DIRECTED STYLE, where the dispatch table is abstracted
;; entirely out of the generic procedures and maintained in a separate table or 
;; "database". This model overcomes both of these weaknesses.
;;
;; When new types are added to the system, we do not need to modify either the 
;; existing sub-procedures, or the generic procedures for handling them. We simply
;; program the new sub-procedures for handling these types, and add the appropriate
;; entries in the dispatch table.
;;

;;
;; ====== 
;; ANSWER
;; ====== 
;;
;; First, we answer the question: For each of these strategies -- generic operations
;; with explicit dispatches, data-directed style and message-passing style -- describe 
;; the changes that must be made to a system in order to add new types or new operations.
;; First, we address the issue of adding new types:
;;
;; EXPLICIT DISPATCH: ADDING A NEW TYPE
;; ------------------------------------ 
;;  1. We must create new type-specific constructors and selectors.
;;  2. These type-specific constructors and selectors must be named
;;     in such a way as to avoid naming conflicts with the rest of 
;;     the system.
;;  3. The constructors must "tag" the newly constructed data with the 
;;     appropriate type-specific tag.
;;  4. Each of the generic selectors must be modified and updated to 
;;     recognize the new type and dispatch accordingly when invoked.
;;
;; DATA-DIRECTED STYLE: ADDING A NEW TYPE
;; -------------------------------------- 
;;  1. We must create new type-specific constructors and selectors.
;;  2. Names for these new type-specific constructors and selectors 
;;     must be chosen in an internally consistent and coherent way, but 
;;     we need not concern ourselves with potential naming conflicts 
;;     arising from outside the new module we are designing. 
;;  3. The constructors must "tag" the newly constructed data with the
;;     appropriate type-specific tag.
;;  4. We must install the newly created type-specific constructors and 
;;     selectors into the operations table, under the correct "operation"
;;     and "type" selectors.
;;
;; MESSAGE-PASSING STYLE: ADDING A NEW TYPE
;; ---------------------------------------- 
;;  1. We must define a new type-specific constructor for the type.
;;  2. The constructor must internally define and implement procedures for 
;;     handling any possible invocation made to it by a generic selector
;;     (i.e., define the generic selectors in its own type-specific way).
;;  3. The new constructor must be named in a globally unique way, but 
;;     the names chosen internally for handling the various generic selection
;;     operations need not be globally unique.
;;
;; Next we address the issue of adding new operations:
;;
;; EXPLICIT DISPATCH: ADDING A NEW OPERATION
;; -----------------------------------------
;;  1. For each pre-existing type, we must define a new type-specific 
;;     procedure for handling the new operation.
;;  2. The names chosen for each new procedure/type must be globally unique
;;     across the entire system.
;;  3. A new generic selector must be defined and added to the system.
;;  4. This new generic selector must be programmed to recognize and dispatch
;;     across all pre-existing types in the system appropriately.
;;
;; DATA-DIRECTED STYLE: ADDING A NEW OPERATION
;; -------------------------------------------
;;  1. A new type-specific procedure for handling the operation must 
;;     be added to the installation package for each type.
;;  2. The dispatch table must be updated so that this newly defined
;;     procedure is installed in the correct type/operation combination, 
;;     for each pre-existing type.
;;
;; MESSAGE-PASSING STYLE: ADDING A NEW OPERATION
;; --------------------------------------------- 
;;  1. For each pre-existing type, the constructor must be modified 
;;     to internally recognize the new operation and dispatch on it
;;     appropriately.
;;
;; QUESTION: which organization would be most appropriate for a system in 
;; which new types must be often added?
;;
;; The most appropriate organization for this would be message passing.
;;
;; None of the existing types, or their constructors, need to be modified
;; if we are adding a new type. Instead, all we need to do is define the 
;; constructor for the new type in a globally unique way, and implement 
;; type-specific procedures for responding to the generic selectors.
;;
;; QUESTION: which organization would be most appropriate for a system in 
;; which new operations must often be added?
;;
;; The most appropriate organization for this would be data-directed style.
;;
;; Message-passing style becomes a bit more cumbersome when we are constantly
;; adding new operations, since the dispatch routine in each pre-existing
;; constructor must be modified to handle the new operation. It's true that 
;; in the data-directed style we similarly have to update the install package
;; for each type to support the new operation, but this seems like a less 
;; cumbersome route to go than in the case of message-passing style.
;;