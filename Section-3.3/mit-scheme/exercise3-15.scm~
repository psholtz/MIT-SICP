;;
;; Exercise 3.15
;;

;;
;; The definitions we will use in this exercise:
;;
(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

;;
;; Before invoking "set-to-wow!" the z1 (and x) data structures look like:
;;


        --- --- 
z1 --> | * | * |
        --- --- 
         |   |
         v   v
        --- ---       --- --- 
x -->  | * | * | --> | * | / |
        --- ---       --- ---
         |             |
         v             v 
        ---           --- 
       | a |         | b |
        ---           --- 

;;
;; Invoking "set-to-wow!" locates the "car" of the argument data structure, 
;; and mutates its value to "wow":
;;
        --- --- 
z1 --> | * | * |
        --- --- 
         |   |
         v   v
        --- ---       --- --- 
x -->  | * | * | --> | * | / |
        --- ---       --- ---
         |______       |
                |      |
                v      v 
        ---   -----   --- 
       | a | | wow | | b |
        ---   -----   --- 

;;
;; Note that invoking "set-to-wow!" updates both "z1" and "x":
;;
(set-to-wow! z1)
;; ==> ((wow b) wow b)
z1
;; ==> ((wow b) wow b)
x 
;; ==> (wow b)

;;
;; Before invoking the "set-to-wow!" procedure, the z2 data structure looks like:
;;
        --- ---       --- ---       --- --- 
z2 --> | * | * | --> | * | * | --> | * | / |
        --- ---       --- ---       --- --- 
         |             |             |
         |             v             v
         |            ---           --- 
         |           | a |         | b | 
         |            ---           --- 
         |             ^             ^
         |             |             |
         |            --- ---       --- --- 
          ---------> | * | * | --> | * | / |
                      --- ---       --- ---

;;
;; After invoking "set-to-wow!" on z2, the data structure looks like:
;;
        --- ---       --- ---       --- --- 
z2 --> | * | * | --> | * | * | --> | * | / |
        --- ---       --- ---       --- --- 
         |             |             |
         |             v             v
         |            ---           --- 
         |           | a |         | b | 
         |            ---           --- 
         |                           ^
         |                           |
         |            --- ---       --- --- 
          ---------> | * | * | --> | * | / |
                      --- ---       --- ---
                       | 
                       v
                     ----- 
                    | wow |
                     -----