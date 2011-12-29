;;
;; Exercise 2.24
;;
;; Suppose we evaluate the expression (list 1 (list 2 (list 3 4))). Give the result printed
;; by the interpreter, the corresponding box-and-pointer structure, and the interpretation
;; of this as a tree.
;;

;;
;; Typing in (list 1 (list 2 (list 3 4))) into the interpreter will result in:
;;
(list 1 (list 2 (list 3 4)))
;; ==> (1 (2 (3 4)))

;;
;; This is equivalent to the following sequence of "cons" constructs:
;;
(cons 1 (cons (cons 2 (cons (cons 3 (cons 4 '())) '())) '()))

     --- ---       --- --- 
--> | * | * | --> | * | / |
     --- ---       --- --- 
      |             | 
      v             v 
     ---           --- ---      --- --- 
    | 1 |         | * | * | -> | * | / |
     ---           --- ---      --- --- 
                    |            |
                    v            V
                   ---          --- ---      --- --- 
                  | 2 |        | * | * | -> | * | / |
                   ---          --- ---      --- --- 
                                 |            |
                                 v            v
                                ---          ---
                               | 3 |        | 4 |
                                ---          ---

;;
;; The structure represented as a tree:
;;

       (1 (2 (3 4)))
          /   \ 
         /     \          
        /       \
       /         \ 
      1       (2 (3 4))
               /   \ 
              /     \ 
             2     (3 4)
                    / \
                   /   \
                  3     4  