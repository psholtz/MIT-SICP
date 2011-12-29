;;
;; Exercise 2.24
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