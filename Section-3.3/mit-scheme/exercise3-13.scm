;;

;; [working]

;;
;; If we were to simply evaluate an expression like (define z (list 'a 'b 'c)), 
;; the corresponding box-and-pointer diagram would look like:
;;
       --- ---       --- ---       --- --- 
z --> | * | * | --> | * | * | --> | * | / |  
       --- ---       --- ---       --- ---
        |             |             |
        v             v             v
       ---           ---           --- 
      | a |         | b |         | c |
       ---           ---           ---

;;
;; Note that the "cdr" of the last pair is nil, or "/" in the diagram.
;;

;;
;; We desire instead the box-and-pointer diagram for (define z (make-cycle (list 'a 'b 'c))), 
;; where the "make-cycle" procedure is defined as:
;;
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; 
;; The corresponding box-and-pointer diagram will look like:
;;
         --------------------------------
        |                                |
        v                                |
       --- ---       --- ---       --- ---
z --> | * | * | --> | * | * | --> | * | * |
       --- ---       --- ---       --- ---
        |             |             |
        v             v             v
       ---           ---           ---
      | a |         | b |         | c |
       ---           ---           ---

;;
;; What happens if we try to evaluate (last-pair z)? 
;;

;;
;; The "last-pair" procedure is defined as:
;;
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))