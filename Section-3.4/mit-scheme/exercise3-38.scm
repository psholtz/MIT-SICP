;;
;; [working]
;;

;; 
;; There are 3! = 6 ways in which the transactions can be ordered:
;;

;;
;; 1. Peter, Paul, Mary
;;
(set! balance 100)                       
;; ==> 100
(set! balance (+ balance 10)            
;; ==> 110
(set! balance (- balance 20))            
;; ==> 90
(set! balance (- balance (/ balance 2))) 
;; ==> 45
 
;;
;; 2. Peter, Mary, Paul
;;
(set! balance 100)                       
;; ==> 100
(set! balance (+ balance 10))
;; ==> 110
(set! balance (- balance (/ balance 2)))
;; ==> 55
(set! balance (- balance 20))
;; ==> 35

;;
;; 3. Paul, Peter, Mary
;;
(set! balance 100)
;; ==> 100
(set! balance (- balance 20))
;; ==> 80
(set! balance (+ balance 10))
;; ==> 90
(set! balance (- balance (/ balance 2)))
;; ==> 45

;;
;; 4. Paul, Mary, Peter
;;
(set! balance 100)
;; ==> 100
(set! balance (- balance 20))
;; ==> 80
(set! balance (- balance (/ balance 2)))
;; ==> 40
(set! balance (+ balance 10))
;; ==> 50

;;
;; 5. Mary, Peter, Paul
;;
(set! balance 100)
;; ==> 100
(set! balance (- balance (/ balance 2)))
;; ==> 50
(set! balance (+ balance 10))
;; ==> 60
(set! balance (- balance 20))
;; ==> 40

;;
;; 6. Mary, Paul, Peter
;;
(set! balance 100)
;; ==> 100
(set! balance (- balance (/ balance 2)))
;; ==> 50
(set! balance (- balance 20))
;; ==> 30
(set! balance (+ balance 10))
;; ==> 40

;; 
;; So the possible values are 35, 40, 45, 50.
;;
