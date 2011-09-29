
;; [WORKING]

;;
;; Definition given in the textbook:
;;
(define (dot-product v w)
  (accumulate + 0 (map * v w)))


;;
;; Define the support matrix operations:
;;
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))
