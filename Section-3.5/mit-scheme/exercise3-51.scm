;;
;; [working]
;;

;;
;; Define the supporting stream procedures:
;;
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (display-line x)
  (newline)
  (display x))

;;
;; Define the "show" procedure:
;;
(define (show x)
  (display-line x)
  x)

;;
;; We evaluate the first expression:
;;
(define x (stream-map show (stream-enumerate-interval 0 10)))

;; [work]

;;
;; We evaluate the second expression:
;;
(stream-ref x 5)

;; 
;; We evaluate the third expression:
;;
(stream-ref x 7)