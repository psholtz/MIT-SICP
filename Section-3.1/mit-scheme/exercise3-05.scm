;; [working]
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 number)
  (define (trial)
    (let ((x (random-in-range x1 x2))
	  (y (random-in-range y1 y2)))
      (if (p x y)
	  1
	  0)))
  (define (iter n t)
    (if (= n number)
	t
	(iter (+ n 1) (+ t (trial)))))
  
  (let ((fraction (/ (* (iter 0 0) 1.0) (* number 1.0)))
	(area (* (- x2 x1) (- y2 y1))))
    (* area fraction)))


  ;;(let ((fraction (/ (iter 0 0) number))
;;	(area (- x2 x1) (- y2 y1)))
  ;;  (* fraction area)))

(define x1 0)
(define x2 10)
(define y1 0)
(define y2 10)
(define number 100)

(define unit-predicate (lambda (x y) #t))
(define unit-circle-predicate 
  (lambda (x y)
    (let ((radius (sqrt (+ (* x x) (* y y)))))
      (< radius 1))))

