Exercise 2.49
============= 

Use segments->painter to define the following primitive painters:

1. The painter that draws the outline of the designated frame.
2. The painter that draws an "X" by connecting opposite corners of the frame.
3. The painter that draws a diamond shape by connecting the midpoints of the sides of the frames.
4. The "wave" painter.

Solution
--------- 

It's useful to have two helper procedures defined:

<pre>
(define (make-vectors points)
 (map (lambda (p)
       (let ((x (car p))
             (y (cadr p)))
         (make-vect x y)))
      points))

(define (make-segments vectors)
 (define (make-segments-iter working total)
  (if (null? (cdr working))
      (append total (list (make-segment (car working) (car (car total)))))
      (let ((one (car working))
            (two (cadr working)))
       (make-segments-iter (cdr working) (append total (list (make-segment one two)))))))
 (make-segments-iter vectors '()))
</pre>

With this, we can define our painters as follows:

<pre>
(define p1 (make-vect 0 0))
(define p2 (make-vect 1 0))
(define p3 (make-vect 1 1))
(define p4 (make-vect 0 1))

(define s1 (make-segment p1 p2))
(define s2 (make-segment p2 p3))
(define s3 (make-segment p3 p4))
(define s4 (make-segment p4 p1))

(define square (segments->painter (list s1 s2 s3 s4)))

(paint square)
</pre>

![http://farm9.staticflickr.com/8484/8266210282_f5b6744780_m.jpg](http://farm9.staticflickr.com/8484/8266210282_f5b6744780_m.jpg)

**Square Painter**