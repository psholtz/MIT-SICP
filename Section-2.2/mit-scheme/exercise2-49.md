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

**Square Painter**
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

**X-Marks-the-Spot**
<pre>
(define s5 (make-segment p1 p3))
(define s6 (make-segment p2 p4))

(define x-marks-the-spot (segments->painter (list s5 s6)))

(paint x-marks-the-spot)
</pre>

![http://farm9.staticflickr.com/8223/8266210384_61e21dfa9d_m.jpg](http://farm9.staticflickr.com/8223/8266210384_61e21dfa9d_m.jpg)

**Diamond**
<pre>
(define p5 (scale-vect 0.5 (add-vect p1 p2)))
(define p6 (scale-vect 0.5 (add-vect p2 p3)))
(define p7 (scale-vect 0.5 (add-vect p3 p4)))
(define p8 (scale-vect 0.5 (add-vect p4 p1)))

(define s7 (make-segment p1 p2))
(define s8 (make-segment p2 p3))
(define s9 (make-segment p3 p4))
(define s10 (make-segment p4 p1))

(define diamond (segments->painter (list s7 s8 s9 s10)))

(paint diamond)
</pre>

![http://farm9.staticflickr.com/8489/8266210406_8e417fc961_m.jpg](http://farm9.staticflickr.com/8489/8266210406_8e417fc961_m.jpg)

**Wave**
<pre>
(define points
  '((0.4 0.0)
    (0.5 0.33)
    (0.6 0.0)
    (0.75 0.0)
    (0.6 0.45)
    (0.99 0.15)
    (0.99 0.35)
    (0.8 0.65)
    (0.6 0.65)
    (0.65 0.8)
    (0.6 0.99)
    (0.4 0.99)
    (0.35 0.8)
    (0.4 0.65)
    (0.33 0.65)
    (0.1 0.6)
    (0.0 0.8)
    (0.0 0.6)
    (0.1 0.4)
    (0.3 0.6)
    (0.33 0.5)
    (0.25 0.0)))

(define vectors (make-vectors points))

(define segments (make-segments points))

(define wave (segments->painter segments)

(paint wave)
</pre>

![http://farm9.staticflickr.com/8219/8266210246_7195831966_m.jpg](http://farm9.staticflickr.com/8219/8266210246_7195831966_m.jpg)