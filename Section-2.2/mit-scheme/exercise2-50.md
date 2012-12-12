Exercise 2.50
============= 

Define the transformation "flip-horiz" which flips painters horizontally, and 
transformations that rotate painters counterclockwise by 180 degrees and 270 
degrees.

Solution
-------- 

Lisp code is given as follows:

<pre>
(define (flip-horiz painter)	
  (transform-painter painter	
                     (make-vect 1.0 0.0)   ;; new origin
		     (make-vect 0.0 0.0)   ;; new end of edge 1
		     (make-vect 1.0 1.0))) ;; new end of edge 2

(define (rotate180 painter)
  (transform-painter painter
  		     (make-vect 1.0 1.0)   ;; new origin
	             (make-vect 0.0 1.0)   ;; new end of edge 1
		     (make-vect 1.0 0.0))) ;; new end of edge 2

(define (rotate270 painter)
  (transform-painter painter
  		     (make-vect 0.0 1.0)   ;; new origin
		     (make-vect 0.0 0.0)   ;; new end of edge 1
		     (make-vect 1.0 1.0))) ;; new end of edge 2
</pre>

We invoke the painters as follows:

**Normal Einstein**
<pre>
(paint einstein)
</pre>

![http://farm9.staticflickr.com/8490/8265666635_07fc63b167_m.jpg](http://farm9.staticflickr.com/8490/8265666635_07fc63b167_m.jpg)

**Flip-Horiz Einstein**
<pre>
(paint (flip-horiz einstein))
</pre>

![http://farm9.staticflickr.com/8204/8266735592_033f74bf7f_m.jpg](http://farm9.staticflickr.com/8204/8266735592_033f74bf7f_m.jpg)

**Rotate180 Einstein**
<pre>
(paint (rotate180 einstein))
</pre>

![http://farm9.staticflickr.com/8214/8266735610_a8f297aa43_m.jpg](http://farm9.staticflickr.com/8214/8266735610_a8f297aa43_m.jpg)

**Rotate270 Einstein**
<pre>
(paint (rotate270 einstein))
</pre>

![http://farm9.staticflickr.com/8356/8265666683_00d6d28a07_m.jpg](http://farm9.staticflickr.com/8356/8265666683_00d6d28a07_m.jpg)