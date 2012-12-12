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