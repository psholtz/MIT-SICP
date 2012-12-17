Exercise 2.50
============= 

Define the transformation "flip-horiz" which flips painters horizontally, and 
transformations that rotate painters counterclockwise by 180 degrees and 270 
degrees.

Solution
-------- 

Lisp code is given as follows:

```scheme
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
```

We invoke the painters as follows:

**Normal Einstein**
```scheme
(paint einstein)
```

![http://farm9.staticflickr.com/8490/8265666635_07fc63b167_m.jpg](http://farm9.staticflickr.com/8490/8265666635_07fc63b167_m.jpg)

**Flip-Horiz Einstein**
```scheme
(paint (flip-horiz einstein))
```

![http://farm9.staticflickr.com/8204/8266735592_033f74bf7f_m.jpg](http://farm9.staticflickr.com/8204/8266735592_033f74bf7f_m.jpg)

**Rotate180 Einstein**
```scheme
(paint (rotate180 einstein))
```

![http://farm9.staticflickr.com/8214/8266735610_a8f297aa43_m.jpg](http://farm9.staticflickr.com/8214/8266735610_a8f297aa43_m.jpg)

**Rotate270 Einstein**
```scheme
(paint (rotate270 einstein))
```

![http://farm9.staticflickr.com/8356/8265666683_00d6d28a07_m.jpg](http://farm9.staticflickr.com/8356/8265666683_00d6d28a07_m.jpg)
