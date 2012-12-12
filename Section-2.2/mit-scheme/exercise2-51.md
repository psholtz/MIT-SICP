Exercise 2.51
============= 

Define the "below" operation for painters. Below takes two painters as arguments. The resulting 
painter, given a frame, draws with the first painter in the bottom of the frame and with the 
second painter in the top. Define "below" in two different ways -- first by writing a procedure 
that is analogous to the "beside" procedure given above, and again in terms of "beside" and suitable 
rotation operations (from Exercise 2.50).

Solution
-------- 

The first definition of "below" is given as:

<pre>
(define (below painter1 painter2)
 (let ((split-point (make-vert 0.0 0.5)))
  (let ((paint-bottom
         (transform-painter painter1
                            (make-vect 0.0 0.0)
                            (make-vect 1.0 0.0)
                            split-point))
         (paint-top
          (transform-painter painter2
                             split-point
                             (make-vect 1.0 0.5)
                             (make-vect 0.0 1.0))))
    (lambda (frame)
     (paint-bottom frame)
     (paint-top frame)))))
</pre>

![http://farm9.staticflickr.com/8482/8266751172_6aff18737f_m.jpg](http://farm9.staticflickr.com/8482/8266751172_6aff18737f_m.jpg)

The second definition of "below" is given as:

<pre>
(define (below painter1 painter2)
 (rotate270 (beside (rotate90 painter2) (rotate90 painter1))))
</pre>

![http://farm9.staticflickr.com/8482/8266751172_6aff18737f_m.jpg](http://farm9.staticflickr.com/8482/8266751172_6aff18737f_m.jpg)

Both procedures generate the same picture.