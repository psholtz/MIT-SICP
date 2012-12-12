Exercise 2.52
============= 

Make changes to the square limit of "wave" shown in figure 2.9 by working at each of the levels 
described above. In particular:

1. Add some segments to the primitive "wave" painter of Exercise 2.49 (to add a smile, for example).
2. Change the pattern constructed by "corner-split" (for example, by using only one copy of the 
   "up-split" and "right-split" images instead of two).
3. Modify the version of "square-limit" that uses "square-of-four" so as to assemble the corners in 
   a different pattern. (For example, you might make the big Mr. Rogers look outward frmo each corner
   of the square).

Solution 
--------

Again, define the supporting procedures:

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