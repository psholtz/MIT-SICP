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

(a) We'll add a smile:

<pre>
;; define the man
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
(define segments (make-segments vectors))

;; define the smile
(define p1 (make-vect 0.45 0.75))
(define p2 (make-vect 0.5 0.7))
(define p3 (make-vect 0.55 0.75))

(define s1 (make-segment p1 p2))
(define s2 (make-segment p2 p3))

(define segments (append segments (list s1)))
(define segments (append segments (list s2)))

(define wave (segments->painter segments)
(paint wave)
</pre>

![http://farm9.staticflickr.com/8482/8266809146_5eaa123dac_m.jpg](http://farm9.staticflickr.com/8482/8266809146_5eaa123dac_m.jpg)

**Wave Man Smiles**

(b) We'll change "corner-split" as suggested in the problem statement.

The definitions of "right-split" and "up-split":

<pre>
(define (right-split painter n)
 (if (= n 0)
     painter
     (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (up-split painter n)
 (if (= n 0)
     painter
     (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))
</pre>

The following is our new definition of "corner-split":

<pre>
(define (corner-split painter n)
 (if (= n 0)
     painter
     (let ((up (up-split painter (- n 1)))
           (right (right-pslit painter (- n 1))))
      (let ((top-left up)
            (bottom-right right)
            (corner (corner-split painter (- n 1))))
       (beside (below painter top-left)
               (below bottom-right corner))))))
</pre> 

![http://farm9.staticflickr.com/8485/8265740211_bbefb512e6_m.jpg](http://farm9.staticflickr.com/8485/8265740211_bbefb512e6_m.jpg) ![http://farm9.staticflickr.com/8487/8265740233_c5b53bf88f_m.jpg](http://farm9.staticflickr.com/8487/8265740233_c5b53bf88f_m.jpg) ![http://farm9.staticflickr.com/8212/8265740243_02e0b9028b_m.jpg](http://farm9.staticflickr.com/8212/8265740243_02e0b9028b_m.jpg)

(c) We want to modify the "square-limit" to work differently, say reverse the way Einstein is looking.

In the text, the necessary definitions are given as:

<pre>
(define (identity x) x)

(define (square-of-four tl tr bl br)
 (lambda (painter)
  (let ((top (beside (tl painter) (tr painter)))
        (bottom (beside (bl painter) (br painter))))
   (below bottom top))))

(define (square-limit painter n)
 (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
  (combine4 (corner-split painter n))))
</pre>

To reverse the way Einstein looks, we'll reverse the way the four procedures in "combine4" are applied:

<pre>
(define (square-limit painter n)
 (let ((combine4 (square-of-four flip-vert rotate180 identity flip-horiz)))
  (combine4 (corner-split painter n))))
</pre>

[WORKING]