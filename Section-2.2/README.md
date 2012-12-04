Section 2.2
=========== 

Hierarchical Data and the Closure Property
------------------------------------------ 

Talk about djikstra and the 8-queens problem:

http://en.wikipedia.org/wiki/Eight_queens_puzzle

Functional Geometry:

http://eprints.ecs.soton.ac.uk/7577/1/funcgeo2.pdf

![http://farm9.staticflickr.com/8197/8245029656_0a5611cb7a_z.jpg](http://farm9.staticflickr.com/8197/8245029656_0a5611cb7a_z.jpg)

*Fractal paintings by M.C. Escher ("Square Limit") nicely illustrate the closure property in a vivid, geometrical way.*
(http://euler.slu.edu/escher/index.php/File:Square-limit.jpg)

Nested Mappings
--------------- 

- working -

We can define the nesting procedures:

<pre>
(define (accumulate op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
 (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
 (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
 (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (enumerate-interval low high)
 (if (> low high)
     '()
     (cons low (enumerate-interval (+ low 1) high))))
</pre>

Picture Language
---------------- 

![http://farm9.staticflickr.com/8350/8244248350_1c7e734a3e_m.jpg](http://farm9.staticflickr.com/8350/8244248350_1c7e734a3e_m.jpg)
<pre>
(define einstein2 (beside einstein (flip-vert einstein)))
(paint einstein2)
</pre>
**Combining painters by flipping one of them.**

<p>&nbsp;</p>

![http://farm9.staticflickr.com/8350/8244248376_205c30d8a6_m.jpg](http://farm9.staticflickr.com/8350/8244248376_205c30d8a6_m.jpg)
<pre>
(define einstein4 (below einstein2 einstein2))
(paint einstein4)
</pre>

<pre>
(define (flipped-pairs painter)
 (let ((painter2 (beside painter (flip-vert painter))))
  (below painter2 painter2)))
(paint (flipped-pairs einstein))
</pre>
**Combining painters by stacking them one on top of another.**
 
<p>&nbsp;</p>

![http://farm9.staticflickr.com/8210/8244992686_512e0e47e4_m.jpg](http://farm9.staticflickr.com/8210/8244992686_512e0e47e4_m.jpg) ![http://farm9.staticflickr.com/8348/8244992736_70b8b08c85_m.jpg](http://farm9.staticflickr.com/8348/8244992736_70b8b08c85_m.jpg) ![http://farm9.staticflickr.com/8203/8244992766_3fa2c74fd2_m.jpg](http://farm9.staticflickr.com/8203/8244992766_3fa2c74fd2_m.jpg)
<pre>
(define (right-split painter n)
 (if (= n 0)
  painter
  (let ((smaller (right-split painter (- n 1))))
   (beside painter (below smaller smaller)))))
</pre>
**Using the "right-split" procedure at n = 2, 3 and 4.**

<p>&nbsp;</p>

![http://farm9.staticflickr.com/8205/8245073340_a1b9ac8029_m.jpg](http://farm9.staticflickr.com/8205/8245073340_a1b9ac8029_m.jpg) ![http://farm9.staticflickr.com/8066/8245073298_b708a21e9b_m.jpg](http://farm9.staticflickr.com/8066/8245073298_b708a21e9b_m.jpg) ![http://farm9.staticflickr.com/8479/8245073270_e873644b27_m.jpg](http://farm9.staticflickr.com/8479/8245073270_e873644b27_m.jpg)
<pre>
(define (up-split painter n)
 (if (= n 0)
  painter 
  (let ((smaller (up-split painter (- n 1))))
   (below painter (beside smaller smaller)))))

(define (corner-split painter n)
 (if (= n 0)
  painter
  (let ((up (up-split painter (- n 1)))
        (right (right-split painter (- n 1))))
   (let ((top-left (beside up up))
         (bottom-right (below right right))
         (corner (corner-split painter (- n 1))))
    (beside (below painter top-left)
            (below bottom-right corner))))))
</pre>

**Using the "corner-split" procedure at n = 2, 3 and 4.**

<p>&nbsp;</p>

![http://farm9.staticflickr.com/8069/8245129272_faa0166a62_m.jpg](http://farm9.staticflickr.com/8069/8245129272_faa0166a62_m.jpg)
<pre>
(define (square-limit painter n)
 (let ((quarter (corner-split painter n)))
  (let ((half (beside (flip-horiz quarter) quarter)))
   (below (flip-vert half) half))))
</pre>

**The "square-limit" procedure applied to Einstein. Compare with the M.C. Escher painting above.**