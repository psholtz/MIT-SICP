Section 2.2
=========== 

Hierarchical Data and the Closure Property
------------------------------------------ 

Talk about djikstra and the 8-queens problem:

http://en.wikipedia.org/wiki/Eight_queens_puzzle

Functional Geometry:

http://eprints.ecs.soton.ac.uk/7577/1/funcgeo2.pdf

Square Limit:

http://euler.slu.edu/escher/index.php/File:Square-limit.jpg

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

Define the prime procedure:

<pre>
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) 
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (prime? n)
 (define (get-random-a)
  (+ 2 (random (- n 4))))
 (define (test a)
  (= (expmod a (- n 1) n) 1))
 (cond ((= n 2) #t)
       ((= n 3) #t)
       ((= n 4) #f)
       ((= n 5) #t)
       (else 
        (and (test (- n 1))
             (test (- n 2))
             (test (get-random-a))
             (test (get-random-a))
             (test (get-random-a))))))
</pre>

Now define the procedure itself:

<pre>
(define (prime-sum-pairs n)
 (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                 (map (lambda (j) (list i j))
                      (enumerate-iterval 1 (- i 1))))
                (enumerate-interval 1 n)))))
</pre>

We can now run the procedure:

<pre>
(prime-sum-pairs 4)
;; ==> ((2 1 3) (3 2 5) (4 1 5) (4 3 7))
</pre>

Picture Language
---------------- 

Following examples in the text:

![http://farm9.staticflickr.com/8350/8244248350_1c7e734a3e_m.jpg](http://farm9.staticflickr.com/8350/8244248350_1c7e734a3e_m.jpg)
<pre>
(define einstein2 (beside einstein (flip-vert einstein)))
(paint einstein2)
</pre>

<pre>
(define einstein4 (below einstein2 einstein2))
(paint einstein4)
</pre>