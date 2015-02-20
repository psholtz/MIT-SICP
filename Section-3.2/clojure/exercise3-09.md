Exercise 3.9
============ 

In section 1.2.1 we used the substitution model to analyze two procedures for computing factorials, a resursive version 

```scheme
(define (factorial n)
  (if (= n 1)
      1 
      (* n (factorial (- n 1)))))
``` 

and an iterative version

```scheme
(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
```

Show the environment structures created by evaluating ```(factorial 6)``` using each version of the ```factorial``` procedure.

Solution
-------- 

Environment structures created by evaluating ```(factorial 6)``` using the recursive version (click to enlarge):

[![](https://farm8.staticflickr.com/7292/16587363605_51315f34c4_o.png)](https://farm8.staticflickr.com/7292/16587363605_51315f34c4_o.png)

Environment structures created by evaluating ```(factorial 6)``` using the iterative version (click to enlarge):

[![](https://farm8.staticflickr.com/7281/16400908260_e33af4cc2d_o.png)](https://farm8.staticflickr.com/7281/16400908260_e33af4cc2d_o.png)
