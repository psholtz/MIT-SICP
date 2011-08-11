Recitation 3 - Sept 12, 2007
============================ 

Recursion
--------- 

**Preliminary Notes**

(a) `(define (name arg1 arg2 ...) body)` is syntactic sugar for `(define name (lambda (arg1 arg2 ...) body))`

(b) `(cond (test consequent) (test consequent) ... (else alternative))` is an alternative to `if` when there are more than two cases. The value returned is the consequent where the first test evaluates to true (anything but #f). If no tests are true, evaluate and return the alternative, if any. The alternative `else` is optional. If a consequent is omitted, the value of the test is returned.

Exercises
--------- 

1. Consider the following definitions:

<pre>
(define (our-display x)
        (display x)      ;; this prints x to the console
        x)               ;; this returns x as the value

(define (count1 x)
  (cond ((= x 0) 0)
        (else (our-display x)
              (count1 (- x 1)))))

(define (count2 x)
  (cond ((= x 0) 0)
        (else (count2 (- x 1))
              (our-display x))))
</pre>

What will `(count1 4)` and `(count2 4)` display?

2. Write a procedure `fact` that computes the factorial of a number n.

3. Write a procedure that computes `e`.

4. Write an iterative procedure that computes `e`.

5. Write a procedure `fib` that computes the n-th Fibonacci number.

6. Write a procedure that computes the golden ratio, phi.

[Recitation 3 PDF](http://people.csail.mit.edu/jastr/6001/fall07/r03.pdf)