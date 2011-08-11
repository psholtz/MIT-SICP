Recitation 3 - Sept 12, 2007
============================ 

Recursion
--------- 

**Preliminary Notes**

(a) `(define (name arg1 arg2 ...) body)` is syntactic sugar for `(define name (lambda (arg1 arg2 ...) body))`

(b) `(cond (test consequent) (test consequent) ... (else alternative))` is an alternative to `if` when there are more than two cases. The value returned is the consequent where the first test evaluates to true (anything but #f). If no tests are true, evaluate and return the alternative, if any. The alternative `else` is optional. If a consequent is omitted, the value of the test is returned.

[Document available here](http://people.csail.mit.edu/jastr/6001/fall07/r03.pdf)