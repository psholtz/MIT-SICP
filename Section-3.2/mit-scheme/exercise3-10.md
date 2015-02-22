Exercise 3.10
=============

In the ```make-withdraw``` procedure, the local variable ```balance``` is created as a parameter of ```make-withdraw```. We could also create the local state variable explicitly, using ```let```, as follows:

```scheme
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient bunds"))))
```

Solution
-------- 

[working]