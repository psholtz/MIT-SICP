Exercise 3.10
=============

In the ```make-withdraw``` procedure, the local variable ```balance``` is created as a parameter of ```make-withdraw```. 

We could also create the local state variable explicitly, using ```let```, as follows:

```scheme
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
```

Recall from section 1.3.2 that ```let``` is simply syntactic sugar for a procedure call:

```scheme
(let ((<var> <exp>)) <body>)
```

is interpreted as an alternate syntax for 

```scheme
((lambda (<var>) <body>) <exp>)
```

Use the environment model to analyze this alternate version of ```make-withdraw```, drawing figures like the ones above to illustrate the interactions

```scheme
(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))
```

Show that the two versions of ```make-withdraw``` create objects with the same behavior. How do the environment structures differ for the two versions?

Solution
-------- 

The expression 

```scheme
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
```

can be transformed into

```scheme
(define (make-withdraw initial-amount)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
           (begin (set! balance (- balance amount))
                  balance)
           "Insufficient funds"))) initial-amount))
```

The environment structure that results upon defining ```make-withdraw``` and evaluating the expression ```(define W1 (make-withdraw 100))``` is as follows (click to enlarge):

[![](https://farm9.staticflickr.com/8596/16626009792_b755c69bf0_b.jpg)](https://farm9.staticflickr.com/8596/16626009792_b755c69bf0_b.jpg)

The ```make-withdraw``` procedure takes a single argument, ```initial-amount```. When ```(make-withdraw initial-amount)``` is evaluated, the ```initial-amount``` argument is applied to an internal lambda procedure that also takes a single argument, ```balance```, and which returns a second lambda procedure that likewise takes single argument, ```amount```.

[working]