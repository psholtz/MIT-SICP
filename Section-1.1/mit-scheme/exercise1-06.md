Exercise 1.6
============ 

Alyssa P. Hacker doesn't see why `if` needs to be provided as a special form. "Why can't I just define it as an ordinary procedure in terms of `cond`?" she asks. Alyssa's friend Eva Lu Ator claims this can be done, and she defines a new version of `if`:

```scheme
(define (new-if predicate then-clause else-clause)
	(cond (predicate then-clause)
		(else else-clause)))
```

Eva demonstrates the program for Alyssa:

```scheme
(new-if (= 2 3) 0 5)
;; 5

(new-if (= 1 1) 0 5)
;; 0
```

Delighted, Alyssa uses `new-if` to rewrite the square-root program:

```scheme
(define (sqrt-iter guess x)
	(new-if (good-enough? guess x)
		guess
		(sqrt-iter (improve guess x) x)))
```

What happens when Alyssa attempts to use this to compute square roots? Explain.

Solution
--------

We follow the evaluation of `(sqrt 2.0)` using an applicative-order evaluation model;

```scheme
(sqrt 2.0)
(sqrt-iter 1.0 2.0)
(new-if (good-enough? 1.0 2.0) 1.0 (sqrt-iter (improve 1.0 2.0) 2.0))
```

Since `new-if` is not a special form, the interpreter will attempt to evaluate both the `(good-enough? 1.0 2.0)` operand
and the `(sqrt-iter (improve 1.0) 2.0)` operand, before applying the `new-if` procedure to these 
arguments. 

Evaluation of the `(good-enough? 1.0 2.0)` operand is straightforward enough:
```scheme
(good-enough? 1.0 2.0)
(< (abs (- (square 1.0) 2.0)) 0.001)
(< (abs (- 1.0 2.0)) 0.001)
(< (abs -1.0) 0.001)
(< 1.0 0.001)
#f
```

Evaluation of `(sqrt-iter (improve 1.0 2.0) 2.0)` proves to be more problematic:

```scheme
(sqrt-iter (improve 1.0 2.0) 2.0)
(new-if (good-enough? (improve 1.0 2.0) 2.0) 1.0 (sqrt-iter (improve (improve 1.0 2.0) 2.0) 2.0))
```

We are, in a sense, back to where we started from (only with `(improve 1.0)`, rather than `1.0` being the 
argument to the `sqrt-iter` procedure). We can see that evaluation of the arguments to `new-if`
will result in an infinite recursion, hanging the interpreter.
