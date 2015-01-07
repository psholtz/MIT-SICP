Section 2.1
=========== 

Data Abstractions
----------------- 

In Chapter 1 we looked at computational processes and the role they play in software design. 

In this section the focus is on how to build abstractions by combining data to form compound data, thereby elevating the conceptual level at which we can design programs, increasing the modularity of our program design, and enhancing the expressive power of our programming language.

#### Rational Numbers

For example, suppose we wish to define a procedure that generates linear combinations. The procedure should be able to accept two arguments representing constants, `a` and `b`, as well as two arguments representing variables, `x` and `y`. If we know that these arguments will be represented numerically, we could define this procedure as follows:

```scheme
(define (linear-combination a b x y)
  (+ (* a x) (* b y)))
```

But it would be much more powerful to be able to express the idea that linear combinations can be formed whenever addition and multiplication are defined. In this way, we could invoke the procedure using not only real numbers, but also rational numbers, complex numbers, polynomials and other mathematical data types. The procedure would then look more like:

```scheme
(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))
```

Being able to "glue" a numerator and denominator together to form a pair, or a compound data object, allows us to express this idea while maintaining a highly modular overall program design. 

Church Numerals
--------------- 
The most mind-bending part of this section probably involves the representation of pairs and other data types, including numbers, as procedures. For example, given a successor function `succ`, which adds one, we can define the natural numbers in terms of zero and the successor function as follows:


```scheme
(succ 0)               ;; 1
(succ (succ 0))        ;; 2
(succ (succ (succ 0))) ;; 3
```

[![](http://farm7.static.flickr.com/6092/6235068644_6f4f76bba8.jpg)](http://farm7.static.flickr.com/6092/6235068644_6f4f76bba8.jpg)

More generally, the Church numeral **3** represents the action of applying any given function three times to a value.

Interval Arithmetic
------------------- 

The section concludes with an extended exercise on interval arithmetic. 

The exercise is presented in the context of designing a system that is able to arithmetically manipulate imprecise  quantities. For example, resistance values are usually known only up to some relatively wide margin of error, such as +/- 10% of the resistor's advertised value. If we wish to calculate the equivalent resistance of an entire circuit made up of such devices, we need to know how to add, substract, multiply and divide quantities whose exact  value may be known only to within some predefined tolerance.

For an interesting paper on interval arithmetic and its applications to floating point representation in a computer, see *Interval Arithmetic: from Principles to Implementation* by T. Hickey, Q. Ju and M.H. van Emden ([Download](http://fab.cba.mit.edu/classes/S62.12/docs/Hickey_interval.pdf))

[![](https://farm8.staticflickr.com/7473/15599886883_36fd873774.jpg)](https://farm8.staticflickr.com/7473/15599886883_36fd873774.jpg)