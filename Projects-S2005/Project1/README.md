Project 1 - Basebot
=================== 

Scenario
--------

As you may have noticed this past fall, a remarkable event took place -- the Boston Red Sox won the World Series for the first time in 86 years! You may also have noticed long time Boston residents (such as MIT professors) walking around in a state of bliss. Because many of these folks don't want to have to wait another 86 years for this to happen again, "Red Sox Nation" has hired us to provide some help. In particular, we are to investigate the possibility of perfecting a baseball robot ("basebot") that can accurately throw and can hit with power.

[![](http://farm7.static.flickr.com/6017/5979059285_d5c095a417.jpg)](http://farm7.static.flickr.com/6017/5979059285_d5c095a417.jpg)

Problem 1: Some Simple Physics
------------------------------ 

We are going to begin by modeling how far a baseball can travel -- the same physics will hold for boht hitting a ball and throwing a ball. We are going to simplify things by assuming that baseballs don't spin as they move (clearly false but it makes life much easier). This means we can treat the movement of the baseball as if it were restricted to a two-dimensional plane. So what happens when a baseball is hit? For the moment, we'll model a baseball as a particle that moves along a single dimension with some initial position u, some initial velocity v, and some initial acceleration a, as pictured in Figure 1 below. The equation for the position of the baseball at time t, give a, v and u, is u(t) = (1/2)at^2 + vt + u. Note that this denotes a first order differential equation in time. Later, we can apply this equation to either the horizontal (x) component of baseball motion, or the vertical (y) component of baseball motion.

<strong>[XXX]</strong>

Figure 1: Motion of a basebal in a generic direction.

Write a procedure that takes as input values for a, v, u and t, and returns as output the position of the baseball at time t:

<pre>
(define position 
  (lambda (a v u t)
     YOUR-CODE-HERE))
</pre>

Test your position code for at least the following cases:

<pre>
(position 0 0 0 0)     ;; --> 0
(position 0 0 20 0)    ;; --> 20
(position 0 5 10 10)   ;; --> 60
(position 2 2 2 2)     ;; --> ?
(position 5 5 5 5)     ;; --> ?
</pre>

The template code file `basebot.scm` will have these tests, and other test cases for other procedures, which you should run (you can add/show your output values). In addition, you should add some test cases of your own to these to cover other boundary and typical conditions.

Problem 2: Basic Math
--------------------- 

One of our goals is to determine how far a baseball will travel in the air, if it is hit with some initial velocity at some initial angle with respect to the ground. To do this, we will need to know when the baseball hits the ground, and for that we'll want to find when the y coordinate of the baseball's position reaches zero. This can be discovered by finding the roots of the y position equation, and selecting the one that is larger (later in time). The proper tool for this is the quadratic formula. Given the coefficients of the quadratic equation az^2 + bz + c = 0, write a procedure to find one of the roots (call this `root1`), and another procedure to find the other root (call this `root2`).

<pre>
(define root1
  (lambda (a b c)
    YOUR-CODE-HERE))

(define root2
  (lambda (a b c)
    YOUR-CODE-HERE))
</pre>

You may notice that, depending on how you wrote your procedures, for some test cases you get an error. For example, try `(root1 5 3 6)`. What happens? If you get an error, which is likely if you wrote your code the straightforward way, figure out how to change it so that your procedure returns a false value in those cases where there is not a valid solution.