Recitation 6 - Fall 2007 ([PDF](http://people.csail.mit.edu/jastr/6001/fall07/r06.pdf))
=======================================================================================

Higher Order Procedures
----------------------- 

**Special Form - Let**

<pre>(let bindings body)</pre>

Binds the given bindings for the duration of the body. The bindings are a list of (name-value) pairs. The body consists of one or more expressions which are evaluated in order and hte value of the last is returned. Let is an example of syntactic sugar:

<pre>(let ((arg1 val1) (arg2 val2)) body)</pre>

Is equivalent to:

<pre>((lambda (arg1 arg2) body) val1 val2)</pre>

**Procedures**

<pre>(map op lst)</pre> 

Apply *op* to each element of *lst* in turn and return a list of the results.

<pre>(filter pred let)</pre>

Apply the predicate *pred* to each element of *lst* and return a list of all elements for which the predicate returned true (anything other tahn #f)
