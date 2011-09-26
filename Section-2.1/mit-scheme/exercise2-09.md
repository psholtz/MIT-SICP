Exercise 2.9
============ 

The "width" of an interval is half of the difference between its upper and lower bounds. The width is a measure of the uncertainty of the number specified by the interval. For some arithmetic operations the width of the result of combining two interval is a function only of the widths of the argument intervals, whereas for others the width of the combination is not a function of the widths of the argument intervals. Show that the width of the sum (or difference) of two intervals is a function only of the widths of the intervals being added (or subtracted). Give examples to show that this is not true for multiplication or division.

Solution
-------- 

We would first like to show that the width of the sum or difference of two intervals is a function of the widths of the argument intervals themselves.

Consider two intervals:

<pre>
i1 = (a,b)
i2 = (c,d)
</pre>

These two intervals sum to:

<pre>
i1 + i2 = (a+c,b+d)
</pre>

For the widths of the intervals we have:

<pre>
w1 = (b-a)
w2 = (d-c)
</pre>