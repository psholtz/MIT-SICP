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
w(i1) = (b-a)
w(i2) = (d-c)
</pre>

and the width of the summed interval is similarly:

<pre>
w(i1+i2) = (d+b)-(a+c) = (d-c) + (b-a) = w(i1) + w(i2)
</pre>

In other words, the width of the sum is the sum of the widths.

The difference is simply a special for of the sum, namely, adding the negative of one of the arguments, so we would expect the same arguments to carry over identicallly. 

If we wish to work out the difference by hand, we would have:

<pre>
i1 = (a,b)
i2 = (c,d)
-i2 = (-d,-c)
i1 - i2 = (a-d,b-c)
</pre>

so that:

<pre>
w(i1-i2) = (b-c) - (a-d) = (b-a) + (d-c) = w(i1) + w(i2)
</pre>

And so the width of the sum is still the sum of the widths, as we expect.

However, this is not the case for multiplication. 

Consider again, our two intervals:

<pre>
i1 = (a,b)
i2 = (c,d)
</pre>

We are only looking for a (general) counter-example, so let's simplify our analysis by presuming that 0 < a < b and 0 < c < d and a < b < c < d. We then have:

<pre>
i1 * i2 = (ac, bd)
</pre>

but in this case, we clearly have:

<pre>
w(i1 * i2) = bd - ac
</pre>

which is not a function of either (b-a) or (d-c). QED.