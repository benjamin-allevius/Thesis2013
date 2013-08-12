Thesis2013
==========

Some of the code for my thesis titled "Forecasting Expected Shortfall: An Extreme Value Approach".
The thesis can be found here: http://www.lunduniversity.lu.se/o.o.i.s?id=24965&postid=3735573

Given time, or perhaps per request, I will write a more detailed readme, 
with some instructions on how to use the code. In the meantime, I hope the code is readable enough
to get you going --- just don't forget to alter the file paths to suit your system.
I also recommend changing the code a bit to use a fixed number of threshold exceedances k.
I ran the bootstrap algorithm for the thresholds separately and stored the results in a matrix,
which I then used in the main backtest. As such, the current code expects such a matrix.
