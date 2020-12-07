# Hook_and_Line_Survey_Catch_Analysis
Binary catch data analyzed at the hook level using a modified stepAIC() function from the MASS R package with a sample from the posterior distribution generated using MCMClogit() in the MCMCpack R package.  

The modified stepAIC() adds a buffer to the AIC testing of the models being looped over. The new candidate model has to be better (smaller) than (AIC - buffer) of the current model for the break of the loop NOT to occur. So if the buffer = 1.95 (the default), then the new candidate model has to be almost 2 AIC units better for the AIC stepping to continue. If the buffer is significantly greater than 2 AIC units then the new model has to be a lot better than the current model for the AIC stepping to continue. Hook and Line Survey binary GLM models appear to need a value far greater than 2 AIC units to have parsimony and not have too many variables and biologically dubious interactions terms - this need is not fully understood but in practice works quite well.


        year numSites
     1  2004       74
     2  2005       90
     3  2006       92
     4  2007       99
     5  2008      120
     6  2009      118
     7  2010      121
     8  2011      111
     9  2012      121
     10 2013      120
     11 2014      120
     12 2015      115
     13 2016      121
     14 2017      120
     15 2018      122
     16 2019      122

Reference:  

Analysis of fishery-independent hook and line-based data for use in the stock assessment of bocaccio rockfish (Sebastes paucispinis):


     https://www.sciencedirect.com/science/article/pii/S0165783610002031)
