cd "/Users/willhotten/Library/CloudStorage/OneDrive-LondonBusinessSchool/Documents/PhD/Courses/Year 1/P218-Econometrics/Homework-4"

***** Preamble *****


* Load dataset
use ccapm, clear
	
***** 2a *****
/*
Use GMM to estimate CAPM parameters using the four moment conditions
*/

* Generate lagged vars
gen rrate_L1 = rrate[_n-1]
gen cratio_L1 = cratio[_n-1]
gen er_L = e[_n-1]

* Compute GMM estimates using gmm command
gmm ({b1=1}*(cratio)^(-{b2=1})*rrate -1) (({b1=1}*(cratio)^(-{b2=1})*rrate -1)*cratio[_n-1]) (({b1=1}*(cratio)^(-{b2=1})*rrate -1)*rrate[_n-1]) (({b1=1}*(cratio)^(-{b2=1})*rrate -1)*e[_n-1]), igmm winitial(unadjusted, independent)

* Create outputs table for latex
esttab using hw4_2a_output.tex, se label nonumber title("GMM estimation of CAPM results") replace

***** 2b *****
/*
Change GMM optimal weighting matrix
*/

* Generate time variable
gen time =[_n]

* Compute GMM again but with Newey-West-type HAC estimation with truncation lag = 5
gmm ({b1=1}*(cratio)^(-{b2=1})*rrate -1) (({b1=1}*(cratio)^(-{b2=1})*rrate -1)*cratio[_n-1]) (({b1=1}*(cratio)^(-{b2=1})*rrate -1)*rrate[_n-1]) (({b1=1}*(cratio)^(-{b2=1})*rrate -1)*e[_n-1]), igmm winitial(unadjusted, independent) wmatrix(hac ba 5)

* Create outputs table for latex
esttab using hw4_2b_output.tex, se label nonumber title("GMM estimation of CAPM results - HAC SE's") replace

***** 2c *****
/*
Testing hypothesis that beta = 0.98 at 95% significance level
*/

* Hypothesis test
test [b1]_cons == 0.98
