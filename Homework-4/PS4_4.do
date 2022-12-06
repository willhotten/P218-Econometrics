cd "/Users/willhotten/Library/CloudStorage/OneDrive-LondonBusinessSchool/Documents/PhD/Courses/Year 1/P218-Econometrics/Homework-4"

***** Preamble *****

* Load dataset
use murder, clear
	
***** 4a *****
/*
Running regression of murder rate on constant, exec, unemp and year dummies
*/

* Run specified regression
reg mrdrte exec unem d90 d93

* Create outputs table for latex
esttab using hw4_4a_output.tex, se label nonumber title("Regression of murder rate on constant, executions, unemployment and year dummies") replace

***** 4b *****
/*
Run fixed effects regression
*/

* Declare data is panel
xtset id year

* Run FE regression
xtreg mrdrte exec unem d90 d93, fe

* Create outputs table for latex
esttab using hw4_4b_output.tex, se label nonumber title("Fixed effects regression") replace

* Save coefficients and VCOV matrix
mat betafe=get(_b)
mat Vfe=get(VCE)

***** 4c *****
/*
Run dummy regressions to get residuals and then regress residuals on one another
*/

* Get residuals from regressions
quietly{
xi: reg mrdrte i.id
predict rmrdrte, residuals
label variable rmrdrte  "rmrdrte" 

xi: reg exec i.id
predict rexec, residuals
label variable rexec  "rexec" 

xi: reg unem i.id
predict runem, residuals
label variable runem  "runem" 

xi: reg d90 i.id
predict rd90, residuals
label variable rd90  "rd90" 

xi: reg d93 i.id
predict rd93, residuals
label variable rd93  "rd93" 
}

* Regress residuals on eachother
reg rmrdrte rexec runem rd90 rd93, noconstant

* Create outputs table for latex
esttab using hw4_4c_output.tex, se label nonumber title("Regression of residuals") replace

* Compare to between
xtreg mrdrte exec unem d90 d93, fe vce(conventional)

* Create outputs table for latex
esttab using hw4_4c_output2.tex, se label nonumber title("FE with vce(conventional)") replace

