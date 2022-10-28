cd "/Users/willhotten/Library/CloudStorage/OneDrive-LondonBusinessSchool/Documents/PhD/Courses/Year 1/P218 Econometrics I"

***** Preamble *****

quietly{
	* Load dataset
	use PS1, clear

	* Generating new vars
	gen lnw0 = ln(w0)
	gen exp0 = a0 - ed0 - 6
	gen exp0sq = exp0^2

	* Labelling vars
	label var w0 "Earnings (dollars)"
	label var lnw0 "Log earnings"
	label var ed0 "Education (years)"
	label var a0 "Age (years)"
	label var exp0 "Experience (years)"
	label var exp0sq "Experience squared"
	save PS1_wh, replace
}

***** 5a *****
/*
Regress log(wage) on constant, education, experience and experience squared.
Note: this uses estout package which must be installed for use.
*/

quietly{
	reg lnw0 ed0 exp0 exp0sq
	esttab using hw1_5a_output.tex, se label nonumber title("log(wage) regressed on constant, education, experience and experience squared") replace
}

***** 5b *****
/* 
Creating fitted values variable and rerunning regression
*/

quietly{
	* Rerunning regression and obtaining fitted values
	use PS1_wh, clear
	reg lnw0 ed0 exp0 exp0sq
	predict fittedvals
	save PS1_wh, replace

	* Running regression using fitted values
	reg lnw0 ed0 exp0 fittedvals
	esttab using hw1_5b_output.tex, se label nonumber title("log(wage) regressed on constant, education, experience and fitted values")replace
}

***** 5c *****
/* 
Partialling out log(wage) and experience wrt constant, educaiton and experience squared.
Regressing partialled out log(wage) on partialled out experience.
*/


quietly{
	* Partialling out log(wage):
	reg lnw0 ed0 exp0sq
	predict partial_lnw0, resid
	label var partial_lnw0 "Partialled out log(wage)"

	* Partialling out experience:
	reg exp0 ed0 exp0sq
	predict partial_exp0, resid
	label var partial_exp0 "Partialled out experience"

	* Regressing partialled log(wage) on partial experience
	reg partial_lnw0 partial_exp0
	esttab using hw1_5c_output.tex, se label nonumber title("Partialled out log(wage) regressed on partialled out experience") replace
}

***** 5d *****
/* 
Regressing log(wage) on partialled out experience.
*/

quietly{
	reg lnw0 partial_exp0
	esttab using hw1_5d_output.tex, se label nonumber title("log(wage) regressed on partialled out experience") replace
}



