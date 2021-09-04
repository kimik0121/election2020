* Counterfactual Analysis (Table 3)

clear
clear matrix
set more off
sysdir set PLUS ${plus}

scalar cf = 5 // covid factor (i) 5: OECD average, (ii) 3: Canada, (iii) 8: US 

use ${path}data_election.dta, clear

gen double lnw = ln(wealth + 1)
gen double lncr = ln(criminal_record + 1)
gen double lninc = ln(income_tax)
sum lninc
scalar m1 = r(mean)
replace lninc = lninc - m1

gen double lncovid = ln(covid_rate + 1)
sum lncovid
scalar m2 = r(mean)
replace lncovid = lncovid - m2

gen double inter_covid = lncovid*DM
gen double inter_inc = lninc*DM

forvalues i = 2/5 {
	gen inter_a`i' = area`i'*DM
}

reg y NA_member lnw lncr male ms sky age1 age2 age3 i.DM lncovid lninc inter_covid inter_inc ///
      inter_a2 inter_a3 inter_a4 inter_a5 i.area, vce(cluster district)
*predict yhat, xb

* interaction coefficient
matrix b_hat = e(b)
scalar b_covid = b_hat[1,14]	

gen double covid_rate_star = covid_rate*cf
gen double lncovid_star = ln(covid_rate_star + 1)
replace lncovid = lncovid + m2

gen double delta_star = y
replace delta_star = y - b_covid*lncovid + b_covid*lncovid_star if DM == 1

gen  double temp1 = exp(delta_star)
egen double temp2 = sum(temp1), by(district)
gen  double sj_star = temp1/(1+ temp2)
gen  double votes_star = sj_star*voter
drop temp1 temp2

****** counterfactual voting shares 
egen temp1 = sum(votes), by(electoral_district party)
egen temp2 = sum(votes_star), by(electoral_district party)
egen temp3 = sum(voter), by(electoral_district party)
egen temp4 = sum(tvotes), by(electoral_district party)

duplicates drop electoral_district party, force

replace sj = temp1/temp3
replace sj_star = temp2/temp3

forvalues i = 0/1 {
	#delimit ;
	twoway (kdensity sj if DM == `i') 
		   (kdensity sj_star if DM == `i', lpattern(dash)), 
	xtitle("Voting share", size(medium))
	ytitle("Density")
	ylabel( , nogrid)
	xlabel(0 (.1) .6, labsize(medium) nogrid format(%9.0gc))		
	plotregion(color(white)) graphregion(color(white)) note("")
	legend(label(1 "Observed") label(2 "Simulated") symxsize(10) size(medium) region(lp(blank)))
	;
	graph export ${path}tex\voting_share_`i'.pdf, replace
	; 
	#delimit cr
}	
window manage close graph


****** counterfactual election results 

keep area electoral_district DM sj sj_star
reshape wide sj sj_star, i(area electoral_district) j(DM)
gen DM_win = (sj1 - sj0 > 0)
gen DM_win_star = (sj_star1 - sj_star0 > 0)

tab area DM_win
tab area DM_win_star

