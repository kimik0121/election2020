* OLS (Table 2) and IV (Table A2)

clear
clear matrix
set more off
sysdir set PLUS ${plus}

use ${path}data_election.dta, clear

* log transformation
gen double lnw = ln(wealth + 1)
gen double lncr = ln(criminal_record + 1)
gen double lnr = ln(number_of_running + 1)

gen double lninc = ln(income_tax)
sum lninc
scalar m1 = r(mean)
replace lninc = lninc - m1

gen double lncovid = ln(covid_rate + 1)
sum lncovid
scalar m2 = r(mean)
replace lncovid = lncovid - m2

*reg y NA_member lnw lncr male ms sky age1 age2 age3 i.DM lncovid lninc i.DM#c.lncovid i.DM#c.lninc ///
*      i.DM#i.area i.area, vce(cluster district)

gen double inter_covid = lncovid*DM
gen double inter_inc = lninc*DM

forvalues i = 2/5 {
	gen inter_a`i' = area`i'*DM
}

****** OLS

reg y NA_member lnw lncr male ms sky age1 age2 age3 i.DM lncovid lninc inter_covid inter_inc ///
      inter_a2 inter_a3 inter_a4 inter_a5, vce(cluster district)
	  
outreg2 using ${path}estimation\logit.txt, r2 aster(se) side dec(3) replace
	  
reg y NA_member lnw lncr male ms sky age1 age2 age3 i.DM lncovid lninc inter_covid inter_inc ///
      inter_a2 inter_a3 inter_a4 inter_a5 i.area, vce(cluster district)

outreg2 using ${path}estimation\logit.txt, r2 aster(se) side dec(3) 


****** IV methods
	
* CF 
xi: probit NA_member lnw lncr male ms sky age1 age2 age3 i.DM lncovid lninc inter_covid inter_inc ///
    inter_a2 inter_a3 inter_a4 inter_a5 i.area lnr, cluster(district)
	 
outreg2 using ${path}estimation\probit.txt, side aster(se) dec(3) replace	
predict double zdelta, xb

gen double phi = normalden(zdelta)
gen double phat = normal(zdelta)
gen double imr = phi/phat
gen double phat2 = normal(-zdelta)
gen double imr2 = phi/phat2
gen double r = NA_member*imr - (1-NA_member)*imr2

xi: reg y NA_member lnw lncr male ms sky age1 age2 age3 i.DM lncovid lninc inter_covid inter_inc ///
      inter_a2 inter_a3 inter_a4 inter_a5 r i.area, vce(cluster district)
outreg2 using ${path}estimation\iv.txt, r2 aster(se) side dec(3) replace
/*
etregress y lnw lncr male ms sky age1 age2 age3 i.DM lncovid lninc inter_covid inter_inc ///
      inter_a2 inter_a3 inter_a4 inter_a5 i.area, ///
	  treat(NA_member = lnw lncr male ms sky age1 age2 age3 i.DM lncovid lninc inter_covid inter_inc ///
      inter_a2 inter_a3 inter_a4 inter_a5 i.area lnr) twostep
*/
	  
* Two-step procedure

xi: ivreg2 y lnw lncr male ms sky age1 age2 age3 i.DM lncovid lninc inter_covid inter_inc ///
    inter_a2 inter_a3 inter_a4 inter_a5 i.area (NA_member = phat), gmm2s savefirst cluster(district)

outreg2 using ${path}estimation\iv.txt, aster(se) side dec(3) 
est restore _ivreg2_NA_member
outreg2 using ${path}estimation\iv_first.txt, aster(coef) dec(3) replace

* 2SLS		 
xi: ivreg2 y lnw lncr male ms sky age1 age2 age3 i.DM lncovid lninc inter_covid inter_inc /// 
    inter_a2 inter_a3 inter_a4 inter_a5 i.area (NA_member = lnr), gmm2s savefirst cluster(district)

outreg2 using ${path}estimation\iv.txt, aster(se) side dec(3) 
est restore _ivreg2_NA_member
outreg2 using ${path}estimation\iv_first.txt, aster(coef) dec(3) 

* engogeneity test

qui: xi: ivregress 2sls y lnw lncr male ms sky age1 age2 age3 i.DM lncovid lninc inter_covid inter_inc /// 
         inter_a2 inter_a3 inter_a4 inter_a5 i.area (NA_member = phat), cluster(district)
estat endogenous

qui: xi: ivregress 2sls y lnw lncr male ms sky age1 age2 age3 i.DM lncovid lninc inter_covid inter_inc /// 
         inter_a2 inter_a3 inter_a4 inter_a5 i.area (NA_member = lnr), cluster(district)
estat endogenous



