*********Data Driven Contagion Risk Management in Low- Income 
*********Countries: Machine Learning Applications with COVID-19 in South Asia
******Code for Reproducing Results for India**********

clear all

cls

////////Import Data 

use "/Users/towhid_mahmood/Dropbox/IZA_Covid19/Codes & Data for Shonchoy et. al. (2023)/Data/India/India_data.dta", clear

/////////Creating Sub-indices 

****Urban share of population

gen urbansh_pop = pc11u_pca_tot_p/pc11_pca_tot_p //Calculating Urban Population Share
su urbansh_pop, meanonly
gen urbansh_pop_index = (urbansh_pop - r(min)) / (r(max)- r(min)) 

****Migration share of population 

su outstmigrationshare, meanonly
gen outstmigrationshare_index = (outstmigrationshare - r(min)) / (r(max)- r(min)) 

*****Informal employment share of total employed 
 
gen nonagri = 1 -  pc11_ag_main_share //Calculating employment in Non-Agri
su nonagri, meanonly
gen nonagri_index = (nonagri - r(min)) / (r(max)- r(min)) 
  
*********Beds per million 

gen dlhs_gov_beds_m = (dlhs_gov_beds/pc11_pca_tot_p)*1000000
su dlhs_gov_beds_m, meanonly
gen dlhs_gov_beds_m_index = (dlhs_gov_beds_m - r(min)) / (r(max)- r(min)) 
gen beds_index = 1- dlhs_gov_beds_m_index ////Inversing for consistencey  

/////Generating index with arithmetic mean

gen CR_index_ind = (beds_index +  urbansh_pop_index + outstmigrationshare_index + nonagri_index)/4

label var CR_index_ind "Main Index"

//////Calculating daily cases and death

******There was no daily cases or death data for India. We had to calculate these from cumulative data.

*************Cases 

bys lgd_district_name (date_fmt): gen cases_daily = (total_cases - total_cases[_n-1])  
replace cases_daily=0 if cases_daily<0  //Anomalies like negative cases were converted to zero. These anomalies comprised of 0.79% of the total number of days.

bys lgd_district_name (date_fmt): gen weekly_cases= (total_cases) - total_cases[_n-7]
replace weekly_cases=0 if weekly_cases<0

*************Deaths

bys lgd_district_id (date_fmt): gen deaths_daily = total_deaths - total_deaths[_n-1]
replace deaths_daily=0 if deaths_daily<0 //Anomalies like negative cases were converted to zero. These anomalies comprised of 0.13% of the total number of days.


bys lgd_district_name (date_fmt): gen weekly_deaths= (total_deaths) - total_deaths[_n-7]
replace weekly_deaths=0 if weekly_deaths<0


///Creating IHS (inverse hyperbolic sine transformation) Log Variables 

gen lcases_daily = log(cases_daily+(((cases_daily^2)+1)^(1/2)))
gen ldeaths_daily = log(deaths_daily+(((deaths_daily^2)+1)^(1/2)))

gen lcu_cases=log(total_cases+(((total_cases^2)+1)^(1/2)))
gen lcu_deaths=log(total_deaths+(((total_deaths^2)+1)^(1/2)))

gen lcases_weekly = log(weekly_cases+(((weekly_cases^2)+1)^(1/2)))
gen ldeaths_weekly = log(weekly_deaths+(((weekly_deaths^2)+1)^(1/2)))

//////////Weekly Var 

*****Creating Week Indicator for individual regressions. Use this to create individual graphs for each weekly regression. 

gen week=.

local i 21979	21986	21993	22000	22007	22014	22021	22028	22035	22042	22049	22056	22063	22070	22077	22084	22091	22098	22105	22112	22119	22126	22133	22140	22147	22154	22161	22168	22175	22182	22189	22196	22203	22210	22217	22224	22231	22238	22245	22252	22259	22266	22273	22280	22287	22294	22301	22308	22315	22322	22329	22336	22343	22350	22357	22364	22371	22378	22385	22392	22399	22406	22413	22420	22427	22434	22441	22448	22455	22462	22469	22476	22483	22490	22497	22504	22511	22518	22525	22532	22539	22546	22553	22560	22567	22574	22581
foreach y in `i'{
replace  week=1 if date_fmt==`y'
}

gen date_week = date_fmt if week==1


/////////Regression

********OLS with time trend 

qui reg lcases_daily CR_index_ind i.date_fmt, r 
est table, star(.05 .01 .001) keep(CR_index_ind)

qui reg ldeaths_daily CR_index_ind i.date_fmt, r 
est table, star(.05 .01 .001) keep(CR_index_ind)

********Time Fixed Effect Model with district control 

qui areg lcases_daily CR_index_ind pc11_pca_tot_p, r absorb(date_fmt) 
est table, star(.05 .01 .001) keep(CR_index_ind)

qui areg ldeaths_daily CR_index_ind pc11_pca_tot_p, r absorb(date_fmt) 
est table, star(.05 .01 .001) keep(CR_index_ind)

 
////////OLS - Weekly Total Cases 

********Creating graph for regression with weekly cases

gen beta=.
gen lower=.
gen upper=.

local i 21979	21986	21993	22000	22007	22014	22021	22028	22035	22042	22049	22056	22063	22070	22077	22084	22091	22098	22105	22112	22119	22126	22133	22140	22147	22154	22161	22168	22175	22182	22189	22196	22203	22210	22217	22224	22231	22238	22245	22252	22259	22266	22273	22280	22287	22294	22301	22308	22315	22322	22329	22336	22343	22350	22357	22364	22371	22378	22385	22392	22399	22406	22413	22420	22427	22434	22441	22448	22455	22462	22469	22476	22483	22490	22497	22504	22511	22518	22525	22532	22539	22546	22553	22560	22567	22574	22581
foreach y in `i'{
capture reg lcases_weekly CR_index_ind  if date_week == `y', r
capture replace beta = _b[CR_index_ind] if date_week == `y'
capture replace lower = _b[CR_index_ind] - invttail(e(df_r),0.025)*_se[CR_index_ind] if date_week == `y'
capture replace upper =_b[CR_index_ind] + invttail(e(df_r),0.025)*_se[CR_index_ind] if date_week == `y'
}

twoway rcap  upper lower date_week, lstyle(ci) ylabel(15(5)-10)||   ///
       scatter beta date_week, mstyle(p1)                       ///
       legend(off) xtitle("Date") title("(b1)" "Regression coefficients with" "total cases and 95% confidence interval by week" "India") note("source: authors' calculation")                   ///
	    ytitle("Regression Coefficient") ///
		xlabel(21979 "March 5, 2020" 22028 "Apr 23, 2020" 22140 "Aug 13, 2020" 22252 "Dec 3,2020" 22308 "Jan 28, 2021" 22203 "Oct 15, 2020" 22378 "Apr 8, 2021" 22084 "June 18, 2020" 22438 "June 7, 2021" 22498 "Aug 6, 2021" 22558 "Oct 5, 2021" , angle(vertical)) graphregion(color(white))
		
graph display, xsize(6)


graph save weekly_cases.gph, replace 

//////////////OLS - Weekly (Cumulative Cases)

********Creating graph for regression with weekly cumulative cases

gen beta_cum=.
gen lower_cum=.
gen upper_cum=.

local i 21979	21986	21993	22000	22007	22014	22021	22028	22035	22042	22049	22056	22063	22070	22077	22084	22091	22098	22105	22112	22119	22126	22133	22140	22147	22154	22161	22168	22175	22182	22189	22196	22203	22210	22217	22224	22231	22238	22245	22252	22259	22266	22273	22280	22287	22294	22301	22308	22315	22322	22329	22336	22343	22350	22357	22364	22371	22378	22385	22392	22399	22406	22413	22420	22427	22434	22441	22448	22455	22462	22469	22476	22483	22490	22497	22504	22511	22518	22525	22532	22539	22546	22553	22560	22567	22574	22581
foreach y in `i'{
capture reg lcu_cases CR_index_ind  if date_week == `y', r
capture replace beta_cum = _b[CR_index_ind] if date_week == `y'
capture replace lower_cum = _b[CR_index_ind] - invttail(e(df_r),0.025)*_se[CR_index_ind] if date_week == `y'
capture replace upper_cum =_b[CR_index_ind] + invttail(e(df_r),0.025)*_se[CR_index_ind] if date_week == `y'
}

twoway rcap  upper_cum lower_cum date_week, lstyle(ci) ylabel(15(5)-10)||   ///
       scatter beta_cum date_week, mstyle(p1)                       ///
       legend(off) xtitle("Date") title("(b2)" "Regression coefficients with" "cumulative cases and 95% confidence interval by week" "India") note("source: authors' calculation")                   ///
	    ytitle("Regression Coefficient") ///
		xlabel(21979 "March 5, 2020" 22028 "Apr 23, 2020" 22140 "Aug 13, 2020" 22252 "Dec 3,2020" 22308 "Jan 28, 2021" 22203 "Oct 15, 2020" 22378 "Apr 8, 2021" 22084 "June 18, 2020" 22438 "June 7, 2021" 22498 "Aug 6, 2021" 22558 "Oct 5, 2021" , angle(vertical)) graphregion(color(white))
		
graph display, xsize(6)

graph save weekly_cases_cum.gph, replace 

		
//////////////OLS - Weekly (Total Death)

********Creating graph for regression with weekly deaths

gen beta_death=.
gen lower_death=.
gen upper_death=.

local i 21979	21986	21993	22000	22007	22014	22021	22028	22035	22042	22049	22056	22063	22070	22077	22084	22091	22098	22105	22112	22119	22126	22133	22140	22147	22154	22161	22168	22175	22182	22189	22196	22203	22210	22217	22224	22231	22238	22245	22252	22259	22266	22273	22280	22287	22294	22301	22308	22315	22322	22329	22336	22343	22350	22357	22364	22371	22378	22385	22392	22399	22406	22413	22420	22427	22434	22441	22448	22455	22462	22469	22476	22483	22490	22497	22504	22511	22518	22525	22532	22539	22546	22553	22560	22567	22574	22581
foreach y in `i'{
capture reg lcases_weekly CR_index_ind if date_week == `y', r
capture replace beta_death = _b[CR_index_ind] if date_week == `y'
capture replace lower_death = _b[CR_index_ind] - invttail(e(df_r),0.025)*_se[CR_index_ind] if date_week == `y'
capture replace upper_death =_b[CR_index_ind] + invttail(e(df_r),0.025)*_se[CR_index_ind] if date_week == `y'
}

twoway rcap  upper_death lower_death date_week, lstyle(ci) ylabel(15(5)-5)||   ///
       scatter beta_death date_week, mstyle(p1)                       ///
       legend(off) xtitle("Date") title("(a1)" "Regression coefficients with" "total death and 95% confidence interval by week" "India") note("source: authors' calculation")                   ///
	     ytitle("Regression Coefficient") ///
		 xlabel(21979 "March 5, 2020" 22028 "Apr 23, 2020" 22140 "Aug 13, 2020" 22252 "Dec 3,2020" 22308 "Jan 28, 2021" 22203 "Oct 15, 2020" 22378 "Apr 8, 2021" 22084 "June 18, 2020" 22438 "June 7, 2021" 22498 "Aug 6, 2021" 22558 "Oct 5, 2021" , angle(vertical)) graphregion(color(white))
		
graph display, xsize(6)

graph save weekly_deaths.gph, replace
		
		
//////////////OLS - Weekly (Cumulative Death)

********Creating graph for regression weekly cumulative deaths

gen beta_death_cum=.
gen lower_death_cum=.
gen upper_death_cum=.

local i 21979	21986	21993	22000	22007	22014	22021	22028	22035	22042	22049	22056	22063	22070	22077	22084	22091	22098	22105	22112	22119	22126	22133	22140	22147	22154	22161	22168	22175	22182	22189	22196	22203	22210	22217	22224	22231	22238	22245	22252	22259	22266	22273	22280	22287	22294	22301	22308	22315	22322	22329	22336	22343	22350	22357	22364	22371	22378	22385	22392	22399	22406	22413	22420	22427	22434	22441	22448	22455	22462	22469	22476	22483	22490	22497	22504	22511	22518	22525	22532	22539	22546	22553	22560	22567	22574	22581
foreach y in `i'{
capture reg lcu_deaths CR_index_ind if date_week == `y', r
capture replace beta_death_cum = _b[CR_index_ind] if date_week == `y'
capture replace lower_death_cum = _b[CR_index_ind] - invttail(e(df_r),0.025)*_se[CR_index_ind] if date_week == `y'
capture replace upper_death_cum =_b[CR_index_ind] + invttail(e(df_r),0.025)*_se[CR_index_ind] if date_week == `y'
}

twoway rcap  upper_death_cum lower_death_cum date_week, lstyle(ci) ylabel(15(5)-5)||   ///
       scatter beta_death_cum date_week, mstyle(p1)                       ///
       legend(off) xtitle("Date") title("(a2)" "Regression coefficients with" "cumulative death and 95% confidence interval by week" "India") note("source: authors' calculation")                   ///
	     ytitle("Regression Coefficient") ///
		 xlabel(21979 "March 5, 2020" 22028 "Apr 23, 2020" 22140 "Aug 13, 2020" 22252 "Dec 3,2020" 22308 "Jan 28, 2021" 22203 "Oct 15, 2020" 22378 "Apr 8, 2021" 22084 "June 18, 2020" 22438 "June 7, 2021" 22498 "Aug 6, 2021" 22558 "Oct 5, 2021" , angle(vertical)) graphregion(color(white))
		
graph display, xsize(6)

graph save weekly_deaths_cum.gph, replace		
				
//////////////Combine regression graphs for cases 

graph combine "weekly_cases" "weekly_cases_cum", row(1) graphregion(color(white))
graph display, ysize(2) 

//////////////Combine regression graphs for death

graph combine "weekly_deaths" "weekly_deaths_cum", row(1) graphregion(color(white))
graph display, ysize(2) 


/////////////Create Map and Zoning Graph 

keep if date_fmt==22584 //"31oct2021" Keeping cases data for latest date 
 

gen uid= _n


save "/Users/towhid_mahmood/Dropbox/IZA_Covid19/Codes & Data for Shonchoy et. al. (2023)/Data/India/Ind_Map_Data.dta", replace


******Shape files and the pre-processed data should be saved in the same folder 


cd "/Users/towhid_mahmood/Dropbox/IZA_Covid19/Codes & Data for Shonchoy et. al. (2023)/Data/India"

*******Processing data for superimposing cases on the map
*******For cases

use "indcoord", clear
rename _ID id
merge m:m id using inddb
drop if _merge==1
sort distname
egen uid=group(distname)
drop _merge
merge m:m uid using Ind_Map_Data.dta


gen labtype=1
collapse _X _Y labtype, by(distname total_cases)

gen by_var=1 if total_cases<2624 | total_cases==2624
replace by_var=2 if total_cases>2624 |total_cases==5787
replace by_var=3 if total_cases>5787 | total_cases==14765
replace by_var=4 if total_cases>14765 | total_cases==39694
replace by_var=5 if total_cases>39694
save "indcoord_cases.dta", replace 

*******For deaths

use "indcoord", clear
rename _ID id
merge m:m id using inddb
drop if _merge==1
sort distname
egen uid=group(distname)
drop _merge
merge m:m uid using Ind_Map_Data.dta


gen labtype=1
collapse _X _Y labtype, by(distname total_deaths)

gen by_var=1 if total_deaths<88 | total_deaths==88
replace by_var=2 if total_deaths>88 |total_deaths==212
replace by_var=3 if total_deaths>212 | total_deaths==576
replace by_var=4 if total_deaths>579 | total_deaths==1408
replace by_var=5 if total_deaths>1408
save "indcoord_deaths.dta", replace


********Merging shape file with CR-index Data

use inddb, clear
describe
rename distname lgd_district_name
sort lgd_district_name
gen uid=_n
sort uid
merge 1:1 uid using Ind_Map_Data


*****************CR_index_ind Map*****************************

spmap CR_index_ind using indcoord, clmethod(custom) clbreaks(0 .32 .55 1) ///
clnumber(4) id(id) fcolor(green*.4 orange*.4 red*.5) ///
		legenda(on) legend(size(small)) legtitle("Zoning by Contagion Risk") legstyle(2) ///
		legend(pos(7) row(6) ring(1)) ///
		title("(b1)" "COVID - 19 contagion risk index by district" "India" , size(*.75)) note("source: authors' calculation") plotregion(icolor(white))  

graph save without_cases.gph, replace 


**********Superimposing Actual Cases on CR_index_ind************

spmap CR_index_ind using indcoord, clmethod(custom) clbreaks(0 .32 .55 1) ///
clnumber(4) id(id) osize(none none none none) fcolor(green*.4 orange*.4 red*.4) ///
point(data("indcoord_cases.dta") xcoord(_X) ycoord(_Y)  proportional(total_cases) size(*0.4) fcolor(red%30)  legenda(on) ///
legtitle(" " "Size of Points indicate") leglabel("Number infected") ) ///
		legenda(on) legend(size(small)) legtitle("Zoning by Contagion Risk") legstyle(2) ///
		legend(pos(7) row(6) ring(1)) ///
		title("(b2)" "COVID - 19 contagion risk index by district"  ///
		"superimposing COVID-19 cumulative cases" "(October 31, 2021)" "India", size(*.75)) note("source: authors' calculation")   plotregion(icolor(white))
graph save with_superimposed.gph, replace 

***************Histogram for Zones***********

twoway (hist CR_index_ind, freq fcolor(green*.4)lcolor(black) start(0) title("(b3)" "Distribution of index scores and zoning" "India", size(*.75)) note("source: authors' calculation") width(0.01)legend(off)xtitle("Index Score", axis(1)) ytitle("Number of Districts", axis(1)) ylabel(0(10)50) xlabel(0(.1).8) graphregion(color(white))) ///
(hist CR_index_ind if CR_index_ind>0,  freq fcolor(green*.4) lcolor(black) start(0.1) width(0.01)legend(off)) ///
(hist CR_index_ind if CR_index_ind>.32, freq fcolor(orange*.4) lcolor(black) start(0.1) width(0.01)legend(off)) ///
(hist CR_index_ind if CR_index_ind==.32, freq fcolor(green*.4) lcolor(black) start(0.1) width(0.01)legend(off)) ///
(hist CR_index_ind if CR_index_ind > 0.55,  freq fcolor(red*.5) lcolor(black) start(0.1) width(0.01)legend(off)) ///
(kdensity CR_index_ind, kernel(gaussian) range(0 .8) lpattern(shortdash) lcolor(black) lwidth(thick))

graph save index_a_distribution.gph, replace 


///////Combine 

graph combine "without_cases" "with_superimposed" "index_a_distribution", row(1) graphregion(color(white))
graph display, ysize(2)

**********Superimposing Actual Deaths on CR_index_ind************

spmap CR_index_ind using indcoord, clmethod(custom) clbreaks(0 .32 .55 1) ///
clnumber(4) id(id) osize(none none none none) fcolor(green*.4 orange*.4 red*.4) ///
point(data("indcoord_deaths.dta") xcoord(_X) ycoord(_Y)  proportional(total_deaths) size(*0.4) fcolor(red%30)  legenda(on) ///
legtitle(" " "Size of Points indicate") leglabel("Number infected") ) ///
		legenda(on) legend(size(small)) legtitle("Zoning by Contagion Risk") legstyle(2) ///
		legend(pos(7) row(6) ring(1)) ///
		title("(a1)" "COVID - 19 contagion risk index by district"  ///
		"superimposing COVID-19 cumulative deaths" "(October 31, 2021)" "India", size(*.75)) note("source: authors' calculation")   plotregion(icolor(white))

	graph save "/Users/towhid_mahmood/Dropbox/IZA_Covid19/Codes & Data for Shonchoy et. al. (2023)/Data/Pakistan/india_with_superimposed_death.gph", replace 

*****Saving this in the Pakistan folder to merge 


*******Percentage
gsort -CR_index_ind
keep lgd_district_name total_cases CR_index_ind
drop if total_cases==0
egen cases=total(total_cases)
gen pct_tot=(total_cases/cases)*100
gsort -pct_tot


