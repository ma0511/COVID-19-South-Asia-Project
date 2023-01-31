*********Data Driven Contagion Risk Management in Low- Income 
*********Countries: Machine Learning Applications with COVID-19 in South Asia
******Code for Reproducing Results for Pakistan**********

clear all

cls

////////Import Data 

use "/Users/towhid_mahmood/Dropbox/IZA_Covid19/Codes & Data for Shonchoy et. al. (2023)/Data/Pakistan/Pakistan_data.dta", clear

/////////Creating Sub-indices 

****Urban share of population

su urb_prop_pct, meanonly 
gen urb_prop_pct_index = (urb_prop_pct - r(min)) / (r(max)- r(min))

****Migration share of population 

su intra_migra_pct_pop, meanonly 
gen intra_migra_pct_pop_index = (intra_migra_pct_pop - r(min)) / (r(max)- r(min))

*****Informal employment share of total employed 

su inf_emp_share, meanonly 
gen inf_emp_share_index = (inf_emp_share - r(min)) / (r(max)- r(min))

*********Health Care Facility (As a proxy for beds per )
 
gen health_fac_sel_m_inv=1 - health_fac_sel_m //Inversing for consistency 
su health_fac_sel_m_inv, meanonly
gen health_fac_m = (health_fac_sel_m_inv - r(min)) / (r(max)- r(min))

  
/////Generating index with arithmetic mean

gen  CR_index_pak= (intra_migra_pct_pop_index + urb_prop_pct_index + health_fac_m + inf_emp_share_index)/4
    
label var  CR_index_pak "Main Index"
 
******Generating Cumulative Data

************Cases 
 
bys id (date_fmt): gen cum_cases=sum(daily_cases)
 
bys id (date_fmt): gen weekly_cases= cum_cases - cum_cases[_n-7]
 
************Deaths 

bys id (date_fmt): gen cum_deaths=sum(daily_death)

bys id (date_fmt): gen weekly_deaths= cum_deaths - cum_deaths[_n-7]
 
///Creating IHS (inverse hyperbolic sine transformation) Log Variables 

gen lcases = log(daily_cases+(((daily_cases^2)+1)^(1/2)))
gen ldeaths = log(daily_death+(((daily_death^2)+1)^(1/2)))

gen lcases_weekly = log(weekly_cases+(((weekly_cases^2)+1)^(1/2)))
gen ldeaths_weekly = log(weekly_deaths+(((weekly_deaths^2)+1)^(1/2)))

gen lncumul_cases= log(cum_cases+(((cum_cases^2)+1)^(1/2)))
gen lncumul_deaths= log(cum_deaths+(((cum_deaths^2)+1)^(1/2)))

//////////Weekly Var 

*****Creating Week Indicator for individual regressions. Use this to create individual graphs for each weekly regression. 

gen week=.

local i 22159	22166	22173	22180	22187	22194	22201	22208	22215	22222	22229	22236	22243	22250	22257	22264	22271	22278	22285	22292	22299	22306	22313	22320	22327	22334	22341	22348	22355	22362	22369	22376	22383	22390	22397	22404	22411	22418	22425	22432	22439	22446	22453	22460	22467	22474	22481	22488	22495	22502	22509	22516	22523	22530	22537	22544	22551	22558	22565	22572	22579	22586	22593	22600
foreach y in `i'{
replace  week=1 if date_fmt==`y'
}

gen date_week = date_fmt if week==1

/////////Regression

********OLS with time trend

qui reg lcases CR_index_pak i.date_fmt, r 
est table, star(.05 .01 .001) keep(CR_index_pak)

qui reg ldeaths CR_index_pak i.date_fmt, r 
est table, star(.05 .01 .001) keep(CR_index_pak)

********Time Fixed Effect Model with district control 

qui areg lcases CR_index_pak total_pop, r absorb(date_fmt) 
est table, star(.05 .01 .001) keep(CR_index_pak)

qui areg ldeaths CR_index_pak total_pop, r absorb(date_fmt) 
est table, star(.05 .01 .001) keep(CR_index_pak)

////////OLS - Graphs - Weekly Total Cases 

********Creating graph for regression with weekly cases

gen beta_weekly_cases=.
gen lower_weekly_cases=.
gen upper_weekly_cases=.

local i 22159	22166	22173	22180	22187	22194	22201	22208	22215	22222	22229	22236	22243	22250	22257	22264	22271	22278	22285	22292	22299	22306	22313	22320	22327	22334	22341	22348	22355	22362	22369	22376	22383	22390	22397	22404	22411	22418	22425	22432	22439	22446	22453	22460	22467	22474	22481	22488	22495	22502	22509	22516	22523	22530	22537	22544	22551	22558	22565	22572	22579	22586	22593	22600 
foreach y in `i'{
capture reg lcases_weekly CR_index_pak total_pop if date_week == `y', r
capture replace beta_weekly_cases = _b[CR_index_pak] if date_week == `y'
capture replace lower_weekly_cases = _b[CR_index_pak] - invttail(e(df_r),0.025)*_se[CR_index_pak] if date_week == `y'
capture replace upper_weekly_cases =_b[CR_index_pak] + invttail(e(df_r),0.025)*_se[CR_index_pak] if date_week == `y'
}

twoway rcap  upper_weekly_cases lower_weekly_cases date_week, lstyle(ci) ylabel(15(5)-5)||   ///
       scatter beta_weekly_cases date_week, mstyle(p1)                       ///
       legend(off) xtitle("Date") title("(c1)" "Regression coefficients with" "total cases and 95% confidence interval by week""Pakistan (Sindh)") note("source: authors' calculation") ytitle("Regression Coefficient") ///
xlabel(#15) xlabel(22159 "Sept 1, 2020" 22229 "Oct 11, 2020" 22299 "Jan 19, 2021" 22376 "April 6, 2021" 22446 "Jun 15, 2021" 22509 "Aug 17, 2021" 22569 "Oct 16, 2021", angle(vertical)) graphregion(color(white))

graph display, xsize(6)

graph save weekly_cases.gph, replace 

//////////////OLS - Weekly (Cumulative Cases)

********Creating graph for regression with weekly cumulative cases


gen beta_weekly_cases_cum=.
gen lower_weekly_cases_cum=.
gen upper_weekly_cases_cum=.

local i 22159	22166	22173	22180	22187	22194	22201	22208	22215	22222	22229	22236	22243	22250	22257	22264	22271	22278	22285	22292	22299	22306	22313	22320	22327	22334	22341	22348	22355	22362	22369	22376	22383	22390	22397	22404	22411	22418	22425	22432	22439	22446	22453	22460	22467	22474	22481	22488	22495	22502	22509	22516	22523	22530	22537	22544	22551	22558	22565	22572	22579	22586	22593	22600 
foreach y in `i'{
capture reg lncumul_cases CR_index_pak  total_pop if date_week == `y', r
capture replace beta_weekly_cases_cum = _b[CR_index_pak] if date_week == `y'
capture replace lower_weekly_cases_cum = _b[CR_index_pak] - invttail(e(df_r),0.025)*_se[CR_index_pak] if date_week == `y'
capture replace upper_weekly_cases_cum =_b[CR_index_pak] + invttail(e(df_r),0.025)*_se[CR_index_pak] if date_week == `y'
}

twoway rcap  upper_weekly_cases_cum lower_weekly_cases_cum date_week, lstyle(ci) ylabel(15(5)-5)||   ///
       scatter beta_weekly_cases_cum date_week, mstyle(p1)                       ///
       legend(off) xtitle("Date") title("(c2)" "Regression coefficients with" "cumulative cases and 95% confidence interval by week" "Pakistan (Sindh)") note("source: authors' calculation") ytitle("Regression Coefficient") ///
xlabel(#15) xlabel(22159 "Sept 1, 2020" 22229 "Oct 11, 2020" 22299 "Jan 19, 2021" 22376 "April 6, 2021" 22446 "Jun 15, 2021" 22509 "Aug 17, 2021" 22569 "Oct 16, 2021", angle(vertical)) graphregion(color(white))

graph display, xsize(6)

graph save weekly_cases_cum.gph, replace 

////Combine regression graphs for cases
set scheme s2color
graph combine "weekly_cases" "weekly_cases_cum", row(1) graphregion(color(white))
graph display, ysize(2)
		
//////////////OLS - Weekly (Total Death)

********Creating graph for regression with weekly deaths

gen beta_weekly_death=.
gen lower_weekly_death=.
gen upper_weekly_death=.

local i 22159	22166	22173	22180	22187	22194	22201	22208	22215	22222	22229	22236	22243	22250	22257	22264	22271	22278	22285	22292	22299	22306	22313	22320	22327	22334	22341	22348	22355	22362	22369	22376	22383	22390	22397	22404	22411	22418	22425	22432	22439	22446	22453	22460	22467	22474	22481	22488	22495	22502	22509	22516	22523	22530	22537	22544	22551	22558	22565	22572	22579	22586	22593	22600 
foreach y in `i'{
capture reg ldeaths_weekly CR_index_pak total_pop if date_week == `y', r
capture replace beta_weekly_death = _b[CR_index_pak] if date_week == `y'
capture replace lower_weekly_death = _b[CR_index_pak] - invttail(e(df_r),0.025)*_se[CR_index_pak] if date_week == `y'
capture replace upper_weekly_death =_b[CR_index_pak] + invttail(e(df_r),0.025)*_se[CR_index_pak] if date_week == `y'
}

twoway rcap  upper_weekly_death lower_weekly_death date_week, lstyle(ci) ylabel(15(5)-5)||   ///
       scatter beta_weekly_death date_week, mstyle(p1)                     ///
       legend(off) xtitle("Date") title("(b1)" "Regression coefficients with" "total deaths and 95% confidence interval by week" "for Pakistan (Sindh)") note("source: authors' calculation")                   ///
	     ytitle("Regression Coefficient")xlabel(22159 "Sept 1, 2020" 22229 "Oct 11, 2020" 22299 "Jan 19, 2021" 22376 "April 6, 2021" 22446 "Jun 15, 2021" 22509 "Aug 17, 2021" 22569 "Oct 16, 2021", angle(vertical)) graphregion(color(white))
		
graph display, xsize(6)

graph save weekly_deaths.gph, replace 
			
//////////////OLS - Weekly (Cumulative Death)

********Creating graph for regression weekly cumulative deaths

gen beta_weekly_death_cum=.
gen lower_weekly_death_cum=.
gen upper_weekly_death_cum=.

local i 22159	22166	22173	22180	22187	22194	22201	22208	22215	22222	22229	22236	22243	22250	22257	22264	22271	22278	22285	22292	22299	22306	22313	22320	22327	22334	22341	22348	22355	22362	22369	22376	22383	22390	22397	22404	22411	22418	22425	22432	22439	22446	22453	22460	22467	22474	22481	22488	22495	22502	22509	22516	22523	22530	22537	22544	22551	22558	22565	22572	22579	22586	22593	22600 
foreach y in `i'{
capture reg lncumul_deaths CR_index_pak total_pop if date_week == `y', r
capture replace beta_weekly_death = _b[CR_index_pak] if date_week == `y'
capture replace lower_weekly_death = _b[CR_index_pak] - invttail(e(df_r),0.025)*_se[CR_index_pak] if date_week == `y'
capture replace upper_weekly_death =_b[CR_index_pak] + invttail(e(df_r),0.025)*_se[CR_index_pak] if date_week == `y'
}

twoway rcap  upper_weekly_death lower_weekly_death date_week, lstyle(ci) ylabel(15(5)-5)||   ///
       scatter beta_weekly_death date_week, mstyle(p1)                     ///
       legend(off) xtitle("Date") title("(b2)" "Regression coefficients with" "cumulative deaths and 95% confidence interval by week" "for Pakistan (Sindh)") note("source: authors' calculation")                   ///
	     ytitle("Regression Coefficient")xlabel(22159 "Sept 1, 2020" 22229 "Oct 11, 2020" 22299 "Jan 19, 2021" 22376 "April 6, 2021" 22446 "Jun 15, 2021" 22509 "Aug 17, 2021" 22569 "Oct 16, 2021", angle(vertical)) graphregion(color(white))
		
graph display, xsize(6)

graph save weekly_deaths_cum.gph, replace 

////Combine regression graphs for death

graph combine "weekly_deaths" "weekly_deaths_cum", row(1) graphregion(color(white))
graph display, ysize(2)

/////////////Create Map and Zoning Graph 

keep if date_fmt==22494 // "November 22, 2021" Keeping cases data for latest date
sort district 
gen uid =_n 

save "/Users/towhid_mahmood/Dropbox/IZA_Covid19/Codes & Data for Shonchoy et. al. (2023)/Data/Pakistan/Pakistan_map_data.dta", replace


******Shape files and the pre-processed data should be saved in the same folder 

cd "/Users/towhid_mahmood/Dropbox/IZA_Covid19/Codes & Data for Shonchoy et. al. (2023)/Data/Pakistan"

//////for dots by cases

use "pakcoord", clear
rename _ID id
merge m:m id using pakdb
 
replace adm2_en="Karachi Malir" if adm2_en=="Malir"

local i `" "Karachi Central"	"Karachi East"	"Jacobabad"	"Mirpur Khas"	"Naushahro Feroze"	"Shaheed Benazirabad"	"Karachi South"	"Tando Muhammad Khan"	"Tando Allah Yar"	"Karachi West" "'

local J `""Central"	"East"	"Jccobabad"	"Mirpurkhas"	"N. Feroze"	"S.B.A"	"South"	"T.M Khan"	"Tando Allahyar"	"West" "'

foreach y in `i'{
	gettoken j J:J 
	replace adm2_en="`j'" if adm2_en=="`y'"
}

sort adm2_en
egen uid=group(adm2_en)
drop _merge
merge m:m uid using Pakistan_map_data.dta
gen labtype=1
collapse _X _Y labtype, by(adm2_en cum_cases)

gen by_var=1 if cum_cases<2658|cum_cases==2658
replace by_var=2 if cum_cases>2658 |cum_cases==3619
replace by_var=3 if cum_cases>3619 | cum_cases==5465
replace by_var=4 if cum_cases>5465 | cum_cases==27096
replace by_var=5 if cum_cases>27096
save "pakcoord_cases.dta", replace

//////for dots by deaths

use "pakcoord", clear
rename _ID id
merge m:m id using pakdb
 
sort adm2_en
egen uid=group(adm2_en)
drop _merge
merge m:m uid using Pakistan_map_data.dta
gen labtype=1
collapse _X _Y labtype, by(adm2_en cum_deaths)

gen by_var=1 if cum_deaths<12|cum_deaths==12
replace by_var=2 if cum_deaths>12 |cum_deaths==19
replace by_var=3 if cum_deaths>19 | cum_deaths==43
replace by_var=4 if cum_deaths>43 | cum_deaths==495
replace by_var=5 if cum_deaths>495
save "pakcoord_deaths.dta", replace

********Merging shape file with CR-index Data

use pakdb, clear

replace adm2_en="Karachi Malir" if adm2_en=="Malir"

local i `" "Karachi Central"	"Karachi East"	"Jacobabad"	"Mirpur Khas"	"Naushahro Feroze"	"Shaheed Benazirabad"	"Karachi South"	"Tando Muhammad Khan"	"Tando Allah Yar"	"Karachi West" "'

local J `""Central"	"East"	"Jccobabad"	"Mirpurkhas"	"N. Feroze"	"S.B.A"	"South"	"T.M Khan"	"Tando Allahyar"	"West" "'

foreach y in `i'{
	gettoken j J:J 
	replace adm2_en="`j'" if adm2_en=="`y'"
}

rename adm2_en district
sort district
egen uid=group(district)
sort uid
merge 1:1 uid using Pakistan_map_data

*****************CR-index Map*******************

spmap CR_index_pak using pakcoord, clmethod(custom) clbreaks(0 .27 .71 1) ///
clnumber(4) id(id) fcolor(green*.4 orange*.4 red*.5) ///
		legenda(on) legend(size(small)) legtitle("Zoning by Contagion Risk") legstyle(2) ///
		legend(pos(7) row(6) ring(1)) ///
		title("(c1)" "COVID - 19 contagion risk index by district" "Pakistan (Sindh)" , size(*.75)) note("source: authors' calculation") plotregion(icolor(white))  

graph save without_cases.gph, replace 


**********Superimposing Actual Cases on CR-Index Zone************

spmap CR_index_pak using pakcoord, clmethod(custom) clbreaks(0 .27 .71 1) ///
clnumber(4) id(id) fcolor(green*.4 orange*.4 red*.5) ///
point(data("pakcoord_cases.dta") xcoord(_X) ycoord(_Y)  proportional(cum_cases) size(*0.7) fcolor(red%30)  legenda(on) ///
legtitle(" " "Size of Points indicate") leglabel("Number infected") ) ///
		legenda(on) legend(size(small)) legtitle("Zoning by Contagion Risk") legstyle(2) ///
		legend(pos(7) row(6) ring(1)) ///
		title("(c2)" "COVID - 19 contagion risk index by district"  ///
		"superimposing covid-19 cumulative cases" "(November 22, 2021)" "Pakistan (Sindh)", size(*.75)) note("source:  authors' calculation") plotregion(icolor(white))     
graph save with_superimposed.gph, replace 

*****************************Histogram for Zones*********

twoway (hist CR_index_pak, freq fcolor(green*.4)lcolor(black) start(0.07) title("(c3)" "Distribution of index scores and zoning" "Pakistan (Sindh)", size(*.75) ) note("source: authors' calculation") width(0.06)legend(off)xtitle("Index Score", axis(1)) ytitle("Number of Districts", axis(1)) ylabel(0(2)6) xlabel(0(.1).8) graphregion(color(white))) ///
(hist CR_index_pak if CR_index_pak>0,  freq fcolor(green*.4) lcolor(black) start(0.07) width(0.06)legend(off)) ///
(hist CR_index_pak if CR_index_pak>.26, freq fcolor(orange*.4) lcolor(black)start(0.07) width(0.06)legend(off)) ///
(hist CR_index_pak if CR_index_pak==.26, freq fcolor(green*4) lcolor(black)start(0.07) width(0.06)legend(off)) ///
(hist CR_index_pak if CR_index_pak > 0.7,  freq fcolor(red*.5) lcolor(black) start(0.07) width(0.06)legend(off)) ///
(kdensity CR_index_pak, kernel(gaussian) range(0 .9) lpattern(shortdash) lcolor(black) lwidth(thick))

graph save index_a_distribution.gph, replace 


graph combine "without_cases" "with_superimposed" "index_a_distribution", row(1) graphregion(color(white))
graph display, ysize(2)


**********Superimposing Actual Deaths on CR_index_pak************

spmap CR_index_pak using pakcoord, clmethod(custom) clbreaks(0 .27 .71 1) ///
clnumber(4) id(id) fcolor(green*.4 orange*.4 red*.5) ///
point(data("pakcoord_deaths.dta") xcoord(_X) ycoord(_Y)  proportional(cum_deaths) size(*0.7) fcolor(red%30)  legenda(on) ///
legtitle(" " "Size of Points indicate") leglabel("Number infected") ) ///
		legenda(on) legend(size(small)) legtitle("Zoning by Contagion Risk") legstyle(2) ///
		legend(pos(7) row(6) ring(1)) ///
		title("(a2)" "COVID - 19 contagion risk index by district"  ///
		"superimposing covid-19 cumulative deaths" "(November 22, 2021)" "Pakistan (Sindh)", size(*.75)) note("source:  authors' calculation") plotregion(icolor(white))     
graph save Pakistan_with_superimpose_death.gph, replace 


graph combine "india_with_superimposed_death" "Pakistan_with_superimpose_death" , row(1) graphregion(color(white))
graph display, ysize(2)


*******Percentage

gsort -CR_index_pak
keep district daily_cases CR_index_pak
egen cases=total(daily_cases)
gen pct_tot=(daily_cases/cases)*100
br
