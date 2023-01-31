*********Data Driven Contagion Risk Management in Low- Income 
*********Countries: Machine Learning Applications with COVID-19 in South Asia
******Code for Reproducing Results for Bangladesh**********

clear all

cls

////////Import Data 

use "/Users/towhid_mahmood/Dropbox/IZA_Covid19/Codes & Data for Shonchoy et. al. (2023)/Data/Bangladesh/Bangladesh_data.dta", clear


/////////Creating Sub-indices 

****Urban share of population
gen urban_share=urban/tot_sample
su urban_share, meanonly
gen urban_index = (urban_share - r(min)) / (r(max)- r(min))
 
****Migration share of population 
 
gen migration= (mig_internal+ mig_intl)/tot_sample
su migration, meanonly
gen migration_index = (migration - r(min)) / (r(max)- r(min))
 
*****Informal employment share of total employed 

gen inf_emp_share = inf_emp/ total_emp
su inf_emp_share, meanonly
gen inf_emp_share_index = (inf_emp_share - r(min)) / (r(max)- r(min))

*********Beds per million 
 
gen beds_per_m_inv=1 - beds_per_m //Inversing for consistency 
su beds_per_m_inv, meanonly
gen beds_per_m_inv_index = (beds_per_m_inv - r(min)) / (r(max)- r(min))

 
/////Generating index with arithmetic mean

gen CR_index_bd = (migration_index + inf_emp_share_index + urban_index +beds_per_m_inv_index)/4
 
label var  CR_index_bd "Main Index"


///Creating IHS (inverse hyperbolic sine transformation) Log Variables 

gen lcases=log((cases_march22)+((((cases_march22)^2)+1)^(1/2))) // 

gen lcu_cases=log((new_cumulative_cases)+((((new_cumulative_cases)^2)+1)^(1/2))) 

gen lweek_cases=log((new_weekly_cases)+((((new_weekly_cases)^2)+1)^(1/2))) 


//////////Weekly Var 

*****Creating Week Indicator for individual regressions. Use this to create individual graphs for each weekly regression. 

gen week=.

local i 21980	21987	21994	22001	22008	22015	22022	22029	22036	22043	22050	22057	22064	22071	22078	22085	22092	22099	22106	22113	22120	22127	22134	22141	22148	22155	22162	22169	22176	22183	22190	22197	22204	22211	22218	22225	22232	22239	22246	22253	22260	22267	22274	22281	22288	22295	22302	22309	22316	22323	22330	22337	22344	22351	22358	22365	22372	22379	22386	22393	22400	22407	22414	22421	22428	22435	22442	22449	22456	22463	22470	22477	22484	22491	22498	22505	22512	22519	22526	22533	22540	22547	22554	22561	22568	22575	22582	22589	22596	22603	22610	22617	22624	22631	22638	22645	22652	22659	22666	22673	22680	22687	22694	22701	22708	22715
foreach y in `i'{
replace  week=1 if date_fmt==`y'
}

gen date_week = date_fmt if week==1


/////////Regression

********OLS with time trend 

qui reg lcases CR_index_bd i.date_fmt, r 
est table, star(.05 .01 .001) keep(CR_index_bd)

********Time Fixed Effect Model with district control 

qui areg lcases CR_index_bd pop_2011, r absorb(date_fmt) 
est table, star(.05 .01 .001) keep(CR_index_bd)

////////OLS - Graphs - Weekly Total Cases 

********Creating graph for regression with weekly cases

gen beta_weekly_cases=.
gen lower_weekly_cases=.
gen upper_weekly_cases=.

local i 21980	21987	21994	22001	22008	22015	22022	22029	22036	22043	22050	22057	22064	22071	22078	22085	22092	22099	22106	22113	22120	22127	22134	22141	22148	22155	22162	22169	22176	22183	22190	22197	22204	22211	22218	22225	22232	22239	22246	22253	22260	22267	22274	22281	22288	22295	22302	22309	22316	22323	22330	22337	22344	22351	22358	22365	22372	22379	22386	22393	22400	22407	22414	22421	22428	22435	22442	22449	22456	22463	22470	22477	22484	22491	22498	22505	22512	22519	22526	22533	22540	22547	22554	22561	22568	22575	22582	22589	22596	22603	22610	22617	22624	22631	22638	22645	22652	22659	22666	22673	22680	22687	22694	22701	22708	22715
foreach y in `i'{
capture reg lweek_cases CR_index_bd total_pop if date_week == `y', r
capture replace beta_weekly_cases = _b[CR_index_bd] if date_week == `y'
capture replace lower_weekly_cases = _b[CR_index_bd] - invttail(e(df_r),0.025)*_se[CR_index_bd] if date_week == `y'
capture replace upper_weekly_cases =_b[CR_index_bd] + invttail(e(df_r),0.025)*_se[CR_index_bd] if date_week == `y'
}

twoway rcap  upper_weekly_cases lower_weekly_cases date_week, lstyle(ci) ylabel(15(5)-5) ||   ///
       scatter beta_weekly_cases date_week, mstyle(p1)                       ///
       legend(off) xtitle("Date") title("(a1)" "Regression coefficients with" "total cases and 95% confidence interval by week" "Bangladesh") note("source: authors' calculation") ytitle("Regression Coefficient") ///
xlabel(#15) xlabel(21980 "March 06, 2020" 22200  "Oct 12, 2020" 22050 "May 15,2020" 22267 "Jan 20, 2021" 22331 "Feb 20, 2021" 22400 "April 30, 2021" 22127 "July 31, 2020" 22456 "June 25, 2021" 22516 "Aug 24, 2022" 22577 "Oct 23, 2021" 22637 "Dec 22,2021" 22697 "Feb 20, 2022", angle(vertical)) graphregion(color(white))

graph display, xsize(6)

qui graph save weekly_cases.gph, replace 


//////////////OLS - Weekly (Cumulative Cases)

********Creating graph for regression with weekly cumulative cases


gen beta_weekly_cases_cum=.
gen lower_weekly_cases_cum=.
gen upper_weekly_cases_cum=.

local i 21980	21987	21994	22001	22008	22015	22022	22029	22036	22043	22050	22057	22064	22071	22078	22085	22092	22099	22106	22113	22120	22127	22134	22141	22148	22155	22162	22169	22176	22183	22190	22197	22204	22211	22218	22225	22232	22239	22246	22253	22260	22267	22274	22281	22288	22295	22302	22309	22316	22323	22330	22337	22344	22351	22358	22365	22372	22379	22386	22393	22400	22407	22414	22421	22428	22435	22442	22449	22456	22463	22470	22477	22484	22491	22498	22505	22512	22519	22526	22533	22540	22547	22554	22561	22568	22575	22582	22589	22596	22603	22610	22617	22624	22631	22638	22645	22652	22659	22666	22673	22680	22687	22694	22701	22708	22715
foreach y in `i'{
capture reg lcu_cases CR_index_bd total_pop if date_week == `y', r
capture replace beta_weekly_cases_cum = _b[CR_index_bd] if date_week == `y'
capture replace lower_weekly_cases_cum = _b[CR_index_bd] - invttail(e(df_r),0.025)*_se[CR_index_bd] if date_week == `y'
capture replace upper_weekly_cases_cum =_b[CR_index_bd] + invttail(e(df_r),0.025)*_se[CR_index_bd] if date_week == `y'
}

twoway rcap  upper_weekly_cases_cum lower_weekly_cases_cum date_week, lstyle(ci) ylabel(15(5)-5) ||   ///
       scatter beta_weekly_cases_cum date_week, mstyle(p1)                       ///
       legend(off) xtitle("Date") title("(a2)" "Regression coefficients with" "cumulative cases and 95% confidence interval by week" "Bangladesh") note("source: authors' calculation") ytitle("Regression Coefficient") ///
xlabel(#15) xlabel(21980 "March 06, 2020" 22200  "Oct 12, 2020" 22050 "May 15,2020" 22267 "Jan 20, 2021" 22331 "Feb 20, 2021" 22400 "April 30, 2021" 22127 "July 31, 2020" 22456 "June 25, 2021" 22516 "Aug 24, 2022" 22577 "Oct 23, 2021" 22637 "Dec 22,2021" 22697 "Feb 20, 2022", angle(vertical)) graphregion(color(white))

graph display, xsize(6)

qui graph save weekly_cases_cum.gph, replace 


//////////////Combine Regression Graphs 

graph combine "weekly_cases" "weekly_cases_cum", row(1) graphregion(color(white))
graph display, ysize(2) 

/////////////Create Map and Zoning Graph 

keep if date=="February102022" //Keeping cases data for latest date 
sort District
drop uid
gen uid=_n
sort uid  

save "/Users/towhid_mahmood/Dropbox/IZA_Covid19/Codes & Data for Shonchoy et. al. (2023)/Data/Bangladesh_map_data.dta", replace

******Shape files and the pre-processed data should be saved in the same folder 

cd "/Users/towhid_mahmood/Dropbox/IZA_Covid19/Codes & Data for Shonchoy et. al. (2023)/Data/Bangladesh"

clear all
shp2dta using BGD_ADM2, database(bddb) coordinates(bdcoord) genid(id) replace

use Bangladesh_map_data.dta, clear

*******Processing data for superimposing cases on the map

use "bdcoord", clear
rename _ID uid
merge m:m uid using Bangladesh_map_data
gen labtype=1
collapse _X _Y labtype, by(district new_cumulative_cases)

gen by_var=1 if new_cumulative_cases<4816|new_cumulative_cases==4816
replace by_var=2 if new_cumulative_cases>4816 |new_cumulative_cases==9775  
replace by_var=3 if new_cumulative_cases>9775 | new_cumulative_cases== 20003
replace by_var=4 if new_cumulative_cases>20003 | new_cumulative_cases==30460 
replace by_var=5 if new_cumulative_cases>30460 
save "bdcoord_cases.dta", replace

********Merging shape file with CR-index Data

use bddb, clear
rename Name district
sort district
gen uid=_n
sort uid
merge 1:1 uid using Bangladesh_map_data

*****************CR-index Map*****************************

spmap CR_index_bd using bdcoord, clmethod(custom) clbreaks(0 .43 .62 1) ///
clnumber(4) id(id) fcolor(green*.4 orange*.4 red*.5) ///
		legenda(on) legend(size(medium)) legtitle("Zoning by Contagion Risk") legstyle(2) ///
		legend(pos(7) row(6) ring(1)) ///
		title("(a1)" "Covid - 19 contagion risk index by district"  ///
		"Bangladesh", size(*.75)) note("source:  authors' calculation")plotregion(icolor(white))   

qui graph save without_cases.gph, replace 


**********Superimposing Actual Cases on CR-Index Zone************

spmap CR_index_bd using bdcoord, clmethod(custom) clbreaks(0 .43 .62 1)  ///
clnumber(4) id(id) fcolor(green*.4 orange*.4 red*.5) ///
point(data("bdcoord_cases.dta") xcoord(_X) ycoord(_Y)  proportional(new_cumulative_cases) size(*0.7) fcolor(red%30) ocolor(black) legenda(on) ///
legtitle(" " "Size of Points indicate") leglabel("Number infected") ) ///
		legenda(on) legend(size(medium)) legtitle("Zoning by Contagion Risk") legstyle(2) ///
		legend(pos(7) row(6) ring(1)) ///
		title("(a2)" "Covid - 19 contagion risk index by district"  ///
		"superimposing covid-19 cumulative cases" "(February 28, 2022)" "Bangladesh", size(*.75)) note("source:  authors' calculation") plotregion(icolor(white))    
qui graph save with_superimposed.gph, replace 

*****************************Histogram for Zones***********

twoway (hist CR_index_bd, freq fcolor(green*.4)lcolor(black) start(0.07) title("(a3)" "Distribution of index scores and zoning""Bangladesh", size(*.75)) note("source: authors' calculation") width(0.03)legend(off)xtitle("Index Score", axis(1)) ytitle("Number of Districts", axis(1)) ylabel(0(5)15) xlabel(0(.1).8) graphregion(color(white))) ///
(hist CR_index_bd if CR_index_bd>0,  freq fcolor(green*.4) lcolor(black) start(.1) width(0.03)legend(off)) ///
(hist CR_index_bd if CR_index_bd>.43, freq fcolor(orange*.4) lcolor(black)start(.1) width(0.03)legend(off)) ///
(hist CR_index_bd if CR_index_bd==.43, freq fcolor(green*4) lcolor(black)start(.1) width(0.03)legend(off)) ///
(hist CR_index_bd if CR_index_bd ==0.62,  freq fcolor(orange*.4) lcolor(black)start(.1) width(0.03)legend(off)) ///
(hist CR_index_bd if CR_index_bd > 0.64,  freq fcolor(red*.5) lcolor(black)start(.1) width(0.03)legend(off)) ///
(kdensity CR_index_bd, kernel(gaussian) range(0 .8) lpattern(shortdash) lcolor(black) lwidth(thick)) 

qui graph save CR_index_bd_distribution.gph, replace 


graph combine "without_cases" "with_superimposed" "CR_index_bd_distribution", row(1) graphregion(color(white))
graph display, ysize(2)

*******Percentage Share of Covid Cases by District and CR-index
gsort -CR_index_bd
keep District cases_march22 CR_index_bd
egen cases=total(cases_march22)
gen pct_tot=(cases_march22/cases)*100
br



