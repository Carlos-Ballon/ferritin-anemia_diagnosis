
**********************
* Wealth INDEX *
**********************

*==============================================================================
 * Do file to preparing ENDES
 *

 * @project   {2018} - {Juntos & Depressive Symptoms}

 * @created   {Jan 2019}

 * @revised   {...}

 *      

 * @category   Cleaning Do File

 * @ado's      Not apply

 * @author     Percy Soto-Becerra <percys1991@gmail.com>

 * @author     Not apply another author

 * @copyright  Not apply

 * @license    Not apply

 * @version    Stata/SE 15.1

 * @link       Not apply

 * @see        Not apply

 * @since      Not apply

 * @deprecated Not apply
 

*==============================================================================

version 15.1

clear all

pause on

set more off

set trace off

set maxvar 15000

set matsize 10000

set seed 123456789

*===============================================================================

local dir `c(pwd)'

cd "`c(pwd)'"


use "bd_hogares_for_wealth.dta", clear

** Variable HOUSE: No hay en ENDES Peru

** Variable identificadora de cuestinario completo
tab hv015

** Variable LAND:
tab hv244

** Variable DOMESTIC:
tab DOMESTIC

** Variables de Tenencias:
#delimit ;
foreach var of varlist sh61a sh61b sh61c sh61d sh61e sh61j sh61k sh61l sh61m 
	sh61n sh61o sh61p sh61q sh61r sh61s sh76a sh76b sh76c sh76d sh76e  hv243a 
	hv243c hv225 { ;
	replace `var' = 0 if `var' == . & hv015 == 1 ;
} ;
#delimit cr

* NMSLEEPINGROOM
gen nmember = hv012
replace nmember = hv013 if hv012 == 0

gen sleeproom = hv216
replace sleeproom = 1 if hv216 == 0

gen NMSLEEPROOM = round(nmember/sleeproom)

* Source of Drinking Water
tab hv201, gen(drinkwater_dum)

* Type of Toilet
#delimit ;
recode hv205 (11 = 1 "inside dwelling") (12 = 2 "inside building") 
	(22 = 3 "septic tank") (21 = 4 "VIP latrine") 
	(24 = 5 "Hanging, floating latrine") (23 = 6 "Tradional pit latrine") 
	(31 32 = 7 "No facility/river/canal/bush/field") 
	(96 = 8 "Other type of latrine/toilet"), gen(toilet) lab(toilet) ;
#delimit cr

tab toilet, gen(toilet_dum)

* Shared type of toilet
tab hv225

* Floor of dweliing
tab hv213, gen(floor_dum)

* Wall of dwelling
tab hv214, gen(wall_dum)

* Roof of dwelling
tab hv215, gen(roof_dum)

* Type of cooking fuel
tab hv226, gen(cook_dum)

* Lighting
tab sh70, gen(light_dum)


#delimit ;
foreach var of varlist floor_dum1 floor_dum2 floor_dum3 floor_dum4 floor_dum5 
	floor_dum6 floor_dum7 wall_dum* roof_dum* { ;
	replace `var' = 0 if `var' == . & hv015 == 1 ;
} ;
#delimit cr

* Garbage
#delimit ;
recode sh58 (11/15 = 1 "picked from home") (21/24 = 2 "placed in public conainers")
	(31 41 42 43 44 = 3 "tossed into field/street/yeard/distant/place/stream") 
	(51 = 4 "burned") (61 = 5 "buried") (71 = 6 "composted") (81 = 7 "fed to animals")
	(96 98 = 8 "other disposal or DK"), gen(garbage) lab(garbage) ;
#delimit cr

tab garbage, gen(garbage_dum)

********************************************************************************
** SCORE TOTAL (URBANO + RURAL) **
#delimit ;
tabstat sh61a sh61b sh61c sh61d sh61e sh61j sh61k sh61l sh61m sh61n sh61o sh61p 
	sh61q sh61r sh61s sh76a sh76b sh76c sh76d sh76e DOMESTIC NMSLEEPROOM 
	drinkwater_dum1 drinkwater_dum2 drinkwater_dum3 drinkwater_dum4 
	drinkwater_dum5 drinkwater_dum6 drinkwater_dum7 drinkwater_dum8 
	drinkwater_dum9 drinkwater_dum10 drinkwater_dum11 toilet_dum1 toilet_dum2 
	toilet_dum3 toilet_dum4 toilet_dum5 toilet_dum6 toilet_dum7 toilet_dum8 
	hv225 floor_dum1 floor_dum2 floor_dum6 floor_dum4 floor_dum5 floor_dum3 
	floor_dum7 wall_dum1 wall_dum2 wall_dum3 wall_dum4 wall_dum5 wall_dum6 
	wall_dum7 wall_dum8 wall_dum9 wall_dum10 wall_dum11 wall_dum12 roof_dum1 
	roof_dum2 roof_dum3 roof_dum4 roof_dum5 roof_dum6 roof_dum7 roof_dum8 
	roof_dum9 roof_dum10 cook_dum1 cook_dum2 cook_dum3 cook_dum4 cook_dum5 
	cook_dum6 cook_dum7 cook_dum8 cook_dum9 cook_dum10 cook_dum11 cook_dum12 
	light_dum1 light_dum2 light_dum3 light_dum4 light_dum5 light_dum6 light_dum7 
	light_dum8 if hv015 == 1, stat(mean sd n) 
	col(stat) missing varwidth(15) format(%14.5f) ;
#delimit cr

** PCA TOTAL **
#delimit ;
pca sh61a sh61b sh61c sh61d sh61e sh61j sh61k sh61l sh61m sh61n sh61o sh61p 
	sh61q sh61r sh61s sh76a sh76b sh76c sh76d sh76e DOMESTIC NMSLEEPROOM 
	drinkwater_dum1 drinkwater_dum2 drinkwater_dum3 drinkwater_dum4 
	drinkwater_dum5 drinkwater_dum6 drinkwater_dum7 drinkwater_dum8 
	drinkwater_dum9 drinkwater_dum10 drinkwater_dum11 toilet_dum1 toilet_dum2 
	toilet_dum3 toilet_dum4 toilet_dum5 toilet_dum6 toilet_dum7 toilet_dum8 
	hv225 floor_dum1 floor_dum2 floor_dum6 floor_dum4 floor_dum5 floor_dum3 
	floor_dum7 wall_dum1 wall_dum2 wall_dum3 wall_dum4 wall_dum5 wall_dum6 
	wall_dum7 wall_dum8 wall_dum9 wall_dum10 wall_dum11 wall_dum12 roof_dum1 
	roof_dum2 roof_dum3 roof_dum4 roof_dum5 roof_dum6 roof_dum7 roof_dum8 
	roof_dum9 roof_dum10 cook_dum1 cook_dum2 cook_dum3 cook_dum4 cook_dum5 
	cook_dum6 cook_dum7 cook_dum8 cook_dum9 cook_dum10 cook_dum11 cook_dum12 
	light_dum1 light_dum2 light_dum3 light_dum4 light_dum5 light_dum6 light_dum7 
	light_dum8 if hv015 == 1, components(1) 
	correlation means ;
#delimit cr

predict COM if e(sample), score


********************************************************************************
** SCORE URBANO **
#delimit ;
foreach var of varlist hv246a hv246c hv246d hv246e hv246g hv246h hv246i hv246j { ;
	recode `var' (98 = .) ; 
	sum `var' if hv015 == 1, meanonly ;
	scalar mean_`var' = r(mean) ;
	replace `var' = mean_`var' if `var' == . & hv015 == 1 ;
} ;
#delimit cr

gen landarea = hv245
recode landarea (98 = .)
replace landarea = 0 if hv244 == 0
sum landarea if hv015 == 1, meanonly
replace landarea = r(mean) if landarea == . & hv015 == 1


#delimit ;
tabstat sh61a sh61b sh61c sh61d sh61e sh61j sh61k sh61l sh61m sh61n sh61o sh61p 
	sh61q sh61r sh61s sh76a sh76b sh76c sh76d sh76e hv243a hv243c hv243d hv246a 
	hv246c hv246d hv246e hv246g hv246h hv246i hv246j DOMESTIC hv244 NMSLEEPROOM 
	drinkwater_dum1 drinkwater_dum2 drinkwater_dum3 drinkwater_dum4 
	drinkwater_dum5 drinkwater_dum6 drinkwater_dum7 drinkwater_dum8 
	drinkwater_dum9 drinkwater_dum10 drinkwater_dum11 toilet_dum1 toilet_dum2 
	toilet_dum3 toilet_dum4 toilet_dum5 toilet_dum6 toilet_dum7 toilet_dum8 
	hv225 floor_dum1 floor_dum2 floor_dum6 floor_dum4 floor_dum5 floor_dum3 
	floor_dum7 wall_dum1 wall_dum2 wall_dum3 wall_dum4 wall_dum5 wall_dum6 
	wall_dum7 wall_dum8 wall_dum9 wall_dum10 wall_dum11 wall_dum12 roof_dum1 
	roof_dum2 roof_dum3 roof_dum4 roof_dum5 roof_dum6 roof_dum7 roof_dum8 
	roof_dum9 roof_dum10 cook_dum1 cook_dum2 cook_dum3 cook_dum4 cook_dum5 
	cook_dum6 cook_dum7 cook_dum8 cook_dum9 cook_dum10 cook_dum11 cook_dum12 
	light_dum1 light_dum2 light_dum3 light_dum4 light_dum5 light_dum6 light_dum7 
	light_dum8 landarea if hv015 == 1  & hv025 == 1, stat(mean sd n) 
	col(stat) missing varwidth(15) format(%14.5f) ;
#delimit cr

** PCA URBANO **
#delimit ;
pca sh61a sh61b sh61c sh61d sh61e sh61j sh61k sh61l sh61m sh61n sh61o sh61p 
	sh61q sh61r sh61s sh76a sh76b sh76c sh76d sh76e hv243a hv243c hv243d hv246a 
	hv246c hv246d hv246e hv246g hv246h hv246i hv246j DOMESTIC hv244 NMSLEEPROOM 
	drinkwater_dum1 drinkwater_dum2 drinkwater_dum3 drinkwater_dum4 
	drinkwater_dum5 drinkwater_dum6 drinkwater_dum7 drinkwater_dum8 
	drinkwater_dum9 drinkwater_dum10 drinkwater_dum11 toilet_dum1 toilet_dum2 
	toilet_dum3 toilet_dum4 toilet_dum5 toilet_dum6 toilet_dum7 toilet_dum8 
	hv225 floor_dum1 floor_dum2 floor_dum6 floor_dum4 floor_dum5 floor_dum3 
	floor_dum7 wall_dum1 wall_dum2 wall_dum3 wall_dum4 wall_dum5 wall_dum6 
	wall_dum7 wall_dum8 wall_dum9 wall_dum10 wall_dum11 wall_dum12 roof_dum1 
	roof_dum2 roof_dum3 roof_dum4 roof_dum5 roof_dum6 roof_dum7 roof_dum8 
	roof_dum9 roof_dum10 cook_dum1 cook_dum2 cook_dum3 cook_dum4 cook_dum5 
	cook_dum6 cook_dum7 cook_dum8 cook_dum9 cook_dum10 cook_dum11 cook_dum12 
	light_dum1 light_dum2 light_dum3 light_dum4 light_dum5 light_dum6 light_dum7 
	light_dum8 landarea if hv015 == 1 & hv025 == 1, components(1) 
	correlation means ;
#delimit cr


predict URB if e(sample), score

********************************************************************************
** SCORE RURAL **

#delimit ;
tabstat sh61a sh61b sh61c sh61d sh61e sh61j sh61k sh61l sh61m sh61n sh61o sh61p 
	sh61q sh61r sh61s sh76a sh76b sh76c sh76d sh76e hv243a hv243c hv243d hv246a 
	hv246c hv246d hv246e hv246g hv246h hv246i hv246j DOMESTIC hv244 NMSLEEPROOM 
	drinkwater_dum1 drinkwater_dum2 drinkwater_dum3 drinkwater_dum4 
	drinkwater_dum5 drinkwater_dum6 drinkwater_dum7 drinkwater_dum8 
	drinkwater_dum9 drinkwater_dum10 drinkwater_dum11 toilet_dum1 toilet_dum2 
	toilet_dum3 toilet_dum4 toilet_dum5 toilet_dum6 toilet_dum7 toilet_dum8 
	hv225 floor_dum1 floor_dum2 floor_dum6 floor_dum4 floor_dum5 floor_dum3 
	floor_dum7 wall_dum1 wall_dum2 wall_dum3 wall_dum4 wall_dum5 wall_dum6 
	wall_dum7 wall_dum8 wall_dum9 wall_dum10 wall_dum11 wall_dum12 roof_dum1 
	roof_dum2 roof_dum3 roof_dum4 roof_dum5 roof_dum6 roof_dum7 roof_dum8 
	roof_dum9 roof_dum10 cook_dum1 cook_dum2 cook_dum3 cook_dum4 cook_dum5 
	cook_dum6 cook_dum7 cook_dum8 cook_dum9 cook_dum10 cook_dum11 cook_dum12 
	light_dum1 light_dum2 light_dum3 light_dum4 light_dum5 light_dum6 light_dum7 
	light_dum8 landarea if hv015 == 1  & hv025 == 2, stat(mean sd n) 
	col(stat) missing varwidth(15) format(%14.5f) ;
#delimit cr

** PCA RURAL **
#delimit ;
pca sh61a sh61b sh61c sh61d sh61e sh61j sh61k sh61l sh61m sh61n sh61o sh61p 
	sh61q sh61r sh61s sh76a sh76b sh76c sh76d sh76e hv243a hv243c hv243d hv246a 
	hv246c hv246d hv246e hv246g hv246h hv246i hv246j DOMESTIC hv244 NMSLEEPROOM 
	drinkwater_dum1 drinkwater_dum2 drinkwater_dum3 drinkwater_dum4 
	drinkwater_dum5 drinkwater_dum6 drinkwater_dum7 drinkwater_dum8 
	drinkwater_dum9 drinkwater_dum10 drinkwater_dum11 toilet_dum1 toilet_dum2 
	toilet_dum3 toilet_dum4 toilet_dum5 toilet_dum6 toilet_dum7 toilet_dum8 
	hv225 floor_dum1 floor_dum2 floor_dum6 floor_dum4 floor_dum5 floor_dum3 
	floor_dum7 wall_dum1 wall_dum2 wall_dum3 wall_dum4 wall_dum5 wall_dum6 
	wall_dum7 wall_dum8 wall_dum9 wall_dum10 wall_dum11 wall_dum12 roof_dum1 
	roof_dum2 roof_dum3 roof_dum4 roof_dum5 roof_dum6 roof_dum7 roof_dum8 
	roof_dum9 roof_dum10 cook_dum1 cook_dum2 cook_dum3 cook_dum4 cook_dum5 
	cook_dum6 cook_dum7 cook_dum8 cook_dum9 cook_dum10 cook_dum11 cook_dum12 
	light_dum1 light_dum2 light_dum3 light_dum4 light_dum5 light_dum6 light_dum7 
	light_dum8 landarea if hv015 == 1 & hv025 == 2, components(1) 
	correlation means ;
#delimit cr

predict RUR if e(sample), score

regress COM URB
predict COMBSCORE1 if e(sample), xb

regress COM RUR
predict COMBSCORE2 if e(sample), xb

gen COMBSCOR = .
format %9.5g COMBSCOR
replace COMBSCOR = COMBSCORE1 if hv025 == 1
replace COMBSCOR = COMBSCORE2 if hv025 == 2

drop COMBSCORE1 COMBSCORE2

* Recalculando weights para poolear ENDES (HOGARES)
** Fuente: https://userforum.dhsprogram.com/index.php?t=msg&goto=3570&
/*=============================================================================================
pesoadj = peso*(total pob Hogares en pais en momento de encuesta)/(numero de Hogares entrevistada)
==============================================================================================*/
scalar TOTWT = 1000000

quietly summarize hv005

scalar T = r(sum)

gen wg_adj = wg_denorm*T/TOTWT

gen HHEMWT = wg_denorm*hv012/1000000

rename COMBSCOR wealthnew

xtile wealthnewq = wealthnew[pweight = HHEMWT], nq(5)

xtile wealthnewd = wealthnew[pweight = HHEMWT], nq(10)

 destring hhid, gen(hhidn)
 
 drop hhid2
 
 tostring hhidn, gen(hhids)
 
 egen hhid2 = concat(year hhids)
 
 drop _merge
 
 lab var wealthnew "Wealth Index score of pooled ENDES"
 
 lab var wealthnewq "Quintiles of Wealth Index score"
 
 lab var wealthnewd "Deciles of Wealth Index score"
 
 hist wealthnew

save "bd_wealth_index.dta", replace
