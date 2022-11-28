clear
set more off
log using "icfp2022_log.smcl", replace

*********
**SETUP**
*********

cd "[INSERT FILE PATH HERE]"

use [INSERT FILE NAME HERE].dta


*keep only panel members in the de facto population 
keep if resultfq_1 == 1 & resultfq_2 == 1
keep if (resident_1 == 11 | resident_1 == 22) & (resident_2 == 11 | resident_2 == 22)

************************************
**UNMET NEED BY COUNTRY AT PHASE 1**
************************************
svyset [pw=panelweight], psu(eaid_1) strata(strata_1)

svy: tab unmetneed_1 country, col
svy: tab unmetyn_1 country, col

*******************************
**LOGISTIC REGRESSION RESULTS**
*******************************

replace unmetyn_1 = . if unmetyn_1 == 99
svy: logit unmetyn_2 unmetyn_1 if country == 1, or
svy: logit unmetyn_2 unmetyn_1 if country == 7, or

************
**RECODING**
************

foreach var of varlist fpynot* {
replace `var' = 0 if unmetyn_1 == 1 & `var' == 99
recode `var' (0=0) (1=1) (else=0)
}

replace pregnant_1 = 0 if pregnant_1 > 90

*Creates a binary variable that indicates whether one reason the woman gave for not using FP was opposition or prohibition to family planning
egen fpyopposed = rowmax(fpynotrelig_1 fpynotoppf_1 fpynotfate_1 fpynotopph_1 fpynotoppo_1 fpynotsidef_1 fpynotsdhlth_1 fpynotconv_1 fpynotbody_1)
replace fpyopposed = 0 if unmetyn_1 != 1
*Creates a binary variable that indicates whether one reason the woman gave for not using FP was a method access issue
egen fpymethod = rowmax(fpynotfar_1 fpynotcost_1 fpynotkno_1 fpynotsrc_1 fpynotavail_1 fpynotavailp_1 pregnant_1)
replace fpymethod = 0 if unmetyn_1 != 1
*Creates a binary variable that indicates whether one reason the woman gave for not using FP was low risk of pregnancy
egen fpylowrisk = rowmax(fpynotbstfd_1 fpynothsbaway_1 fpynotmeno_1 fpynotamen_1 fpynotnosex_1 fpynotmar_1 fpynotinf_1)
replace fpylowrisk = 0 if unmetyn_1 != 1
*Creates a binary variable that indicates whether the woman's reason for not using family planning was some other reason or unknown
gen fpyother = 0
replace fpyother = 1 if unmetyn_1 == 1 & fpyopposed !=1 & fpymethod !=1 & fpylowrisk !=1

******************************
**RECODED REASONS BY COUNTRY**
******************************

tabstat fpyopposed fpymethod fpylowrisk fpyother if unmetyn_1 == 1 [weight=panelweight], by(country)

***********
**RESULTS**
***********

svy: logit unmetyn_2 fpyopposed fpymethod fpylowrisk unmetyn_1 if country == 1, or
svy: logit unmetyn_2 fpyopposed fpymethod fpylowrisk unmetyn_1 if country == 7, or

log close





