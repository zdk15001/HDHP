* Date created: 29 June 2016
* Date modified: 29 July 2016
* Chenyuan Liu

*************************************************************
* LOAD IN THE DATA FOR KFF Employer Health Survey Data 2015
*************************************************************
set more off
clear

cd "\AppendixC\"

do AppendixC_clean.do

***********************************
* Output result
************************************
import excel "runavc_allplan.xlsx", sheet("output_normal") firstrow clear
rename C firm
rename J av
rename D pty
keep firm pty av
g hd = pty == 4
tabstat av, by(hd) stat(mean sd)
sum av if hd == 0
matrix t2[21,1] = r(mean)
matrix t2[22,1] = r(sd)
sum av if hd == 1
matrix t2[21,2] = r(mean)
matrix t2[22,2] = r(sd)

// Appendix Table C1
matrix list AC1

// Appendix Table C2, column 1 and 2
matrix list t2
matrix list t0
//save av_allplan.dta, replace
