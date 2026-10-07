* Date created: 29 June 2016
* Date modified: 29 July 2016
* Created by Chenyuan Liu (chenyuan.liu@wisc.edu)

set more off
clear

cd "\Analysis"

* Load data from MATLAB
import delimited avcom.csv, clear
rename v1 firm
rename v2 pty
rename v3 gty
rename v4 csr0
g domi = gty == 3
save avcom.dta, replace

* clean data
do clean_2015.do

* merge data from MATLAB
gen impu = e27iflg==1|f3biflg==1|e3biflg==1|g3biflg==1|f6iflg==1|g6iflg==1|g18iflg==1|e29iflg==1|f29iflg==1|g29iflg==1|d32iflg==1|e32iflg==1|f32iflg==1|g32iflg==1|e7aiflg==1|e7biflg==1|e9bxiflg==1|f9bxiflg==1|g9bxiflg==1|g41iflg==1|g43iflg==1
merge 1:1 firm pty using avcom.dta
keep if _merge == 3
drop _merge

* reshape back to wide table
gen pty3="hdp"
replace pty3="other" if pty==1|pty==2|pty==3
drop pty
drop planindex
reshape wide pctcvrd fr1 bi11a bi11a2 prm11a3 prm11a4 bi11b bi11c bi11e bi13a3 bi13a4 bi13b oly cs18 cs18a cs18acat cs18b cs18bcat cs18c grdfthr fr1a fr1b fr1c fr1d ddct2 cs27 cs27a cs29 cs29a cs30a cs30aa cs30ab cs30ab1 cs30b cs30ba cs30bb cs30bb1 cs30c cs30ca cs30cb cs30cb1 cs30d cs30da cs30db cs30db1 cs32 ddct3b ddct3c ddct4 cs40 ddct5 bi50 bi51 ddct5a ddct5a1 cs6 cs6a cs6acat cs6b cs6bcat cs6c cs7a cs7acat cs7b cs7bcat cs7d cs7x1 cs7x2 cs7x3 cs7x4 cs7x5 cs8a cs8b cs8c cs8d cs8e cs8f cs8x1 cs8x2 cs8x3 cs8x4 cs8x5 cs8x6 cs9 cs9bx cs9bxcat ddctf ddcts empfa empsa epctf epcts hashigh hashigh2 ofp ofenr prmffull prmsfull pwt pctf pcts prmfa prmmf prmms prmsa rdctf rdcts wkrf wkrfa wkrs wkrsa sd1 sd2 sd3 prob_1_11 prob_1_12 prob_1_13 prob_1_14 prob_1_15 prob_1_16 prob_1_21 prob_1_22 prob_1_23 prob_1_24 prob_1_25 prob_1_26 prob_1_31 prob_1_32 prob_1_33 prob_1_34 prob_1_35 prob_1_36 prob_2_11 prob_2_12 prob_2_13 prob_2_14 prob_2_21 prob_2_22 prob_2_31 prob_2_32 prob_2_33 prob_2_4 prob_2_42 prob_3_1 prob_3_2 prob_3_3 prob_3_4 prob_3_5, i(firm) j(pty3) string
la de mLabel 5 "HMO" 6 "PPO" 7 "POS" 
la val pty2 mLabel
rename pty2 pty

* generate more variables
{
g nprmsahdp=prmsahdp*pctshdp
g nprmsaother=prmsaother*pctsother
g largefirm=size==6
g private=a10==1
replace age50=age50-1
replace hiincome=hiincome-1
g union=2-a4
replace fr1hdp=fr1hdp-1
replace fr1other=fr1other-1
g hsa=ofphdp==2
g hra=ofphdp==1
replace g43ann=. if hsa!=1
replace g41=. if hra!=1
}

* Table 2. Strict Dominance Classification for Simplified Plan Representations
tabstat pctcvrdhdp, by (gty) stat (mean)

* Table 7. Dominance Pattern and Firm Demographics
local varlist "largefirm hiincome age50 union fr1hdp private k19_analysis hsa"
foreach var in `varlist' {
	ttest domi, by (`var')
}
tabstat domi, by(indust4) stat(n mean)

* Appendix A. Family Coverage
{
g g42new=g42
replace g42new=0 if g42==.
g g44annnew=g44ann
replace g44annnew=0 if g44ann==.
g nprmfahdp=prmfahdp*pctfhdp-g42new-g44annnew
g nprmfaother=prmfaother*pctfother
g difnprmfa=nprmfaother-nprmfahdp
g difddctfa=ddctfother-ddctfhdp
g difsd2fa=difnprmfa+difddctfa

g nprmsihdp=prmsahdp*pctshdp-g41-g43ann
g nprmsiother=prmsaother*pctsother
g difnprmsi=nprmsiother-nprmsihdp
g difddctsi=ddctsother-ddctshdp
g difsd2si=difnprmsi+difddctsi

count if difsd2si>=0
count if difsd2si>=0 & difsd2fa>=0
}

* Appendix Table B1
{
g prob_1=prob_1_11hdp==1 | prob_1_12hdp==1 | prob_1_13hdp==1 | prob_1_14hdp==1 | prob_1_15hdp==1 | prob_1_16hdp==1 | prob_1_21hdp==1 | prob_1_22hdp==1 | prob_1_23hdp==1 | prob_1_24hdp==1 | prob_1_25hdp==1 | prob_1_26hdp==1 | prob_1_31hdp==1 | prob_1_32hdp==1 | prob_1_33hdp==1 | prob_1_34hdp==1 | prob_1_35hdp==1 | prob_1_36hdp==1|prob_1_11other==1 | prob_1_12other==1 | prob_1_13other==1 | prob_1_14other==1 | prob_1_15other==1 | prob_1_16other==1 | prob_1_21other==1 | prob_1_22other==1 | prob_1_23other==1 | prob_1_24other==1 | prob_1_25other==1 | prob_1_26other==1 | prob_1_31other==1 | prob_1_32other==1 | prob_1_33other==1 | prob_1_34other==1 | prob_1_35other==1 | prob_1_36other==1
g prob_21= prob_2_11hdp==1 | prob_2_12hdp==1 | prob_2_13hdp==1 |  prob_2_11other==1 | prob_2_12other==1 | prob_2_13other==1 | prob_2_15==1 
g prob_23= prob_2_31hdp==1 | prob_2_32hdp==1 | prob_2_33hdp==1 | prob_2_31other==1 | prob_2_32other==1 | prob_2_33other==1 
g prob_24= prob_2_42hdp==1 | prob_2_42other==1
g prob_31 = prob_3_1other==1 | prob_3_2other==1 | prob_3_1hdp==1 | prob_3_2hdp==1
g probo=prob_1==1 | prob_21==1 | prob_23==1 | prob_24==1 | prob_31==1 

g gtyprob=.
replace gtyprob=gty if probo==0
g gtyimpu=.
replace gtyimpu=gty if impu==0
g gtyipc=.
replace gtyipc=gty if cs8ahdp==0 & cs8aother==0

g csprob=.
replace csprob=csr0 if probo==0
g csimpu=.
replace csimpu=csr0 if impu==0
g csipc=.
replace csipc=csr0 if cs8ahdp==0 & cs8aother==0

local varlist "gty gtyprob gtyimpu gtyipc"
foreach var in `varlist' {
	tab `var', matcell(`var')
}

local varlist "csr0 csprob csimpu csipc"
matrix t6=J(4,2,.)
local irow=0
foreach var in `varlist' {
	local ++irow
	quietly sum `var'
	matrix t6[`irow',1]=r(mean)
	matrix t6[`irow',2]=r(sd)
}
matrix list t6
}

* Appendix Table C2
local varlist "ddcts cs9bx prmsa pcts nprmsa fr1"
foreach i in `varlist'{
	sum `i'other
}
foreach i in `varlist'{
	sum `i'hdp
}
local varlist "hsa g43ann hra g41"
foreach i in `varlist'{
	sum `i'
}

local varlist "largefirm age50 hiincome private union"
foreach i in `varlist' {
	sum `i'
}