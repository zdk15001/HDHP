* Date created: 29 June 2016
* Date modified: 29 July 2016
* Chenyuan Liu

*************************************************************
* LOAD IN THE DATA FOR KFF Employer Health Survey Data 2015
*************************************************************
set more off
cd "\AppendixC\"

use "health benefits 2015.dta", clear

******************
* CLEANING DATASET
******************
{
gen firm =_n
gen hdhp3=hdhp
replace hdhp3=(hdhp==1 | hdhp==2)
replace hdhp3=0 if (hdhp3!=1)
gen pty=0
replace pty=1 if hdhp3==1 & hmo==0 & ppo==0 & pos==0
replace pty=2 if hdhp3==0 & hmo==1 & ppo==0 & pos==0
replace pty=3 if hdhp3==0 & hmo==0 & ppo==1 & pos==0
replace pty=4 if hdhp3==0 & hmo==0 & ppo==0 & pos==1
replace pty=5 if hdhp3==1 & hmo==1 & ppo==0 & pos==0
replace pty=6 if hdhp3==1 & hmo==0 & ppo==1 & pos==0
replace pty=7 if hdhp3==1 & hmo==0 & ppo==0 & pos==1
replace pty=8 if hdhp3==0 & hmo==1 & ppo==1 & pos==0
replace pty=9 if hdhp3==0 & hmo==1 & ppo==0 & pos==1
replace pty=10 if hdhp3==0 & hmo==0 & ppo==1 & pos==1
replace pty=11 if hdhp3==0 & hmo==1 & ppo==1 & pos==1
replace pty=12 if hdhp3==1 & hmo==0 & ppo==1 & pos==1
replace pty=13 if hdhp3==1 & hmo==1 & ppo==0 & pos==1
replace pty=14 if hdhp3==1 & hmo==1 & ppo==1 & pos==0
replace pty=15 if hdhp3==1 & hmo==1 & ppo==1 & pos==1

// drop firms with no insurance coverage /refuse to answer question
drop if pty==0
// drop firms offer no plan
drop if d6==. & e6==. & f6==. & g6==. 

// Appendix Table C1
matrix AC1 = J(4,2,.)
count 
matrix AC1[1,1] = r(N)
count if pty==5 | pty==6 | pty==7 
matrix AC1[2,1] = r(N)
g numpl = (d6 != .) + (e6 != .) + (f6 != .) + (g6 != .)
egen totalpl = total(numpl)
sum totalpl 
matrix AC1[1,2] = r(mean)
matrix AC1[2,2] = 2*AC1[2,1]
drop numpl totalpl


//drop obs with no saving option informaiton (missing value for hsa/hra contribution). Note: missing value is not 0 contribution. 0 contribution will have a 0 in g41/g43.
drop if g43==. & g41==. & (hdhp3==1) 

//drop obs with missing policy info (e6 (ov), e9 (moop))
drop if g6==. & (hdhp3==1)
drop if d6==. & (hmo==1)
drop if e6==. & (ppo==1)
drop if f6==. & (pos==1) 
drop if g9==. & (hdhp3==1)
drop if d9==. & (hmo==1)
drop if e9==. & (ppo==1)
drop if f9==. & (pos==1) 

//drop obs with plan information not consistent with pty
drop if pty==1 & (d6!=. | e6!=. | f6!=.)
drop if pty==2 & (g6!=. | e6!=. | f6!=.)
drop if pty==3 & (d6!=. | g6!=. | f6!=.)
drop if pty==4 & (d6!=. | e6!=. | g6!=.)
drop if pty==5 & (e6!=. | f6!=.)
drop if pty==6 & (d6!=. | f6!=.)
drop if pty==7 & (e6!=. | d6!=.) 
drop if pty==8 & (g6!=. | f6!=.)
drop if pty==9 & (g6!=. | e6!=.)
drop if pty==10 & (g6!=. | d6!=.) 
drop if pty==11 & g6!=.
drop if pty==12 & d6!=.
drop if pty==13 & e6!=.
drop if pty==14 & f6!=.

count if pty==5 | pty==6 | pty==7
matrix AC1[3,1] = r(N)
matrix AC1[3,2] = 2*AC1[3,1]
matrix AC1[4,1] = 331
matrix AC1[4,2] = 662

//drop if firm==560 //this firm offer exactly same two plans

//drop obs if no MOOP or larger than $6850
drop if (e9==2 | e9bx>6850) & ppo==1
drop if (d9==2 | d9bx>6850) & hmo==1
drop if (f9==2 | f9bx>6850) & pos==1
drop if (g9==2 | g9bx>6850) & hdhp3==1  

// drop plans with ddct not divisible by 5
drop if mod(ddctshdp,5)!=0 & (hdhp3==1)
drop if mod(ddctshmo,5)!=0 & (hmo==1)
drop if mod(ddctsppo,5)!=0 & (ppo==1)
drop if mod(ddctspos,5)!=0 & (pos==1) 

* some data manipulation so that makes the cleaning a lot easier
// Replace missing values in copay/coinsurance by 0
replace g27a=0 if g27a==.
replace d27a=0 if d27a==.
replace e27a=0 if e27a==.
replace f27a=0 if f27a==.

replace g6a=0 if g6a==.
replace g6b=0 if g6b==.
replace g18a=0 if g18a==.
replace g18b=0 if g18b==.
replace g8a=0 if g8a==.
replace g8b=0 if g8b==.
replace g8c=0 if g8c==.
replace g8d=0 if g8d==.
replace g7a=0 if g7a==.
replace g7b=0 if g7b==. 
replace g7d=0 if g7d==. 

replace d6a=0 if d6a==.
replace d6b=0 if d6b==.
replace d18a=0 if d18a==.
replace d18b=0 if d18b==.
replace d8a=0 if d8a==.
replace d8b=0 if d8b==.
replace d8c=0 if d8c==.
replace d8d=0 if d8d==.
replace d7a=0 if d7a==.
replace d7b=0 if d7b==. 
replace d7d=0 if d7d==. 

replace e6a=0 if e6a==.
replace e6b=0 if e6b==.
replace e18a=0 if e18a==.
replace e18b=0 if e18b==.
replace e8a=0 if e8a==.
replace e8b=0 if e8b==.
replace e8c=0 if e8c==.
replace e8d=0 if e8d==.
replace e7a=0 if e7a==.
replace e7b=0 if e7b==.
replace e7d=0 if e7d==.

replace f6a=0 if f6a==.
replace f6b=0 if f6b==.
replace f18a=0 if f18a==.
replace f18b=0 if f18b==.
replace f8a=0 if f8a==.
replace f8b=0 if f8b==.
replace f8c=0 if f8c==.
replace f8d=0 if f8d==.
replace f7a=0 if f7a==.
replace f7b=0 if f7b==. 
replace f7d=0 if f7d==. 

//drop plans with ddct larger than moop
drop if ddctshdp+g27a>g9bx & (hdhp3==1)
drop if ddctshmo+d27a>d9bx & (hmo==1)
drop if ddctsppo+e27a>e9bx & (ppo==1)
drop if ddctspos+f27a>f9bx & (pos==1) 
//ddct=moop but there is cost sharing rule for some services
drop if ddctshdp+g27a==g9bx & (g6==1|g6==2|g18==1|g18==2|g29==1 | g29==2|g29==3|g29==4|g7a!=0|g7b!=0|g7d!=0|g8a!=0|g8b!=0|g8c!=0|g8d!=0) & (hdhp3==1)
drop if ddctshmo+d27a==d9bx & (d6==1|d6==2|d18==1|d18==2|d29==1 | d29==2|d29==3|d29==4|d7a!=0|d7b!=0|d7d!=0|d8a!=0|d8b!=0|d8c!=0|d8d!=0) & (hmo==1)
drop if ddctsppo+e27a==e9bx & (e6==1|e6==2|e18==1|e18==2|e29==1 | e29==2|e29==3|e29==4|e7a!=0|e7b!=0|e7d!=0|e8a!=0|e8b!=0|e8c!=0|e8d!=0) & (ppo==1)
drop if ddctspos+f27a==f9bx & (f6==1|f6==2|f18==1|f18==2|f29==1 | f29==2|f29==3|f29==4|f7a!=0|f7b!=0|f7d!=0|f8a!=0|f8b!=0|f8c!=0|f8d!=0) & (pos==1) 
//ddct<moop but no cost sharing rule for any service
drop if ddctshdp+g27a<g9bx & ((g6==3 | g6==4) & (g18==3 | g18==4) & (g29==5 | g29==6) & (g7a==0 & g7b==0) & (g8a==0 & g8b==0 & g8c==0) & g7d==0 & g8d==0) & (hdhp3==1)
drop if ddctshmo+d27a<d9bx & ((d6==3 | d6==4) & (d18==3 | d18==4) & (d29==5 | d29==6) & (d7a==0 & d7b==0) & (d8a==0 & d8b==0 & d8c==0) & d7d==0 & d8d==0) & (hmo==1)
drop if ddctsppo+e27a<e9bx & ((e6==3 | e6==4) & (e18==3 | e18==4) & (e29==5 | e29==6) & (e7a==0 & e7b==0) & (e8a==0 & e8b==0 & e8c==0) & e7d==0 & e8d==0) & (ppo==1)
drop if ddctspos+f27a<f9bx & ((f6==3 | f6==4) & (f18==3 | f18==4) & (f29==5 | f29==6) & (f7a==0 & f7b==0) & (f8a==0 & f8b==0 & f8c==0) & f7d==0 & f8d==0) & (pos==1)  
}

*********************************************************************
* RESTRUCTURING THE DATASET FROM WIDE TO LONG. NOW EACH OBS IS A PLAN
*********************************************************************
{
gen g2=.
gen g4=.
rename b12b pctcvrd1
rename d1 fr11
rename d11a bi11a1
rename d11a2 bi11a21
rename d11a3 prm11a31
rename d11a4 prm11a41
rename d11b bi11b1
rename d11c bi11c1
rename d11e bi11e1
rename d13a3 bi13a31
rename d13a4 bi13a41
rename d13b bi13b1
rename d15 oly1
rename d18 cs181
rename d18a cs18a1
rename d18acat cs18acat1
rename d18b cs18b1
rename d18bcat cs18bcat1
rename d18c cs18c1
rename d19 grdfthr1
rename d1a fr1a1
rename d1b fr1b1
rename d1c fr1c1
rename d1d fr1d1
rename d2 ddct21
rename d27 cs271
rename d27a cs27a1
rename d29 cs291
rename d29a cs29a1
rename d30a cs30a1
rename d30aa cs30aa1
rename d30ab cs30ab1
rename d30ab1 cs30ab11
rename d30b cs30b1
rename d30ba cs30ba1
rename d30bb cs30bb1
rename d30bb1 cs30bb11
rename d30c cs30c1
rename d30ca cs30ca1
rename d30cb cs30cb1
rename d30cb1 cs30cb11
rename d30d cs30d1
rename d30da cs30da1
rename d30db cs30db1
rename d30db1 cs30db11
rename d32 cs321
rename d3b ddct3b1
rename d3c ddct3c1
rename d4 ddct41
rename d40 cs401
rename d5 ddct51
rename d50 bi501
rename d51 bi511
rename d5a ddct5a1
rename d5a1 ddct5a11
rename d6 cs61
rename d6a cs6a1
rename d6acat cs6acat1
rename d6b cs6b1
rename d6bcat cs6bcat1
rename d6c cs6c1
rename d7a cs7a1
rename d7acat cs7acat1
rename d7b cs7b1
rename d7bcat cs7bcat1
rename d7d cs7d1
rename d7x1 cs7x11
rename d7x2 cs7x21
rename d7x3 cs7x31
rename d7x4 cs7x41
rename d7x5 cs7x51
rename d8a cs8a1
rename d8b cs8b1
rename d8c cs8c1
rename d8d cs8d1
rename d8e cs8e1
rename d8f cs8f1
rename d8x1 cs8x11
rename d8x2 cs8x21
rename d8x3 cs8x31
rename d8x4 cs8x41
rename d8x5 cs8x51
rename d8x6 cs8x61
rename d9 cs91
rename d9bx cs9bx1
rename d9bxcat cs9bxcat1
rename ddctfhmo ddctf1
rename ddctshmo ddcts1
rename empfhmoa empfa1
rename empshmoa empsa1
rename epctfhmo epctf1
rename epctshmo epcts1
rename hashighhmo hashigh1
rename hashighhmo2 hashigh21
rename hmo ofp1
rename hmo2 ofenr1
rename hmoffull prmffull1
rename hmosfull prmsfull1
rename hmowt pwt1
rename pctfhmo pctf1
rename pctshmo pcts1
rename prmfhmoa prmfa1
rename prmmfhmo prmmf1
rename prmmshmo prmms1
rename prmshmoa prmsa1
rename rdctfhmo rdctf1
rename rdctshmo rdcts1
rename wkrfhmo wkrf1
rename wkrfhmoa wkrfa1
rename wkrshmo wkrs1
rename wkrshmoa wkrsa1
rename b12c pctcvrd2
rename e1 fr12
rename e11a bi11a2
rename e11a2 bi11a22
rename e11a3 prm11a32
rename e11a4 prm11a42
rename e11b bi11b2
rename e11c bi11c2
rename e11e bi11e2
rename e13a3 bi13a32
rename e13a4 bi13a42
rename e13b bi13b2
rename e15 oly2
rename e18 cs182
rename e18a cs18a2
rename e18acat cs18acat2
rename e18b cs18b2
rename e18bcat cs18bcat2
rename e18c cs18c2
rename e19 grdfthr2
rename e1a fr1a2
rename e1b fr1b2
rename e1c fr1c2
rename e1d fr1d2
rename e2 ddct22
rename e27 cs272
rename e27a cs27a2
rename e29 cs292
rename e29a cs29a2
rename e30a cs30a2
rename e30aa cs30aa2
rename e30ab cs30ab2
rename e30ab1 cs30ab12
rename e30b cs30b2
rename e30ba cs30ba2
rename e30bb cs30bb2
rename e30bb1 cs30bb12
rename e30c cs30c2
rename e30ca cs30ca2
rename e30cb cs30cb2
rename e30cb1 cs30cb12
rename e30d cs30d2
rename e30da cs30da2
rename e30db cs30db2
rename e30db1 cs30db12
rename e32 cs322
rename e3b ddct3b2
rename e3c ddct3c2
rename e4 ddct42
rename e40 cs402
rename e5 ddct52
rename e50 bi502
rename e51 bi512
rename e5a ddct5a2
rename e5a1 ddct5a12
rename e6 cs62
rename e6a cs6a2
rename e6acat cs6acat2
rename e6b cs6b2
rename e6bcat cs6bcat2
rename e6c cs6c2
rename e7a cs7a2
rename e7acat cs7acat2
rename e7b cs7b2
rename e7bcat cs7bcat2
rename e7d cs7d2
rename e7x1 cs7x12
rename e7x2 cs7x22
rename e7x3 cs7x32
rename e7x4 cs7x42
rename e7x5 cs7x52
rename e8a cs8a2
rename e8b cs8b2
rename e8c cs8c2
rename e8d cs8d2
rename e8e cs8e2
rename e8f cs8f2
rename e8x1 cs8x12
rename e8x2 cs8x22
rename e8x3 cs8x32
rename e8x4 cs8x42
rename e8x5 cs8x52
rename e8x6 cs8x62
rename e9 cs92
rename e9bx cs9bx2
rename e9bxcat cs9bxcat2
rename ddctfppo ddctf2
rename ddctsppo ddcts2
rename empfppoa empfa2
rename empsppoa empsa2
rename epctfppo epctf2
rename epctsppo epcts2
rename hashighppo hashigh2
rename hashighppo2 hashigh22
rename ppo ofp2
rename ppo2 ofenr2
rename ppoffull prmffull2
rename pposfull prmsfull2
rename ppowt pwt2
rename pctfppo pctf2
rename pctsppo pcts2
rename prmfppoa prmfa2
rename prmmfppo prmmf2
rename prmmsppo prmms2
rename prmsppoa prmsa2
rename rdctfppo rdctf2
rename rdctsppo rdcts2
rename wkrfppo wkrf2
rename wkrfppoa wkrfa2
rename wkrsppo wkrs2
rename wkrsppoa wkrsa2
rename b12d pctcvrd3
rename f1 fr13
rename f11a bi11a3
rename f11a2 bi11a23
rename f11a3 prm11a33
rename f11a4 prm11a43
rename f11b bi11b3
rename f11c bi11c3
rename f11e bi11e3
rename f13a3 bi13a33
rename f13a4 bi13a43
rename f13b bi13b3
rename f15 oly3
rename f18 cs183
rename f18a cs18a3
rename f18acat cs18acat3
rename f18b cs18b3
rename f18bcat cs18bcat3
rename f18c cs18c3
rename f19 grdfthr3
rename f1a fr1a3
rename f1b fr1b3
rename f1c fr1c3
rename f1d fr1d3
rename f2 ddct23
rename f27 cs273
rename f27a cs27a3
rename f29 cs293
rename f29a cs29a3
rename f30a cs30a3
rename f30aa cs30aa3
rename f30ab cs30ab3
rename f30ab1 cs30ab13
rename f30b cs30b3
rename f30ba cs30ba3
rename f30bb cs30bb3
rename f30bb1 cs30bb13
rename f30c cs30c3
rename f30ca cs30ca3
rename f30cb cs30cb3
rename f30cb1 cs30cb13
rename f30d cs30d3
rename f30da cs30da3
rename f30db cs30db3
rename f30db1 cs30db13
rename f32 cs323
rename f3b ddct3b3
rename f3c ddct3c3
rename f4 ddct43
rename f40 cs403
rename f5 ddct53
rename f50 bi503
rename f51 bi513
rename f5a ddct5a3
rename f5a1 ddct5a13
rename f6 cs63
rename f6a cs6a3
rename f6acat cs6acat3
rename f6b cs6b3
rename f6bcat cs6bcat3
rename f6c cs6c3
rename f7a cs7a3
rename f7acat cs7acat3
rename f7b cs7b3
rename f7bcat cs7bcat3
rename f7d cs7d3
rename f7x1 cs7x13
rename f7x2 cs7x23
rename f7x3 cs7x33
rename f7x4 cs7x43
rename f7x5 cs7x53
rename f8a cs8a3
rename f8b cs8b3
rename f8c cs8c3
rename f8d cs8d3
rename f8e cs8e3
rename f8f cs8f3
rename f8x1 cs8x13
rename f8x2 cs8x23
rename f8x3 cs8x33
rename f8x4 cs8x43
rename f8x5 cs8x53
rename f8x6 cs8x63
rename f9 cs93
rename f9bx cs9bx3
rename f9bxcat cs9bxcat3
rename ddctfpos ddctf3
rename ddctspos ddcts3
rename empfposa empfa3
rename empsposa empsa3
rename epctfpos epctf3
rename epctspos epcts3
rename hashighpos hashigh3
rename hashighpos2 hashigh23
rename pos ofp3
rename pos2 ofenr3
rename posffull prmffull3
rename possfull prmsfull3
rename poswt pwt3
rename pctfpos pctf3
rename pctspos pcts3
rename prmfposa prmfa3
rename prmmfpos prmmf3
rename prmmspos prmms3
rename prmsposa prmsa3
rename rdctfpos rdctf3
rename rdctspos rdcts3
rename wkrfpos wkrf3
rename wkrfposa wkrfa3
rename wkrspos wkrs3
rename wkrsposa wkrsa3
rename b12e pctcvrd4
rename g1 fr14
rename g11a bi11a4
rename g11a2 bi11a24
rename g11a3 prm11a34
rename g11a4 prm11a44
rename g11b bi11b4
rename g11c bi11c4
rename g11e bi11e4
rename g13a3 bi13a34
rename g13a4 bi13a44
rename g13b bi13b4
rename g15 oly4
rename g18 cs184
rename g18a cs18a4
rename g18acat cs18acat4
rename g18b cs18b4
rename g18bcat cs18bcat4
rename g18c cs18c4
rename g19 grdfthr4
rename g1a fr1a4
rename g1b fr1b4
rename g1c fr1c4
rename g1d fr1d4
rename g2 ddct24
rename g27 cs274
rename g27a cs27a4
rename g29 cs294
rename g29a cs29a4
rename g30a cs30a4
rename g30aa cs30aa4
rename g30ab cs30ab4
rename g30ab1 cs30ab14
rename g30b cs30b4
rename g30ba cs30ba4
rename g30bb cs30bb4
rename g30bb1 cs30bb14
rename g30c cs30c4
rename g30ca cs30ca4
rename g30cb cs30cb4
rename g30cb1 cs30cb14
rename g30d cs30d4
rename g30da cs30da4
rename g30db cs30db4
rename g30db1 cs30db14
rename g32 cs324
rename g3b ddct3b4
rename g3c ddct3c4
rename g40 ddct44
rename g40 cs404
rename g5 ddct54
rename g50 bi504
rename g51 bi514
rename g5a ddct5a4
rename g5a1 ddct5a14
rename g6 cs64
rename g6a cs6a4
rename g6acat cs6acat4
rename g6b cs6b4
rename g6bcat cs6bcat4
rename g6c cs6c4
rename g7a cs7a4
rename g7acat cs7acat4
rename g7b cs7b4
rename g7bcat cs7bcat4
rename g7d cs7d4
rename g7x1 cs7x14
rename g7x2 cs7x24
rename g7x3 cs7x34
rename g7x4 cs7x44
rename g7x5 cs7x54
rename g8a cs8a4
rename g8b cs8b4
rename g8c cs8c4
rename g8d cs8d4
rename g8e cs8e4
rename g8f cs8f4
rename g8x1 cs8x14
rename g8x2 cs8x24
rename g8x3 cs8x34
rename g8x4 cs8x44
rename g8x5 cs8x54
rename g8x6 cs8x64
rename g9 cs94
rename g9bx cs9bx4
rename g9bxcat cs9bxcat4
rename ddctfhdp ddctf4
rename ddctshdp ddcts4
rename empfhdpa empfa4
rename empshdpa empsa4
rename epctfhdp epctf4
rename epctshdp epcts4
rename hashighhdp hashigh4
rename hashighhdp2 hashigh24
rename hdhp ofp4
rename hdhp2 ofenr4
rename hdpffull prmffull4
rename hdpsfull prmsfull4
rename hdpwt pwt4
rename pctfhdp pctf4
rename pctshdp pcts4
rename prmfhdpa prmfa4
rename prmmfhdp prmmf4
rename prmmshdp prmms4
rename prmshdpa prmsa4
rename rdctfhdp rdctf4
rename rdctshdp rdcts4
rename wkrfhdp wkrf4
rename wkrfhdpa wkrfa4
rename wkrshdp wkrs4
rename wkrshdpa wkrsa4
}

******************************
* GENERATE MORE VARIABLES
******************************
{
//generate firm level variables
g firmld=pty!=1
g firmhd=hdhp3==1 
g largefirm=size==6
replace age50=age50-1
replace hiincome=hiincome-1
g private=a10==1
g union=2-a4


matrix t0=J(6,2,.)
local irow=0
local varlist "largefirm age50 hiincome private union"
foreach var in `varlist' {
	local ++irow
	quietly sum `var' if firmld==1
	matrix t0[`irow',1]=r(mean)
	*local ++irow
	*matrix t0[`irow',1]=r(sd)
}
local ++irow
matrix t0[`irow',1]=r(N)

local irow=0
local varlist "largefirm age50 hiincome private union"
foreach var in `varlist' {
	local ++irow
	quietly sum `var' if firmhd==1
	matrix t0[`irow',2]=r(mean)
	*local ++irow
	*matrix t0[`irow',2]=r(sd)
}
local ++irow
matrix t0[`irow',2]=r(N)
matrix list t0

drop firmld firmhd
}

//generate plan level variables
{
gen pty2=pty
drop pty
reshape long pctcvrd@ fr1@ bi11a@ bi11a2@ prm11a3@ prm11a4@ bi11b@ bi11c@ bi11e@ bi13a3@ bi13a4@ bi13b@ oly@ cs18@ cs18a@ cs18acat@ cs18b@ cs18bcat@ cs18c@ grdfthr@ fr1a@ fr1b@ fr1c@ fr1d@ ddct2@ cs27@ cs27a@ cs29@ cs29a@ cs30a@ cs30aa@ cs30ab@ cs30ab1@ cs30b@ cs30ba@ cs30bb@ cs30bb1@ cs30c@ cs30ca@ cs30cb@ cs30cb1@ cs30d@ cs30da@ cs30db@ cs30db1@ cs32@ ddct3b@ ddct3c@ ddct4@ cs40@ ddct5@ bi50@ bi51@ ddct5a@ ddct5a1@ cs6@ cs6a@ cs6acat@ cs6b@ cs6bcat@ cs6c@ cs7a@ cs7acat@ cs7b@ cs7bcat@ cs7d@ cs7x1@ cs7x2@ cs7x3@ cs7x4@ cs7x5@ cs8a@ cs8b@ cs8c@ cs8d@ cs8e@ cs8f@ cs8x1@ cs8x2@ cs8x3@ cs8x4@ cs8x5@ cs8x6@ cs9@ cs9bx@ cs9bxcat@ ddctf@ ddcts@ empfa@ empsa@ epctf@ epcts@ hashigh@ hashigh2@ ofp@ ofenr@ prmffull@ prmsfull@ pwt@ pctf@ pcts@ prmfa@ prmmf@ prmms@ prmsa@ rdctf@ rdcts@ wkrf@ wkrfa@ wkrs@ wkrsa@, i(firm) j(pty)
drop if cs6==. //drop not offering plans

g pty3=pty
replace pty3=1 if (pty!=4)
g nprmsa=pcts*prmsa

replace fr1=fr1-1
g hsa= (pty==4 & g43ann!=.)
g hra=(pty==4 & g41!=.)

matrix t2 = J(23, 2, .)
local irow=0
local varlist "ddcts cs9bx prmsa pcts nprmsa fr1"
foreach var in `varlist' {
	quietly sum `var' if pty != 4
	local ++irow
	matrix t2[`irow', 1] = r(mean)
	local ++irow
	matrix t2[`irow', 1] = r(sd)
}
matrix t2[23,1] = r(N)
local irow=0
local varlist "ddcts cs9bx prmsa pcts nprmsa fr1 hsa g43ann hra g41"
foreach var in `varlist' {
	quietly sum `var' if pty == 4
	local ++irow
	matrix t2[`irow', 2] = r(mean)
	local ++irow
	matrix t2[`irow', 2] = r(sd)
}
sum ddcts if pty == 4
matrix t2[23,2] = r(N)
}

****************
* CREATING FLAGS
****************
{
gen byte prob_1_11 = cs6c==1 //Coinsurance structure-max Primary office visits
gen byte prob_1_12 = cs18c==1 //Coinsurance structure-max Specialty office visits
gen byte prob_1_13 = cs30ab1==1 //Coinsurance structure-max Drug tier 1
gen byte prob_1_14 = cs30bb1==1 //Coinsurance structure-max Drug tier 2
gen byte prob_1_15 = cs30cb1==1 //Coinsurance structure-max Drug tier 3
gen byte prob_1_16 = cs30db1==1 //Coinsurance structure-max Drug tier 4
gen byte prob_1_21 = cs6c==2 //Coinsurance structure-min Primary office visits
gen byte prob_1_22 = cs18c==2 //Coinsurance structure-min Specialty office visits
gen byte prob_1_23 = cs30ab1==2 //Coinsurance structure-min Drug tier 1
gen byte prob_1_24 = cs30bb1==2 //Coinsurance structure-min Drug tier 2
gen byte prob_1_25 = cs30cb1==2 //Coinsurance structure-min Drug tier 3
gen byte prob_1_26 = cs30db1==2 //Coinsurance structure-min Drug tier 4
gen byte prob_1_31 = cs6c==3 //Coinsurance - both min and max Primary office visits
gen byte prob_1_32 = cs18c==3 //Coinsurance - both min and max Specialty office visits
gen byte prob_1_33 = cs30ab1==3 //Coinsurance - both min and max Drug tier 1
gen byte prob_1_34 = cs30bb1==3 //Coinsurance - both min and max Drug tier 2
gen byte prob_1_35 = cs30cb1==3 //Coinsurance - both min and max Drug tier 3
gen byte prob_1_36 = cs30db1==3 //Coinsurance - both min and max Drug tier 4
gen byte prob_2_11 = cs8x4==1 //Cost sharing structure - both copay and coinsurance inpatient
gen byte prob_2_12 = cs7x4==1 //Cost sharing structure - both copay and coinsurance Outpatient surgery
gen byte prob_2_13 = cs8x5==1 & cs8x3==1 //Cost sharing structure - both copay and coinsurance Both per day copay and coinsurance for inpatient service
gen byte prob_2_14 = cs8x5==1 //Cost sharing structure - both copay and coinsurance Plan has per day copay for inpatient services
gen byte prob_2_15=(firm==998)|(firm==3395) //firm with per diem copay adjusted
gen byte prob_2_21 = cs8x4==2 //Cost sharing structure - either copay and coinsurance, whichever is greater inpatient
gen byte prob_2_22 = cs7x4==2 //Cost sharing structure - either copay and coinsurance, whichever is greater Outpatient surgery
gen byte prob_2_31 = cs6==4 //Cost sharing structure - none of the above Primary office visits
gen byte prob_2_32 = cs18==4 //Cost sharing structure - none of the above Specialty office visits
gen byte prob_2_33 = cs29==6 //Cost sharing structure - none of the above drug
gen byte prob_2_42 = cs7x3==2 & cs8x3==1 & cs7x2==1 //plans with outpatient coinsurance rate replaced by inpatient for AV calculation
gen byte prob_3_1 = cs8x1==1 //Separate deductible Inpatient
gen byte prob_3_2 = cs7x1==1 //Separate deductible Outpatient surgery
gen byte prob_3_3 = cs27==1 & (ddct3c==1 | ddct3c==.) //Drug having separate ddct and general ddct 
gen byte prob_3_4 = (ddct3c==. | ddct3b==.) & pty!=4
gen byte prob_3_5 = (ddct3b==. | ddct3c==.) & pty==4
}

*********************************************
* REPLACE MISSING VALUES AND CREATING NEW VAR
*********************************************
{
replace cs27a = 0 if cs27a==. //if no seperate drug ddct, then seperate drug ddct=0
replace cs6a=0 if cs6a==.
replace cs6b=0 if cs6b==.
replace cs18a=0 if cs18a==.
replace cs18b=0 if cs18b==.
replace cs8a=0 if cs8a==.
replace cs8b=0 if cs8b==.
replace cs8c=0 if cs8c==.
replace cs7a=0 if cs7a==.
replace cs7b=0 if cs7b==. // Replace missing values in copay/coinsurance by 0
replace cs6=3 if cs6==4
replace cs18=3 if cs18==4
replace cs29=5 if cs29==6 
replace cs30a=3 if cs30a==4 //replace "none of the above" in cost sharing by "no cost sharing"
replace cs30b=3 if cs30b==4 
replace cs30c=3 if cs30c==4 
replace cs30d=3 if cs30d==4 //replace "copay or coins + difference" and "some other amount" in cost sharing by "no cost sharing", according to copay/coinsurance rate
replace cs32=1 if cs32==. //treat missing value in coverage for specialty drug as "Yes"
replace cs30b=cs30a  if cs29==4
replace cs30ba=cs30aa if cs29==4
replace cs30bb=cs30ab if cs29==4
replace cs30bb1=cs30ab1 if cs29==4  //replace tier-2 data with tier-1 data if cost sharing rule is the same for all tiers
replace cs30c=cs30b  if cs29==4 | cs29==3
replace cs30ca=cs30ba if cs29==4 | cs29==3
replace cs30cb=cs30bb if cs29==4 | cs29==3
replace cs30cb1=cs30bb1 if cs29==4 | cs29==3 //replace tier-3 data with tier-2 data if cost sharing rule is the same for all tiers or 2-tier cost sharing rule is the same for all tiers
replace cs30d=cs30c  if cs29==4 | cs29==3 | cs29==2
replace cs30da=cs30ca if cs29==4 | cs29==3| cs29==2
replace cs30db=cs30cb if cs29==4 | cs29==3| cs29==2
replace cs30db1=cs30cb1 if cs29==4 | cs29==3 | cs29==2 //replace tier-4 data with tier-3 data if cost sharing rule is the same for all tiers or 2-tier cost sharing rule
replace cs30da=0 if cs32==2
replace cs30db=100 if cs32==2 //not covering specialty drugs -> 100% enrollee coinsurance rate for tier-4 drug
replace cs30aa=0 if cs30aa==. 
replace cs30ba=0 if cs30ba==. 
replace cs30ca=0 if cs30ca==. 
replace cs30da=0 if cs30da==.  //replace missing copay as 0
replace cs30ab=0 if cs30ab==. 
replace cs30bb=0 if cs30bb==. 
replace cs30cb=0 if cs30cb==. 
replace cs30db=0 if cs30db==.  //replace missing coinsurance as 0
replace cs8x5=2 if firm==3395 & pty==3 
replace cs8c=0 if firm==3395 & pty==3 // adjust for copay per diem data
replace g41=0 if g41==.
replace g43ann=0 if g43ann==. //if a plan don't have saving account contribution, replace it with $0 contribution

replace ddct3c=1 if ddct3c==. //if xx service subject to general ddct is missing for other plans, it happens along with 0 ddct, so it should be "yes"
replace ddct3b=1 if ddct3b==. 

gen byte prob_5_1 = cs27a+ddcts>cs9bx //ddct larger than moop
gen byte prob_5_2 = (ddcts+cs27a==cs9bx)&(cs6==1|cs6==2|cs18==1|cs18==2|cs29==1 | cs29==2|cs29==3|cs29==4) //ddct=moop but there is cost sharing rule for some services
gen byte prob_5_3 = (ddcts+cs27a<cs9bx)&(cs6==3&cs18==3&cs29==5&cs7a==0&cs7b==0&cs8a==0&cs8b==0&cs8c==0) //ddct<moop 
gen byte prob_4_4= mod(ddcts,5)!=0
drop if prob_4_4==1
drop if prob_5_1==1
drop if prob_5_2==1 
drop if prob_5_3==1 //drop plans with these problems. Note, this only drops the plan, but not the firm. 
replace cs7b=cs8b if cs7x3==2 & cs8x3==1 & cs7x2==1 //replace outpatient coinsurance rate if it only has copay for outpatient and it has coinsurance for inpatient services
gen byte prob_2_4 = cs7x2==1 & cs7x3==2 &cs8x3==2 //Cost sharing structure - copay for outpatient Only copay, no coinsurance
bysort firm: egen prob_2_4_t=max(prob_2_4)
drop if prob_2_4_t==1 //drop firms with plans with only copay in outpatient surgery

gen sd1=prmsa*pcts //annual premiums for single coverage*worker% contribution for single coverage=work's annual net pay for premium of single coverage
replace sd1=sd1-g43ann if ofp==2 & pty==4 //treat firm's deposit into HSA account as a deduction of premium. 
gen planindex = _n
}


