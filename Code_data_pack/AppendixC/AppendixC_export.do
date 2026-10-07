
set more off
clear

cd "\AppendixC\"

do AppendixC_clean.do

export excel ddcts cs7x5 cs7b cs9bx cs27 ddct3c cs27a cs8x5 cs8c cs8e ddct3b cs6 cs6a cs6b cs18 cs18a cs18b cs8x4 cs8x6 cs8a cs8b cs29 cs30a cs30aa cs30ab cs30b cs30ba cs30bb cs30c cs30ca cs30cb cs30d cs30da cs30db cs7x4 cs7x1 cs7d cs8x1 cs8d grdfthr cs18c cs30ab1 cs30bb1 cs30cb1 cs30db1 cs32 cs6c cs7a cs7x2 cs7x3 cs8f cs8x2 cs8x3 prob_1_11 prob_1_12 prob_1_13 prob_1_14 prob_1_15 prob_1_16 prob_1_21 prob_1_22 prob_1_23 prob_1_24 prob_1_25 prob_1_26 prob_1_31 prob_1_32 prob_1_33 prob_1_34 prob_1_35 prob_1_36 prob_2_11 prob_2_12 prob_2_13 prob_2_14 prob_2_21 prob_2_22 prob_2_31 prob_2_32 prob_2_33 prob_2_4 prob_3_1 prob_3_2 prob_3_3 firm pty planindex prmsa sd1 g41 prob_5_1 prob_5_2 pcts prob_3_4 prob_3_5 using "runavc_allplan.xlsx", sheet("raw") sheetmodify cell(A1) nolabel firstrow(variables)
