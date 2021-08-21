*need to install texresults and esttab and winsor2

cd "C:/Users/kamil/Desktop/David" 
use dataforregression_mock.dta, clear





tab hastdc
****TABLE 1

*sum stats for all accounts
estpost tabstat bage bincome btenure badbchecking hastdc binterest bsaldotdc bcreditlimit, /*         
*/  statistics( mean sd p25 p50 p75 ) /*
*/  columns(statistics)
esttab using sumstat.tex,  /*
 */ cells("mean(fmt(%10.2fc)) sd(fmt(%10.2fc)) p25(fmt(%10.2fc)) p50(fmt(%10.2fc)) p75(fmt(%10.2fc))") collabels("\small{Mean}" "\small{Std dev}" "\small{P25}" "\small{P50}" "\small{P75}") /*
 */  replace label nonum noobs nodep nomtitles fragment
 
*sum stats for accounts with tdc
estpost tabstat bage bincome btenure badbchecking binterest bsaldotdc bcreditlimit if hastdc==1, /*
*/  statistics( mean sd p25 p50 p75 ) /*
*/  columns(statistics)
esttab using sumstathastdc.tex,  /*
*/ cells("mean(fmt(%10.2fc)) sd(fmt(%10.2fc)) p25(fmt(%10.2fc)) p50(fmt(%10.2fc)) p75(fmt(%10.2fc))") collabels("\small{Mean}" "\small{Std dev}" "\small{P25}" "\small{P50}" "\small{P75}") /*
 */  replace label nonum noobs nodep nomtitles fragment

 
 *sum stats for accounts with CC interest
estpost tabstat bage bincome btenure badbchecking binterest bsaldotdc bcreditlimit if hastdc==1 & binterest>0 & binterest!=., /*
*/  statistics( mean sd p25 p50 p75 ) /*
*/  columns(statistics)
esttab using sumstathas_ccinterest.tex,  /*
*/ cells("mean(fmt(%10.2fc)) sd(fmt(%10.2fc)) p25(fmt(%10.2fc)) p50(fmt(%10.2fc)) p75(fmt(%10.2fc))") collabels("\small{Mean}" "\small{Std dev}" "\small{P25}" "\small{P50}" "\small{P75}") /*
 */  replace label nonum noobs nodep nomtitles fragment

 
 
 *sum stats for accounts in top quartile of balance distribution
estpost tabstat bage bincome btenure badbchecking binterest bsaldotdc bcreditlimit if badbchecking>=15000, /*
*/  statistics( mean sd p25 p50 p75 ) /*
*/  columns(statistics)
esttab using sumstattopq.tex,  /*
*/ cells("mean(fmt(%10.2fc)) sd(fmt(%10.2fc)) p25(fmt(%10.2fc)) p50(fmt(%10.2fc)) p75(fmt(%10.2fc))") collabels("\small{Mean}" "\small{Std dev}" "\small{P25}" "\small{P50}" "\small{P75}") /*
 */  replace label nonum noobs nodep nomtitles fragment
 

 
  
 *sum stats for accounts in top quartile of balance distribution with CC
estpost tabstat bage bincome btenure badbchecking binterest bsaldotdc bcreditlimit if badbchecking>=15000 & hastdc==1, /*
*/  statistics( mean sd p25 p50 p75 ) /*
*/  columns(statistics)
esttab using sumstattopq_withCC.tex,  /*
*/ cells("mean(fmt(%10.2fc)) sd(fmt(%10.2fc)) p25(fmt(%10.2fc)) p50(fmt(%10.2fc)) p75(fmt(%10.2fc))") collabels("\small{Mean}" "\small{Std dev}" "\small{P25}" "\small{P50}" "\small{P75}") /*
 */  replace label nonum noobs nodep nomtitles fragment
 
 
 
 
  
 *sum stats for accounts in top quartile of balance distribution with CC interest
estpost tabstat bage bincome btenure badbchecking binterest bsaldotdc bcreditlimit if badbchecking>=15000& hastdc==1 & binterest>0 & binterest!=., /*
*/  statistics( mean sd p25 p50 p75 ) /*
*/  columns(statistics)
esttab using sumstattopq_withCCInterest.tex,  /*
*/ cells("mean(fmt(%10.2fc)) sd(fmt(%10.2fc)) p25(fmt(%10.2fc)) p50(fmt(%10.2fc)) p75(fmt(%10.2fc))") collabels("\small{Mean}" "\small{Std dev}" "\small{P25}" "\small{P50}" "\small{P75}") /*
 */  replace label nonum noobs nodep nomtitles fragment
 
 
 
*TABLE 2 
*covariate balance
*differences in means
sum bage
texresults using balance.txt, texmacro(testcont) result(r(mean)) unitzero replace
areg trat bage bincome btenure badbchecking hastdc binterest bsaldotdc bcreditlimit, abs(strata) robust
foreach var in bage bincome btenure badbchecking hastdc binterest bsaldotdc bcreditlimit{
areg `var' trat, abs(strata) robust
texresults using balance.txt, texmacro(`var'cont) result(_b[_cons]) unitzero append
local a = _b[_cons] +  _b[trat]
texresults using balance.txt, texmacro(`var'treat) result(`a') unitzero append
texresults using balance.txt, texmacro(`var'coef) result(_b[trat]) unitzero append
texresults using balance.txt, texmacro(`var'se) result(_se[trat]) unitzero append
}


*joint test
areg trat bage bincome btenure badbchecking hastdc binterest bsaldotdc bcreditlimit, abs(strata) robust
foreach var in bage bincome btenure badbchecking hastdc binterest bsaldotdc bcreditlimit{
texresults using balance.txt, texmacro(omni`var') result(_b[`var']) unitzero round(0.0001)append
texresults using balance.txt, texmacro(omnise`var') result(_se[`var']) unitzero round(0.0001) append
}

local f = Ftail(e(df_m), e(df_r), e(F))
texresults using balance.txt, texmacro(omniF) result(`f')  unitzero round(.0001) append

************************************************************
*******************TABLE 3 - SAVING AND BORROWING******************
************************************************************
 
gen savingsoverincome = sdoprom_vista_t1/ing_mensual
sum savingsoverincome, det
winsor2 savingsoverincome, c(1 99) replace
xtile decsavincome = savingsoverincom, nq(10)

 
 gen hasccdebt = (sdo_cierre_tdc_1>0 & sdo_cierre_tdc_1!=.)
 gen hasint= (int_t1>0 & int_t1!=.)
 

estpost sum savingsoverincome hasccdebt hasint
matrix b= e(mean)
mat lis b
forvalues x= 1(1)10{
quietly: estpost sum savingsoverincome hasccdebt hasint if decsav==`x'
matrix a`x'= e(mean)
mat lis a`x'
matrix b = b\a`x'
 }
 mat lis b
 mat c = b[1..10,1...]
 mat lis c
 

 
estpost sum sdoprom_vista_t1 sdo_cierre_tdc_1 int_t1
matrix bb= e(mean)
mat lis bb
forvalues x= 1(1)10{
quietly: estpost sum sdoprom_vista_t1 sdo_cierre_tdc_1 int_t1 if decsav==`x' & hasint==1
matrix aa`x'= e(mean)
mat lis aa`x'
matrix bb = bb\aa`x'
 }
 mat lis bb
 mat cc = bb[1..10,1...]
 mat lis cc
 
  
 mat e = c,cc
 mat lis e
 
 mat rownames e = 1 2 3 4 5 6 7 8 9 10
 estout matrix(e, fmt(%10.2fc)) using borrowsaving.tex,  replace nonum style(tex)  collabels(none) mlabels(none)

 
 
 
 
 xtile decsavincome_wtdc = savingsoverincom if hastdc==1, nq(10)

 tab decsavincome_wtdc
 

 
estpost sum savingsoverincome hasccdebt hasint if hastdc==1
matrix b= e(mean)
mat lis b
*add row with count
estpost sum savingsoverincome if hastdc==1
matrix count1= e(count)
mat lis count1
matrix b = count1,b
mat lis b


forvalues x= 1(1)10{
quietly: estpost sum savingsoverincome hasccdebt hasint if decsavincome_wtdc==`x' & hastdc==1
matrix a`x'= e(mean)
mat lis a`x'
*add row with count
estpost sum savingsoverincome if decsavincome_wtdc==`x' & hastdc==1
matrix count1= e(count)
mat lis count1
matrix a`x' = count1,a`x'
mat lis a`x'

matrix b = b\a`x'
 }
 mat lis b
 mat c = b[1..11,1...]
 mat lis c
 

 
estpost sum sdoprom_vista_t1 sdo_cierre_tdc_1 int_t1 if hastdc==1 & hasint==1
matrix bb= e(mean)
mat lis bb
*add row with count
quietly: estpost sum savingsoverincome if hasint==1 & hastdc==1
matrix count1= e(count)
mat lis count1
matrix bb = count1,bb
mat lis bb


forvalues x= 1(1)10{
quietly: estpost sum sdoprom_vista_t1 sdo_cierre_tdc_1 int_t1 if decsavincome_wtdc==`x' & hasint==1 & hastdc==1
matrix aa`x'= e(mean)
mat lis aa`x'
*add row with count
quietly: estpost sum savingsoverincome if decsavincome_wtdc==`x' & hasint==1 & hastdc==1
matrix count1= e(count)
mat lis count1
matrix aa`x' = count1,aa`x'
mat lis aa`x'

matrix bb = bb\aa`x'
 }
 mat lis bb
 mat cc = bb[1..11,1...]
 mat lis cc
 
  
 mat e = c,cc
 mat lis e
 
 mat rownames e = All 1 2 3 4 5 6 7 8 9 10
 estout matrix(e, fmt("%10.0g" "%10.2fc" "%10.2fc" "%10.2fc" "%10.0g" "%10.2fc" "%10.2fc" "%10.2fc" )) using borrowsaving_wtdc.tex,  replace nonum style(tex)  collabels(none) mlabels(none)


 
 
 cap drop puzzle
 gen puzzle = (savingsoverincome>=.5 & hasint==1) if hastdc==1
  tab puzzle 
 
 
 
 
 
************************************************************
******************TABLE 4 *********************
************************************************************
 
 
*The puzzle population vs not - all paying interest payments.
 reg bincome puzzle
texresults using puzzle.txt, texmacro(testcontpuzzle) result(r(mean)) unitzero replace
 foreach var in bage bincome btenure badbchecking binterest bsaldotdc bcreditlimit{
reg `var' puzzle
texresults using puzzle.txt, texmacro(`var'contpuzzle) result(_b[_cons]) unitzero append
local a = _b[_cons] +  _b[puzzle]
texresults using puzzle.txt, texmacro(`var'puzzle) result(`a') unitzero append
texresults using puzzle.txt, texmacro(`var'coefpuzzle) result(_b[puzzle]) unitzero append
texresults using puzzle.txt, texmacro(`var'sepuzzle) result(_se[puzzle]) unitzero append
}


 
 
 
 *The puzzle and the predicted treatment effects.
 
 
 xtile quartile = predict, nq(4)
 
 
 tab quartile if hastdc==1
 tab quartile if hastdc==1 & hasint==1
 tab quartile if hastdc==1 & hasint==1 & puzzle==1
 
 label var quartile "Quartiles of Predicted Treatment Effect"
 hist quartile if hastdc==1 & hasint==1 & puzzle==1,frac color(navy) disc gap(40)
 
 
 *FIGURE 5
 graph export "puzzle_bypredict.png", as(png) replace
 
 
  
 
 **** persistance of interest payments
 
 
 keep randomid  int*   bint*   has* puzzle
 keep if hastdc==1
forvalues i = 1(1)6{
	gen h_int`i' = int_t`i'>0 & int_t`i'!=.
}

bysort randomid: egen upuzzle = max(puzzle)
count if h_int1==1 
count if h_int1==1 & h_int2==1
count if h_int1==1 & h_int2==1 & h_int3==1

reshape long h_int, i(randomid) j(t)

tsset randomid t
sum h_int if l.h_int==1
sum h_int if l.h_int==1 & upuzzle==1
sum h_int if l.h_int==1 & upuzzle==0

 
 ***THIS IS THE LAST ROW OF TABLE 4
 
reg h_int upuzzle if l.h_int==1, vce(cluster randomid)
texresults using puzzle.txt, texmacro(hintcontpuzzle) result(_b[_cons]) unitzero append
local a = _b[_cons] +  _b[upuzzle]
texresults using puzzle.txt, texmacro(hintpuzzle) result(`a') unitzero append
texresults using puzzle.txt, texmacro(hintcoefpuzzle) result(_b[upuzzle]) unitzero append
texresults using puzzle.txt, texmacro(hintsepuzzle) result(_se[upuzzle]) unitzero append


save data_t4.dta, replace
  
 
************************************************************
*******************TABLE 5************
************************************************************
 
 
 
 ****************regresion table with avreage results
use dataforregression_mock.dta, clear
tab hastdc
winsor2 vista_log, replace
winsor2 vista, replace

sum vista if trat==0
estadd scalar m= r(mean)

label define trat 0 "Control" 1 "Any treatment" , replace
label values trat trat



*overall effect
areg vista_log i.trat, abs(strata)
sum vista if trat==0
estadd scalar m= r(mean)
estimates store model1

*by message
areg vista_log i.msj, abs(strata)
sum vista if trat==0
estadd scalar m= r(mean)
estimates store model2

*by periodicity
encode perio, gen(perio)
areg vista_log i.perio, abs(strata)
sum vista if trat==0
estadd scalar m= r(mean)
estimates store model3

label define perio 1 "Control" 2 "Bi-weekly" 3 "Weekly", replace
label values perio perio

label define msj 1 "Msg1" 2 "Msg2" 3 "Msg3" 4 "Msg4" 5 "Msg5" 6 "Msg6" 7 "Msg7", replace
label values msj msj



label var vista_log "\makecell{Log of \\ Checking Acct. \\ Balance +1}"

label var int_log "\makecell{Log of \\ Credit Card \\ Interest +1}"


*people with credit card
areg vista_log i.trat if hastdc==1, abs(strata)
sum vista if trat==0 & hastdc==1
estadd scalar m= r(mean)
estimates store model4


*people with credit card ccint
winsor2 int_log, replace
areg int_log i.trat if hastdc==1, abs(strata)
winsor2 intereses, replace
sum intereses if trat==0 & hastdc==1
estadd scalar m= r(mean)
estimates store model5




esttab model1 model2 model3 model4 model5 using itt.tex, replace  ///
	label booktabs b(3) se(3) eqlabels(none) alignment(S S) ///
	drop(0.trat 0.msj 1.perio _cons ) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(N m, fmt(0 2) layout("\multicolumn{1}{l}{@}" "\multicolumn{1}{l}{@}") labels(`"Observations"' `"\makecell[l]{Mean of Checking Acct. \\ Balance in Control Group}"'))





*** table 6
	
xtile ventil= predic, nq(20)
tab ventil

cap drop top
gen top = 1 if venti>=16
replace top=0 if venti<=5

*differences in means
sum bage

reg bincome top
texresults using top.txt, texmacro(testconttop) result(r(mean)) unitzero replace
foreach var in bage bincome btenure badbchecking hastdc binterest bsaldotdc bcreditlimit{
reg `var' top
texresults using top.txt, texmacro(`var'conttop) result(_b[_cons]) unitzero append
local a = _b[_cons] +  _b[top]
texresults using top.txt, texmacro(`var'top) result(`a') unitzero append
texresults using top.txt, texmacro(`var'coeftop) result(_b[top]) unitzero append
texresults using top.txt, texmacro(`var'setop) result(_se[top]) unitzero append
}



	
******TABLE 12********	
**THIS TABLE WAS SEMIMANUALLY PRODUCED. I AM REQUESTING THE INCLUSION OF A CODE TO AUTOMATE THE PROCESS.
**THE EXPERIMENTAL STRATA, WHICH DEFINE THE CONTENT OF EACH COLUMN ARE GIVEN BY THE FOLLOWIN VARIABLES
*COLUMN 1 CUARTILES_SDO_VISTA---  QUARTILES OF CHECKING ACCOUNT BALANCES
*COLUMN 2 CUARTILES INGRESO --- QUARILES INCOME
*COLUMN 3 CUARTILES EDAD --- QUARTILES AGE
*COLUMN 4 MEDIANA_ANTIGUEDAD ---- MEDIAN TENURE WITH BANORTE
*COLUMN 5 RAGO_TXN_ATM --- Median of ATM TRANSACTION 
*COLUMN 6 RANGO TXN TDD --- mEDIAN OF DEBIT CARD TRANSACTION

	

*by experimental strata - any treatment
 foreach var in cuartiles_sdo_vista cuartiles_edad cuartiles_ingreso mediana_antiguedad rango_txn_atm rango_txn_tdd {

encode `var', gen(n`var')
areg vista_log 1.trat#n`var', abs(strata)
}


 foreach var in cuartiles_sdo_vista cuartiles_edad cuartiles_ingreso mediana_antiguedad rango_txn_atm rango_txn_tdd {
areg vista_log 1.trat##n`var', abs(strata) robust
}

*COLUMN 7 CASH_LESS --- IS DIGITAL
*COLUMN 8 BANCOPRINCIPAL --- MAIN BANK
*COLUMN 9 HASTDC --- HAS CREDIT CARD


areg vista_log trat##bancoprincipal, abs(strata) robust
areg vista_log trat##cash_less, abs(strata) robust
areg vista_log  trat##hastdc, abs(strata) robust


******TABLE 13********
* THIS TABLE ALSO NEEDS TO BE AUTOMATED
sum intereses if trat==0 & ncuartiles_sdo==4 & hastdc==1
areg vista_log  trat if ncuartiles_sdo==4 & hastdc==1, abs(strata) robust
areg int_log  trat if ncuartiles_sdo==4 & hastdc==1, abs(strata) robust





*TABLE 14



display c(current_time)
cap drop beta
gen beta=.
levelsof strata, local(st)
foreach i in `st'{
    quietly: cap areg vista_log trat  if strata== `i', abs(strata)
	quietly: replace beta= _b[trat] if strata==`i'
}
display c(current_time)





sum vista_log, det
winsor2 vista_log, replace
sum vista_log, det



tab quart

xtile q2 = beta, nq(4)

tab quart q2

cap drop lntdc
gen lntdc = ln(sdotdc+1)
winsor2 lntdc, replace
winsor2 int_log, replace


winsor2 vista, replace
winsor2 interes, replace
winsor2 sdotdc, replace

*the following regressions are used to build Table 14 was built manually. By manually I mean that I don't have a code exporting the coefficient regressions into the latex file. I am requesting the automation of table 14.

*columns 1 to 4


*panel a

*vista_log goes for ln checking account balance
*int_log goes for ln credit card interest
*lntdc  goes for ln credit card balance

areg vista_log trat if q2==4, abs(strata)
areg int_log trat if q2==4, abs(strata)
areg lntdc trat if q2==4, abs(strata)

*vista goes for checking account balance
*interest goes for credit card interest
*sdotdc goes for credit card balance

sum vista if q2==4 & trat==0
sum interes if q2==4 & trat==0
sum sdotdc if q2==4 & trat==0

*panel b
areg vista_log trat if q2==4 & hastdc==1, abs(strata)
areg int_log trat if q2==4 & hastdc==1, abs(strata)
areg lntdc trat if q2==4 & hastdc==1, abs(strata)

sum vista if q2==4 & trat==0 & hastdc==1
sum interes if q2==4 & trat==0 & hastdc==1
sum sdotdc if q2==4 & trat==0 & hastdc==1



*panel c
areg vista_log trat if q2==4 & hastdc==1 &binterest>0, abs(strata)
areg int_log trat if q2==4 & hastdc==1&binterest>0, abs(strata)
areg lntdc trat if q2==4 & hastdc==1&binterest>0, abs(strata)

sum vista if q2==4 & trat==0 & hastdc==1 & binterest>0
sum interes if q2==4 & trat==0 & hastdc==1 & binterest>0
sum sdotdc if q2==4 & trat==0 & hastdc==1 & binterest>0




*columns 5 to 8

*panel a

areg vista_log trat if quart==4, abs(strata)
areg int_log trat if quart==4, abs(strata)
areg lntdc trat if quart==4, abs(strata)
sum vista if quart==4 & trat==0
sum interes if quart==4 & trat==0
sum sdotdc if quart==4 & trat==0

*panel b

areg vista_log trat if quart==4 & hastdc==1, abs(strata)
areg int_log trat if quart==4 & hastdc==1, abs(strata)
areg lntdc trat if quart==4 & hastdc==1, abs(strata)

sum vista if quart==4 & trat==0 & hastdc==1
sum interes if quart==4 & trat==0 & hastdc==1
sum sdotdc if quart==4 & trat==0 & hastdc==1



*panel c

areg vista_log trat if quart==4 & hastdc==1 &binterest>0, abs(strata)
areg int_log trat if quart==4 & hastdc==1&binterest>0, abs(strata)
areg lntdc trat if quart==4 & hastdc==1&binterest>0, abs(strata)

sum vista if quart==4 & trat==0 & hastdc==1 & binterest>0
sum interes if quart==4 & trat==0 & hastdc==1 & binterest>0
sum sdotdc if quart==4 & trat==0 & hastdc==1 & binterest>0



***

*This is the information for table 15
tab q2 quart


