clear
set more off
log close _all
cap log using EM_Board_Remuneration.log, text replace 

/******************************
		Earnings Management and Board Remuneration and Board Activity
******************************/

global USER "Ryan McWay"
local date `c(current_date)'
local time `c(current_time)'
local input "raw"
local output "text"
local edit "edit"
**************************************************************************
display "Analysis run by $USER for EM and Board Remuneration and Board Activity at `date' and `time'"

use "`input'\BD.dta" 

//************** Basic Regressions - Regresiones Básicas
xtreg qtob contesf size i.yc, fe r
outreg2 using "`output'\Basic.doc",  replace keep(contesf size) addtext(Country-Year FE, YES) addstat( R-squared, e(r2_w)) title("Table 4. Contestability to control and firm performance: OLS-Fixed Effect estimates") ctitle("Tobin's Q") dec(3) nocons

xtreg qtob contesf size dtta i.yc, fe r
outreg2 using "Basic.doc",  append keep(contesf size dtta) addtext(Country-Year FE, YES) addstat( R-squared, e(r2_w)) title("Table 4. Contestability to control and firm performance: OLS-Fixed Effect estimates") ctitle("Tobin's Q") dec(3) nocons

//************** GMM Panel Data for ABS(DA2) - Datos de Panel GMM para ABS(DA2)
xtabond2 absdam2 own1 size wleverage1 wroa wz , small twostep gmm(own1, lag(1 9))
est store ecu1
outreg2 using "`output'\GMM.doc", replace keep(own1 wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

xtabond2 absdam2 logown1 size wleverage1 wroa wz , small twostep gmm(logown1, lag(1 9))
est store ecu2
outreg2 using "`output'\GMM.doc", append keep(logown1 wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

xtabond2 absdam2 insown insown2 size wleverage1 wroa wz , small twostep gmm(insown, lag(1 9))
est store ecu3
outreg2 using "`output'\GMM.doc", append keep(insown insown2 wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

xtabond2 absdam2 loginsown size wleverage1 wroa wz , small twostep gmm(loginsown, lag(1 9))
est store ecu4
outreg2 using "`output'\GMM.doc", append keep(loginsown wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

xtabond2 absdam2 own1 bactiv size wleverage1 wroa wz , small twostep gmm(own1 bactiv, lag(1 9))
est store ecu5
outreg2 using "`output'\GMM.doc", append keep(own1 bactiv size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

xtabond2 absdam2 own1 bactiv bsize size wleverage1 wroa wz , small twostep gmm(own1 bactiv bsize, lag(1 9))
est store ecu6
outreg2 using "`output'\GMM.doc", append keep(own1 bactiv bsize size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

xtabond2 absdam2 own1 bactiv bsize bown size wleverage1 wroa wz , small twostep gmm(own1 bactiv bsize bown, lag(1 9))
est store ecu7
outreg2 using "`output'\GMM.doc", append keep(own1 bactiv bsize bown size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

xtabond2 absdam2 own1 bactiv bsize bown bgender size wleverage1 wroa wz , small twostep gmm(own1 bactiv bsize bown bgender, lag(1 9))
est store ecu8
outreg2 using "`output'\GMM.doc", append keep(own1 bactiv bsize bown bgender size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

xtabond2 absdam2 own1 bactiv bsize bown bgender baudit size wleverage1 wroa wz , small twostep gmm(own1 bactiv bsize bown bgender baudit, lag(1 9))
est store ecu9
outreg2 using "`output'\GMM.doc", append keep(own1 bactiv bsize bown bgender baudit size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

xtabond2 absdam2 own1 bactiv bsize bown bgender baudit bdual size wleverage1 wroa wz , small twostep gmm(own1 bactiv bsize bown bgender baudit bdual, lag(1 9))
est store ecu10
outreg2 using "`output'\GMM.doc", append keep(own1 bactiv bsize bown bgender baudit bdual size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

xtabond2 absdam2 own1 bactiv bsize bown bgender baudit bdual bindepend size wleverage1 wroa wz , small twostep gmm(own1 bactiv bsize bown bgender baudit bdual bindepend, lag(1 9))
est store ecu11
outreg2 using "`output'\GMM.doc", append keep(own1 bactiv bsize bown bgender baudit bdual bindepend size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

xtabond2 absdam2 own1 bactiv bsize bown bgender baudit bdual1 bindepend size wleverage1 wroa wz , small twostep gmm(own1 bactiv bsize bown bgender baudit bdual1 bindepend, lag(1 9))
est store ecu12
outreg2 using "`output'\GMM.doc", append keep(own1 bactiv bsize bown bgender baudit bdual1 bindepend size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

xtabond2 absdam2 own1 bactiv bsize bown bgender baudit bdual bindepend  size wleverage1 wroa wz , small twostep gmm(own1 bactiv bsize bown bgender baudit bdual bindepend , lag(1 9))
est store ecu13
outreg2 using "`output'\GMM.doc", append keep(own1 bactiv bsize bown bgender baudit bdual bindepend  size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

xtabond2 absdam2 own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz , small twostep gmm(own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu14
outreg2 using "`output'\GMM.doc", append keep(own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA1)") dec(4)  nocons tstat

//************ Results Output
outreg2 [ecu1 ecu2 ecu3 ecu4 ecu5 ecu6 ecu7 ecu8 ecu9 ecu10 ecu11 ecu12 ecu13 ecu14] using "`output'\resultados1.xls", excel replace addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA1)") dec(4)  nocons tstat

//*************** GMM Panel Data for ABS(DA1) - Datos de Panel GMM para ABS(DA1)
xtabond2 absdam1 own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz , small twostep gmm(own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu15
outreg2 using "`output'\GMM.doc", append keep(own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA1)") dec(4)  nocons tstat

//*************** GMM Panel Data for ABS(DA3) - Datos de Panel GMM para ABS(DA3)
xtabond2 absdam3 own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz , small twostep gmm(own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu16
outreg2 using "`output'\GMM.doc", append keep(own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA3)") dec(4)  nocons tstat

//*************** GMM Panel Data for ABS(DA4) - Datos de Panel GMM para ABS(DA4)
xtabond2 absdam4 own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz , small twostep gmm(own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu17
outreg2 using "`output'\GMM.doc", append keep(own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA4)") dec(4)  nocons tstat

//**************** GMM Panel Data for ABS(DA5) - Datos de Panel GMM para ABS(DA5)
xtabond2 absdam5 own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz , small twostep gmm(own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu18
outreg2 using "`output'\GMM.doc", append keep(own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA5)") dec(4)  nocons tstat

//************** Results Output
outreg2 [ecu15 ecu16 ecu17 ecu18] using "`output'\resultados1.xls", excel replace addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA1)") dec(4)  nocons tstat

//************** GMM Panel Data for ABS(DA1)/insown - Datos de Panel GMM para ABS(DA1) e insown
xtabond2 absdam1 insown bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz , small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu19
outreg2 using "`output'\GMM.doc", replace keep(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA1)") dec(4)  nocons tstat

//************* GMM Panel Data for ABS(DA2)/insown - Datos de Panel GMM para ABS(DA2) e insown
xtabond2 absdam2 insown bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz , small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu20
outreg2 using "`output'\GMM.doc", append keep(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

//************** GMM Panel Data for ABS(DA3)/insown - Datos de Panel GMM para ABS(DA3) e insown
xtabond2 absdam3 insown bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz , small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu21
outreg2 using "`output'\GMM.doc", append keep(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA3)") dec(4)  nocons tstat

//************* GMM Panel Data for ABS(DA4)/insown - Datos de Panel GMM para ABS(DA4) e insown
xtabond2 absdam4 insown bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz , small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu22
outreg2 using "`output'\GMM.doc", append keep(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA4)") dec(4)  nocons tstat

//************** GMM Panel Data for ABS(DA5)/insown - Datos de Panel GMM para ABS(DA5) e insown
xtabond2 absdam5 insown bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz , small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu23
outreg2 using "`output'\GMM.doc", append keep(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA5)") dec(4)  nocons tstat

//************ Results Output
outreg2 [ecu19 ecu20 ecu21 ecu22 ecu23] using "`output'\resultados1.xls", excel replace addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA1)") dec(4)  nocons tstat

//**************** GMM Panel Data for ABS(DA1)/insown/insown2 - Datos de Panel GMM para ABS(DA1) e insown e insown2
xtabond2 absdam1 insown insown2 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz , small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu24
outreg2 using "`output'\GMM.doc", replace keep(insown insown2 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA1)") dec(4)  nocons tstat

//**************** GMM Panel Data for ABS(DA2)/insown - Datos de Panel GMM para ABS(DA2) e insown
xtabond2 absdam2 insown insown2 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz , small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu25
outreg2 using "`output'\GMM.doc", append keep(insown insown2 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

//**************** GMM Panel Data for ABS(DA3)/insown - Datos de Panel GMM para ABS(DA3) e insown
xtabond2 absdam3 insown insown2 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz , small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu26
outreg2 using "`output'\GMM.doc", append keep(insown insown2 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA3)") dec(4)  nocons tstat

//**************** GMM Panel Data for ABS(DA4)/insown - Datos de Panel GMM para ABS(DA4) e insown
xtabond2 absdam4 insown insown2 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz , small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu27
outreg2 using "`output'\GMM.doc", append keep(insown insown2 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA4)") dec(4)  nocons tstat

//*************** GMM Panel Data for ABS(DA5)/insown - Datos de Panel GMM para ABS(DA5) e insown
xtabond2 absdam5 insown insown2 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz , small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu28
outreg2 using "`output'\GMM.doc", append keep(insown insown2  bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA5)") dec(4)  nocons tstat

//************ Results Output
outreg2 [ecu24 ecu25 ecu26 ecu27 ecu28] using "`output'\resultados1.xls", excel replace addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA1)") dec(4)  nocons tstat

//************ GMM Panel Data for ABS(DA1)/insown/bremun3 - Datos de Panel GMM para ABS(DA1) e insown y bremun3
xtabond2 absdam1 insown bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz , small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun3, lag(1 9))
est store ecu29
outreg2 using "`output'\GMM.doc", replace keep(insown bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA1)") dec(4)  nocons tstat

//************** GMM Panel Data for ABS(DA2)/insown - Datos de Panel GMM para ABS(DA2) e insown
xtabond2 absdam2 insown bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz , small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun3, lag(1 9))
est store ecu30
outreg2 using "`output'\GMM.doc", append keep(insown bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

//************* GMM Panel Data for ABS(DA3)/insown - Datos de Panel GMM para ABS(DA3) e insown
xtabond2 absdam3 insown bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz , small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun3, lag(1 9))
est store ecu31
outreg2 using "`output'\GMM.doc", append keep(insown bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA3)") dec(4)  nocons tstat

//************ GMM Panel Data for ABS(DA4)/insown - Datos de Panel GMM para ABS(DA4) e insown
xtabond2 absdam4 insown bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz , small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun3, lag(1 9))
est store ecu32
outreg2 using "`output'\GMM.doc", append keep(insown bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA4)") dec(4)  nocons tstat

//************* GMM Panel Data for ABS(DA5)/insown - Datos de Panel GMM para ABS(DA5) e insown
xtabond2 absdam5 insown bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz , small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun3, lag(1 9))
est store ecu33
outreg2 using "`output'\GMM.doc", append keep(insown bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA5)") dec(4)  nocons tstat

//************** Results Output
outreg2 [ecu29 ecu30 ecu31 ecu32 ecu33] using "`output'\resultados1.xls", excel replace addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA1)") dec(4)  nocons tstat

//************** GMM Panel Data for ABS(DA1)/logown1/bremun3 - Datos de Panel GMM para ABS(DA1) y logown1 y bremun3
xtabond2 absdam1 logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz , small twostep gmm(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3, lag(1 9))
est store ecu34
outreg2 using "`output'\GMM.doc", replace keep(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA1)") dec(4)  nocons tstat

//************** GMM Panel Data for ABS(DA2)/logown1 - Datos de Panel GMM para ABS(DA2) e logown1
xtabond2 absdam2 logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz , small twostep gmm(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3, lag(1 9))
est store ecu35
outreg2 using "`output'\GMM.doc", append keep(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

//*************** GMM Panel Data for ABS(DA3)/logown1 - Datos de Panel GMM para ABS(DA3) e logown1
xtabond2 absdam3 logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz , small twostep gmm(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3, lag(1 9))
est store ecu36
outreg2 using "`output'\GMM.doc", append keep(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA3)") dec(4)  nocons tstat

//*************** GMM Panel Data for ABS(DA4)/logown1 - Datos de Panel GMM para ABS(DA4) e logown1
xtabond2 absdam4 logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz , small twostep gmm(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3, lag(1 9))
est store ecu37
outreg2 using "`output'\GMM.doc", append keep(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA4)") dec(4)  nocons tstat

//*************** GMM Panel Data for ABS(DA5)/logown1 - Datos de Panel GMM para ABS(DA5) e logown1
xtabond2 absdam5 logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz , small twostep gmm(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3, lag(1 9))
est store ecu38
outreg2 using "`output'\GMM.doc", append keep(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA5)") dec(4)  nocons tstat

//************** Results Output
outreg2 [ecu34 ecu35 ecu36 ecu37 ecu38] using "`output'\resultados1.xls", excel replace addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA1)") dec(4)  nocons tstat

//************** GMM Panel Data for ABS(DA2)/logown1 - Datos de Panel GMM para ABS(DA2) e logown1
xtabond2 absdam1 logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz  , small twostep gmm(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3, lag(1 9))
est store ecu39
outreg2 using "`output'\GMM.doc", replace keep(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz  ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA1)") dec(4)  nocons tstat

//************* GMM Panel Data for ABS(DA2)/logown1 - Datos de Panel GMM para ABS(DA2) e logown1
xtabond2 absdam2 logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz  , small twostep gmm(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3, lag(1 9))
est store ecu40
outreg2 using "`output'\GMM.doc", append keep(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz  ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

//************* GMM Panel Data for ABS(DA3)/logown1 - Datos de Panel GMM para ABS(DA3) e logown1
xtabond2 absdam3 logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz  , small twostep gmm(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3, lag(1 9))
est store ecu41
outreg2 using "`output'\GMM.doc", append keep(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz  ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA3)") dec(4)  nocons tstat

//*************** GMM Panel Data for ABS(DA4)/logown1 - Datos de Panel GMM para ABS(DA4) e logown1
xtabond2 absdam4 logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz  , small twostep gmm(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3, lag(1 9))
est store ecu42
outreg2 using "`output'\GMM.doc", append keep(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz  ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA4)") dec(4)  nocons tstat

//*************** GMM Panel Data for ABS(DA5)/logown1 - Datos de Panel GMM para ABS(DA5) e logown1
xtabond2 absdam5 logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz  , small twostep gmm(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3, lag(1 9))
est store ecu43
outreg2 using "`output'\GMM.doc", append keep(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz  ) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA5)") dec(4)  nocons tstat

//*************** Results Output
outreg2 [ecu*] using "`output'\resultados1.xls", excel replace addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA1)") dec(4)  nocons tstat

//**************** GMM Panel Data and Corporate Goverance Indexes - Datos de Panel GMM y Corporate Governance Indexes
xtabond2 absdam2 own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz govindex, small twostep gmm(own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu44
outreg2 using "`output'\GMM.doc", replace keep(own1 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz govindex) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

xtabond2 absdam2 insown bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz govindex, small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu45
outreg2 using "`output'\GMM.doc", append keep(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz govindex) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

xtabond2 absdam2 insown insown2 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz govindex, small twostep gmm(insown bactiv bsize bown bgender baudit bdual bindepend  bremun2, lag(1 9))
est store ecu46
outreg2 using "`output'\GMM.doc", append keep(insown insown2 bactiv bsize bown bgender baudit bdual bindepend  bremun2 size wleverage1 wroa wz govindex) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

xtabond2 absdam2 logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz govindex, small twostep gmm(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3, lag(1 9))
est store ecu47
outreg2 using "`output'\GMM.doc", append keep(logown1 bactiv bsize bown bgender baudit bdual bindepend  bremun3 size wleverage1 wroa wz govindex) addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA2)") dec(4)  nocons tstat

//************ Result Output
outreg2 [ecu44 ecu45 ecu46 ecu47] using "`output'\resultados1.xls", excel replace addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) title("Table 1. Discretionary Accruals: Model 1") ctitle("ABS(DA1)") dec(4)  nocons tstat

//************ Descriptives
tabstat absdam1 absdam2 absdam3 absdam4 absdam5 own1 own12 logown1 insown insown2 loginsown bactiv bsize bown bgender baudit bdual bdual1 bindepend  bremun2 bremun3 size wleverage1 wroa wz govindex if bactiv!=. & absdam2!=., statistics(mean median sd min max skewness kurtosis count) noseparator columns(variables)
tabstat absdam1 absdam2 absdam3 absdam4 absdam5 if bactiv!=., statistics(mean) by(year) noseparator columns(variables)
pwcorr absdam1 absdam2 absdam3 absdam4 absdam5 own1 own12 logown1 insown insown2 loginsown bactiv bsize bown bgender baudit bdual bdual1 bindepend  bremun2 bremun3 size wleverage1 wroa wz govindex if bactiv!=. & absdam2!=., sig
correlate absdam1 absdam2 absdam3 absdam4 absdam5 own1 own12 logown1 insown insown2 loginsown bactiv bsize bown bgender baudit bdual bdual1 bindepend bremun2 bremun3 size wleverage1 wroa wz govindex if bactiv!=. & absdam2!=.

/***************************
		Results for Spanish Work
****************************/

RESULTADOS TRABAJO EM ESPAÑA
============================
//************* ABS(DA1)
. xtabond2 absdam1 bactiv bremun bremunsqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun bremunsqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", replace addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam1 bactiv bremun1 bremun1sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun1 bremun1sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam1 bactiv bremun2 bremun2sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun2 bremun2sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam1 bactiv bremun3 bremun3sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun3 bremun3sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam1 bactiv wbremun3 wbremun3sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv wbremun3 wbremun3sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam1 bactiv bremun7 bremun7sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun7 bremun7sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam1 bactiv bremun9 bremun9sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun9 bremun9sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

//************** ABS(DA2)
. xtabond2 absdam2 bactiv bremun bremunsqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun bremunsqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam2 bactiv bremun1 bremun1sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun1 bremun1sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam2 bactiv bremun2 bremun2sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun2 bremun2sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam2 bactiv bremun3 bremun3sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun3 bremun3sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam2 bactiv wbremun3 wbremun3sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv wbremun3 wbremun3sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam2 bactiv bremun7 bremun7sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun7 bremun7sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam2 bactiv bremun9 bremun9sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun9 bremun9sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

//************ ABS(DA3)
. xtabond2 absdam3 bactiv bremun bremunsqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun bremunsqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam3 bactiv bremun1 bremun1sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun1 bremun1sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam3 bactiv bremun2 bremun2sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun2 bremun2sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam3 bactiv bremun3 bremun3sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun3 bremun3sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam3 bactiv wbremun3 wbremun3sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv wbremun3 wbremun3sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam3 bactiv bremun7 bremun7sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun7 bremun7sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam3 bactiv bremun9 bremun9sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun9 bremun9sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

//************ ABS(DA4)
. xtabond2 absdam4 bactiv bremun bremunsqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun bremunsqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam4 bactiv bremun1 bremun1sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun1 bremun1sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam4 bactiv bremun2 bremun2sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun2 bremun2sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam4 bactiv bremun3 bremun3sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun3 bremun3sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam4 bactiv wbremun3 wbremun3sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv wbremun3 wbremun3sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam4 bactiv bremun7 bremun7sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun7 bremun7sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam4 bactiv bremun9 bremun9sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun9 bremun9sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

//************ ABS(DA5)
. xtabond2 absdam5 bactiv bremun bremunsqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun bremunsqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam5 bactiv bremun1 bremun1sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun1 bremun1sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam5 bactiv bremun2 bremun2sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun2 bremun2sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam5 bactiv bremun3 bremun3sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun3 bremun3sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam5 bactiv wbremun3 wbremun3sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv wbremun3 wbremun3sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam5 bactiv bremun7 bremun7sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun7 bremun7sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

. xtabond2 absdam5 bactiv bremun9 bremun9sqr insown bsize size wleverage wroa wz, small twostep gmm(insown bactiv bremun9 bremun9sqr bsize, lag(1 4))
. outreg2 using "`output'\GMM.doc", append addtext(Year FE, YES) addstat(No Instruments, e(j), F-test, e(F), p-value, e(F_p), AR(1), e(ar1), p-value, e(ar1p), AR(2), e(ar2), p-value, e(ar2p), Sargan-test, e(sargan), p-value, e(sarganp), Hansen-test, e(hansen), p-value, e(hansenp)) dec(4) tstat

//*********** Summary/Correlate
. summarize absdam3 absdam4 absdam5 bactiv bremun bremun9 insown bsize size wleverage wroa wz if bremun9!=., separator(0)
. correlate absdam3 absdam4 absdam5 bactiv bremun bremun9 insown bsize size wleverage wroa wz if bremun9!=.



save "`edit'\BD.dta" , replace
cd "`output'"
cap log close
translate EM_Board_Remuneration.log EM_Board_Remuneration.pdf, replace

//******* Finish.
