# Stata--EM-Funcition-of-Gender
Paolo Spring 2018 Research

use "C:\Users\Pablo San Martin\Dropbox\SaoSan\Board Gender\DB Gener EUR.dta", clear

. sort iden year
. tsset iden year, yearly
. set more off



********Generación de Primera Medida de Earnings Management (wEM1)********

. gen netcashflow= cashfromoperatingact- cashfrominvestingact- cashfromfinancingact
. gen EM1=((totalrevenue-netcashflow)/ttlcmnsharesout)/l.priceclose
. winsor EM1, generate (wEM1) p(0.01)



********Generación de Segunda Medida de Earnings Management (wEM2)********

. generate chcurrasset= totalcurrentassets-l.totalcurrentassets
. generate chcash= cashandstinvestments-l.cashandstinvestments
. generate chcurrliab= totalcurrliabilities- l.totalcurrliabilities
. generate chcurrltdebt= currentportionltdebttocapitallea-l.currentportionltdebttocapitallea
. generate chtaxpayable= incometaxespayable-l.incometaxespayable
. generate chnotespay=d.notespayablestdebt
. generate accruals= chcurrasset- chcash- chcurrliab+ chcurrltdebt+ chtaxpayable- depreciationamort
. generate EM2=abs(accruals) / abs(cashfromoperatingact)
. winsor EM2, generate (wEM2) p(0.02)



********Generación de Medida de Earnings Quality (eq)********

. generate eq= eqcountrylistrank/100
. label variable eq "Earnings Quality List Rank"



********Generación de Medida de Women on the Board y Variables Independientes de Board********

. generate womenboard= analyticboardfemale/100
. label variable womenboard "% women on board"

. generate womenexecutive= analyticexecutivemembersgenderdi/100
. label variable womenexecutive "% women are executives"

. generate boardsize= ln(cgboardsize)
. label variable boardsize "Ln(boardsize)"

. generate indepboard= analyticindepboard/100
. label variable indepboard "% of independent board members"

. generate nonexecboard= analyticnonexecboard /100
. label variable nonexecboard "% of non-executive board members"

. generate boardmeetings1=ln( boardmeetings)
. label variable boardmeetings1 "Ln(boardmeetings)"

. generate boardmeetingattendanceavg1= boardmeetingattendanceavg/100
. label variable boardmeetingattendanceavg1 "% of attendance"

. generate analyticboardmembercomp1=ln( analyticboardmembercomp)
. label variable analyticboardmembercomp1 "Ln(board compensation in US$)"



********Generación de Variables Independientes de Control********

. gen size=ln(totalassetsreported)
. histogram size, normal
. winsor size, gen(wsize) p(0.005)
. label variable size "Ln(TA)"

. gen mtb=(priceclose*ttlcmnsharesout)/totalequity
. histogram mtb, normal
. winsor mtb , gen( wmtb ) p(0.01)
. label variable mtb "(Price * #shares) / Equity"

. generate tobinq= (totalliabilities+ ( ttlcmnsharesout * priceclose))/ totalassetsreported
. label variable tobinq "(Liabilities + MarketCap) / TA"
. histogram tobiq, normal
. winsor tobiq, gen(wtobinq) p(0.01)

. generate roa = netincomeaftertaxes/ totalassetsreported
. label variable roa "Net Income / Total Assets"
. histogram roa, normal
. winsor roa, generate(wroa) p(0.005)

. generate tdta= totalliabilities/ totalassetsreported
. histogram tdta, normal
. replace tdta=. if tdta>1

. generate marketcap =ttlcmnsharesout* priceclose
. label variable marketcap "Price * #shares"

. gen z=(((1.2*( totalcurrentassets - deferredincometax ))+(1.4* retainedearnings )+(3.3* normalizedebit )+ totalrevenue )/ totalassetsreported )+(0.6* marketcap / totalliabilities )
. label variable z "Altman Z-Score based on normailized EBIT"
. histogram z, normal
. winsor z, gen(wz) p(0.01)

. generate growratesales=d.totalrevenue/ totalrevenue
. label variable growratesales "Change Sales / Sales(t)"
. histogram growratesales, normal
. replace growratesales=. if growratesales>1
. replace growratesales=. if growratesales<-1

. generate capex=(d.pptyplanteqpmtttlgross)/ totalassetsreported
. label variable capex "Change PPE / TA"
. histogram capex, normal
. winsor capex, gen(wcapex) p(0.01)

. gen econindex= ln(econfree)
. label variable econindex "Ln(Economic Freedom Index)"

. gen ownconcen=own1+own2+own3
. label variable ownconcen "Ownership concentration"

. egen id=group( iden )
. egen yc=group(year country )
. egen c=group( country )
. egen y=group (year)



********Generación de Tercera Medida de Earnings Management (absEM3)********

. generate averta= (totalassetsreported-l.totalassetsreported)/2
. label variable averta "Average TA calculated as (TA(t) - TA(t-1)) / TA(t-1)"

. generate ttcaccruals= chcurrasset- chcurrliab- chcash+ chcurrltdebt
. label variable ttcaccruals "Total current accruals calcualted as (Ch.CA - Ch.CL - Ch.Cash + Ch. STDEBT)"

. generate tca= ttcaccruals/ averta
. label variable tca "TcurrentAccruals  / Avg.TA"

. generate cfol= l.cashfromoperatingact/ averta
. label variable cfol "CFO(t-1) / Avg.TA"

. generate cfo= cashfromoperatingact/ averta
. label variable cfo "CFO(t) / Avg.TA"

. generate cfof= f.cashfromoperatingact/ averta
. label variable cfof "CFO(t+1) / Avg.TA"

. regress tca cfol cfo cfof
. predict EM3, residual
. winsor EM3, gen(wEM3) p(0.01)
. generate absEM3 = abs( wEM3)
. label variable absEM3 "EM based on Dechow & Dichev 2002"



********Generación de Cuarta Medida de Earnings Management (absEM4)********

. generate ttaccruals= chcurrasset- chcash- chcurrliab+ chcurrltdebt- depreciationamort
. label variable ttaccruals "Total current accruals calcualted as (Ch.CA - Ch.CL - Ch.Cash + Ch. STDEBT)- Dep"

. generate tc= ttaccruals/ l.totalassetsreported
. label variable tc "TAccruals / TA(t-1)"

. generate chsalesta=d.totalrevenue/l.totalassetsreported
. label variable chsalesta "Change Sales  / TA(t-1)"

. generate ppeta= pptyplanteqpmtttlgross/l.totalassetsreported
. label variable ppeta "Gross PPE  / TA(t-1)"

. regress tc chsalesta ppeta
. predict EM4, residual
. winsor EM4, gen(wEM4) p(0.005)
. generate absEM4=abs(wEM4)
. label variable absEM4 "EM based on Dechow et al 1995"



********Generación de Quinta Medida de Earnings Management (absEM5)********

. generate chsaleschrta=(d.totalrevenue-d.totalreceivablesnet)/l.totalassetsreported
. label variable chsaleschrta "(Change Sales - Change Receivable) / TA(t-1)"
. regress tc chsaleschrta ppeta
. predict EM5, residual
. winsor EM5, gen(wEM5) p(0.005)
. generate absEM5=abs( wEM5)
. label variable absEM5 "EM according to Jones 1991"




********Women on Board - Linear regression with a large dummy-variable set********
********Dependent Variable wEM1********

. areg wEM1 womenboard own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", replace label keep(womenboard own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM1") tstat

. areg wEM1 womenboard boardsize own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard boardsize own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM1") tstat

. areg wEM1 womenboard indepboard own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM1") tstat

. areg wEM1 womenboard indepboard nonexecboard own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard nonexecboard own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM1") tstat

. areg wEM1 womenboard indepboard nonexecboard auditcomm own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM1") tstat

. areg wEM1 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM1") tstat


********Dependent Variable wEM2********

. areg wEM2 womenboard own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM2") tstat

. areg wEM2 womenboard boardsize own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard boardsize own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM2") tstat

. areg wEM2 womenboard indepboard own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM2") tstat

. areg wEM2 womenboard indepboard nonexecboard own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard nonexecboard own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM2") tstat

. areg wEM2 womenboard indepboard nonexecboard auditcomm own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM2") tstat

. areg wEM2 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM2") tstat


********Dependent Variable absEM3********

. areg absEM3 womenboard own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM3") tstat

. areg absEM3 womenboard boardsize own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard boardsize own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM3") tstat

. areg absEM3 womenboard indepboard own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM3") tstat

. areg absEM3 womenboard indepboard nonexecboard own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard nonexecboard own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM3") tstat

. areg absEM3 womenboard indepboard nonexecboard auditcomm own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM3") tstat

. areg absEM3 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM3") tstat



********Dependent Variable absEM4********

. areg absEM4 womenboard own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM4") tstat

. areg absEM4 womenboard boardsize own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard boardsize own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM4") tstat

. areg absEM4 womenboard indepboard own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM4") tstat

. areg absEM4 womenboard indepboard nonexecboard own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard nonexecboard own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM4") tstat

. areg absEM4 womenboard indepboard nonexecboard auditcomm own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM4") tstat

. areg absEM4 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM4") tstat


********Dependent Variable absEM5********

. areg absEM5 womenboard own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM5") tstat

. areg absEM5 womenboard boardsize own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard boardsize own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM5") tstat

. areg absEM5 womenboard indepboard own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM5") tstat

. areg absEM5 womenboard indepboard nonexecboard own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard nonexecboard own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM5") tstat

. areg absEM5 womenboard indepboard nonexecboard auditcomm own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM5") tstat

. areg absEM5 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize tobinq tdta z roa, absorb(yc) r cluster (id)
. outreg2 using "AREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize tobinq tdta z roa) addtext(Year FE, YES, Country FE, YES) addstat(Adj. R-Squared, e(r2_a), F-test, e(F)) dec(4) ctitle("EM5") tstat






********Women on Board - FE********
********Dependent Variable wEM1********

. xtreg wEM1 womenboard own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", replace label keep(womenboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM1") tstat

. xtreg wEM1 womenboard boardsize own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM1") tstat

. xtreg wEM1 womenboard indepboard own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM1") tstat

. xtreg wEM1 womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM1") tstat

. xtreg wEM1 womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM1") tstat

. xtreg wEM1 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM1") tstat


********Dependent Variable wEM2********

. xtreg wEM2 womenboard own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM2") tstat

. xtreg wEM2 womenboard boardsize own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM2") tstat

. xtreg wEM2 womenboard indepboard own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM2") tstat

. xtreg wEM2 womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM2") tstat

. xtreg wEM2 womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM2") tstat

. xtreg wEM2 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM2") tstat




********Dependent Variable absEM3********

. xtreg absEM3 womenboard own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM3") tstat

. xtreg absEM3 womenboard boardsize own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM3") tstat

. xtreg absEM3 womenboard indepboard own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM3") tstat

. xtreg absEM3 womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM3") tstat

. xtreg absEM3 womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM3") tstat

. xtreg absEM3 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM3") tstat



********Dependent Variable absEM4********

. xtreg absEM4 womenboard own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM4") tstat

. xtreg absEM4 womenboard boardsize own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM4") tstat

. xtreg absEM4 womenboard indepboard own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM4") tstat

. xtreg absEM4 womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM4") tstat

. xtreg absEM4 womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM4") tstat

. xtreg absEM4 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM4") tstat



********Dependent Variable absEM5********

. xtreg absEM5 womenboard own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM5") tstat

. xtreg absEM5 womenboard boardsize own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM5") tstat

. xtreg absEM5 womenboard indepboard own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM5") tstat

. xtreg absEM5 womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM5") tstat

. xtreg absEM5 womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM5") tstat

. xtreg absEM5 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa, fe
. outreg2 using "XTREG.xls", append label keep(womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM5") tstat




********Women on Board - Cross-sectional time-series FGLS********
********Dependent Variable wEM1********

. xtgls wEM1 womenboard own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", replace label keep(womenboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM1") tstat

. xtgls wEM1 womenboard boardsize own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM1") tstat

. xtgls wEM1 womenboard indepboard own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM1") tstat

. xtgls wEM1 womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM1") tstat

. xtgls wEM1 womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM1") tstat

. xtgls wEM1 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM1") tstat


********Dependent Variable wEM2********

. xtgls wEM2 womenboard own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtgls wEM2 womenboard boardsize own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtgls wEM2 womenboard indepboard own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtgls wEM2 womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtgls wEM2 womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtgls wEM2 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat




********Dependent Variable absEM3********

. xtgls absEM3 womenboard own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM3") tstat

. xtgls absEM3 womenboard boardsize own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM3") tstat

. xtgls absEM3 womenboard indepboard own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM3") tstat

. xtgls absEM3 womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM3") tstat

. xtgls absEM3 womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM3") tstat

. xtgls absEM3 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM3") tstat



********Dependent Variable absEM4********

. xtgls absEM4 womenboard own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtgls absEM4 womenboard boardsize own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtgls absEM4 womenboard indepboard own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtgls absEM4 womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtgls absEM4 womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtgls absEM4 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat



********Dependent Variable absEM5********

. xtgls absEM5 womenboard own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtgls absEM5 womenboard boardsize own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtgls absEM5 womenboard indepboard own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtgls absEM5 womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtgls absEM5 womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtgls absEM5 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat






********Women on Board - Dynamic Panel Data (Arellano & Bond)********
********Dependent Variable wEM1********

. xtabond wEM1 womenboard own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", replace label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM1") tstat

. xtabond wEM1 womenboard boardsize own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM1") tstat

. xtabond wEM1 womenboard indepboard own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM1") tstat

. xtabond wEM1 womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM1") tstat

. xtabond wEM1 womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM1") tstat

. xtabond wEM1 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM1") tstat


********Dependent Variable wEM2********

. xtabond wEM2 womenboard own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM2") tstat

. xtabond wEM2 womenboard boardsize own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM2") tstat

. xtabond wEM2 womenboard indepboard own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM2") tstat

. xtabond wEM2 womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM2") tstat

. xtabond wEM2 womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM2") tstat

. xtabond wEM2 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM2") tstat




********Dependent Variable absEM3********

. xtabond absEM3 womenboard own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM3") tstat

. xtabond absEM3 womenboard boardsize own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM3") tstat

. xtabond absEM3 womenboard indepboard own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM3") tstat

. xtabond absEM3 womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM3") tstat

. xtabond absEM3 womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM3") tstat

. xtabond absEM3 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM3") tstat



********Dependent Variable absEM4********

. xtabond absEM4 womenboard own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM4") tstat

. xtabond absEM4 womenboard boardsize own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM4") tstat

. xtabond absEM4 womenboard indepboard own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM4") tstat

. xtabond absEM4 womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM4") tstat

. xtabond absEM4 womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM4") tstat

. xtabond absEM4 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM4") tstat



********Dependent Variable absEM5********

. xtabond absEM5 womenboard own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM5") tstat

. xtabond absEM5 womenboard boardsize own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM5") tstat

. xtabond absEM5 womenboard indepboard own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM5") tstat

. xtabond absEM5 womenboard indepboard nonexecboard own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM5") tstat

. xtabond absEM5 womenboard indepboard nonexecboard auditcomm own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM5") tstat

. xtabond absEM5 womenboard indepboard nonexecboard auditcomm boardmeetings1 own1 wsize wtobinq tdta wz roa, lags(1) twostep noconstant endogenous(own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2), Instruments, e(zrank)) dec(4) ctitle("EM5") tstat







********Women on Board - GMM regression LLEGUE HASTA AQUI CHAVALOTE, QUE ME BAJO LA PEREZA, ASI QUE LO COMPLETAS TU, OK?********
********Dependent Variable wEM1********

xtabond2 wEM2 womenboard boardsize indepboard ownconcen wsize wtobinq tdta wz roa econindex i.y ,r twostep gmm(womenboard boardsize indepboard ownconcen wsize, lag(2 6))
outreg2 using "1.2 Basic GMM.doc", replace label keep(womenboard boardsize indepboard ownconcen wsize wtobinq tdta wz roa econindex) addtext(Year FE, YES) addstat(F-Test, e(F), Auto(1), e(ar1p) , Auto(2), e(ar2p) , Hansen p-value, e(hansenp)) title("Table 2. Women on Board and Earnings Management: Generalized Method of Moments") ctitle("EM2") dec(3) nocons 

xtabond2 wEM5 womenboard boardsize indepboard ownconcen wsize wtobinq tdta wz roa econindex i.y ,r twostep gmm(womenboard boardsize indepboard ownconcen wsize, lag(2 6))
outreg2 using "1.2 Basic GMM.doc", append label keep(womenboard boardsize indepboard ownconcen wsize wtobinq tdta wz roa econindex) addtext(Year FE, YES) addstat(F-Test, e(F), Auto(1), e(ar1p) , Auto(2), e(ar2p) , Hansen p-value, e(hansenp)) title("Table 2. Women on Board and Earnings Management: Generalized Method of Moments") ctitle("EM1") dec(3)  nocons 

xtabond2 wEM5 womenboard boardsize indepboard ownconcen wsize wtobinq tdta wz roa econindex i.y ,r twostep gmm(womenboard boardsize indepboard ownconcen wsize, lag(2 6))
outreg2 using "1.2 Basic GMM.doc", append label keep(womenboard boardsize indepboard ownconcen wsize wtobinq tdta wz roa econindex) addtext(Year FE, YES) addstat(F-Test, e(F), Auto(1), e(ar1p) , Auto(2), e(ar2p) , Hansen p-value, e(hansenp)) title("Table 2. Women on Board and Earnings Management: Generalized Method of Moments") ctitle("EM1") dec(3)  nocons 


**CREO QUE**
** 1) mejor métodos Xtreg y GMM**
** 2) mejor EM2, EM4 y EM5**
** 3) Baseline => EM = gender; boardsize; OWN1; Size; TobinQ; Debt; Z; ROA**
** 4) Los modelos con GGG o ECONFREE no cambian mucho el control i.c, con lo que da más o menos igual**
** 5) Creo que habría que incorporar algunas dummies (Common Law vs otras; year law passed, etc.) que den algo de juego a los resultados**  



******************************************
********* DESCRIPTIVE STATISTICS *********
******************************************

***Dependent Variables wEM1 wEM2 absEM3 absEM4 absEM5
tabstat wEM1 wEM2 absEM3 absEM4 absEM5, statistics ( mean median sd min max skewness kurtosis )
tabstat wEM1 wEM2 absEM3 absEM4 absEM5, statistics ( mean) by ( country )
***Indpendent Variables womenboard womenexecutive boardsize indepboard nonexecboard policyboarddiversity ceoboardmember corporategovernancecomm nominationcomm auditcomm compcomm boardmeetings boardmeetings1 analyticboardattendance boardmeetingattendanceavg1 analyticceochairmanseparation wsize wmtb wroa tdta wz wcapex tobinq ownconcen own1 own2 own3 econindex va ps ge rq rl cc econfree ggg
tabstat womenboard womenexecutive boardsize indepboard nonexecboard policyboarddiversity ceoboardmember corporategovernancecomm nominationcomm auditcomm compcomm boardmeetings boardmeetings1 analyticboardattendance boardmeetingattendanceavg1 analyticceochairmanseparation wsize wmtb wroa tdta wz wcapex tobinq ownconcen own1 own2 own3 econindex va ps ge rq rl cc econfree ggg, statistics ( mean median sd min max skewness kurtosis )
tabstat womenboard womenexecutive boardsize indepboard nonexecboard policyboarddiversity ceoboardmember corporategovernancecomm nominationcomm auditcomm compcomm boardmeetings boardmeetings1 analyticboardattendance boardmeetingattendanceavg1 analyticceochairmanseparation wsize wmtb wroa tdta wz wcapex tobinq ownconcen own1 own2 own3 econindex va ps ge rq rl cc econfree ggg, statistics ( mean ) by  (country)
***Correlation matrix womenboard womenexecutive boardsize indepboard boardmeetings1 boardmeetingattendanceavg1 wsize wmtb wroa tdta wz wcapex tobinq ownconcen own1 own2 own3 
correlate womenboard womenexecutive boardsize indepboard boardmeetings1 boardmeetingattendanceavg1 wsize wmtb wroa tdta wz wcapex tobinq ownconcen own1 own2 own3
***Correlation matrix econindex va ps ge rq rl cc econfree ggg
correlate econindex va ps ge rq rl cc econfree ggg










































*************************************
********* FINALES SEGÚN PSM *********
*************************************


********* XTREG *********


. xtreg wEM2 womenboard boardsize own1 wsize tobinq tdta wz roa i.c, fe
. outreg2 using "XTREG.xls", replace label keep(womenboard boardsize own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM2") tstat

. xtreg wEM2 womenboard  boardsize indepboard own1 wsize tobinq tdta wz roa i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize indepboard own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM2") tstat

. xtreg wEM2 womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex ) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM2") tstat

. xtreg wEM2 womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM2") tstat

. xtreg wEM2 womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM2") tstat

. xtreg wEM2 womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM2") tstat


** Más lo que me costó definir un modelo para EM2 y no me sale ahora EM4 y EM5***
. xtreg absEM4 womenboard boardsize own1 wsize tobinq tdta wz roa i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM4") tstat

. xtreg absEM4 womenboard  boardsize indepboard own1 wsize tobinq tdta wz roa i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize indepboard own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM4") tstat

. xtreg absEM4 womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex ) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM4") tstat

. xtreg absEM4 womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM4") tstat

. xtreg absEM4 womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM4") tstat

. xtreg absEM4 womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM4") tstat




. xtreg absEM5 womenboard boardsize own1 wsize tobinq tdta wz roa i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM5") tstat

. xtreg absEM5 womenboard  boardsize indepboard own1 wsize tobinq tdta wz roa i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize indepboard own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM5") tstat

. xtreg absEM5 womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex ) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM5") tstat

. xtreg absEM5 womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM5") tstat

. xtreg absEM5 womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM5") tstat

. xtreg absEM5 womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex i.c, fe
. outreg2 using "XTREG.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Sigma u, e(sigma_u), Sigma e, e(sigma_e), Rho, e(rho), F-test, e(F_f), R-sqr, e(r2_o)) dec(4) ctitle("EM5") tstat



********* XTGLS *********

. xtgls wEM2 womenboard boardsize own1 wsize tobinq tdta wz roa i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", replace label keep(womenboard boardsize own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtgls wEM2 womenboard  boardsize indepboard own1 wsize tobinq tdta wz roa i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize indepboard own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtgls wEM2 womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex ) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtgls wEM2 womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtgls wEM2 womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtgls wEM2 womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat



. xtgls absEM4 womenboard boardsize own1 wsize tobinq tdta wz roa i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtgls absEM4 womenboard  boardsize indepboard own1 wsize tobinq tdta wz roa i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize indepboard own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtgls absEM4 womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtgls absEM4 womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtgls absEM4 womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtgls absEM4 womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat




. xtgls absEM5 womenboard boardsize own1 wsize tobinq tdta wz roa i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtgls absEM5 womenboard  boardsize indepboard own1 wsize tobinq tdta wz roa i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize indepboard own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtgls absEM5 womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex ) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtgls absEM5 womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtgls absEM5 womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtgls absEM5 womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex i.c, p(h) c(ar1) force
. outreg2 using "XTGLS.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat





********* XTPCSE *********

. xtpcse wEM2 womenboard boardsize own1 wsize tobinq tdta wz roa i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", replace label keep(womenboard boardsize own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtpcse wEM2 womenboard  boardsize indepboard own1 wsize tobinq tdta wz roa i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize indepboard own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtpcse wEM2 womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex ) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtpcse wEM2 womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtpcse wEM2 womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtpcse wEM2 womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat



. xtpcse absEM4 womenboard boardsize own1 wsize tobinq tdta wz roa i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtpcse absEM4 womenboard  boardsize indepboard own1 wsize tobinq tdta wz roa i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize indepboard own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtpcse absEM4 womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtpcse absEM4 womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtpcse absEM4 womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtpcse absEM4 womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat




. xtpcse absEM5 womenboard boardsize own1 wsize tobinq tdta wz roa i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtpcse absEM5 womenboard  boardsize indepboard own1 wsize tobinq tdta wz roa i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize indepboard own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtpcse absEM5 womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex ) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtpcse absEM5 womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtpcse absEM5 womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtpcse absEM5 womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex i.c, het c(psar1)
. outreg2 using "XTPCSE.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(R-squared, e(r2), Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat






********* XTABOND *********

. xtabond wEM2 womenboard boardsize own1 wsize tobinq tdta wz roa _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(womenboard own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", replace label keep(womenboard boardsize own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtabond wEM2 womenboard  boardsize indepboard own1 wsize tobinq tdta wz roa _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(womenboard indepboard own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize indepboard own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtabond wEM2 womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(boardsize indepboard nonexecboard own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex ) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtabond wEM2 womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(womenboard boardsize indepboard nonexecboard own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtabond wEM2 womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(womenboard boardsize indepboard nonexecboard boardmeetings own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat

. xtabond wEM2 womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(womenboard boardsize indepboard nonexecboard boardmeetings own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM2") tstat



. xtabond absEM4 womenboard boardsize own1 wsize tobinq tdta wz roa _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(womenboard own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtabond absEM4 womenboard  boardsize indepboard own1 wsize tobinq tdta wz roa _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(womenboard indepboard own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize indepboard own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtabond absEM4 womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(boardsize indepboard nonexecboard own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtabond absEM4 womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(womenboard boardsize indepboard nonexecboard own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtabond absEM4 womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(womenboard boardsize indepboard nonexecboard boardmeetings own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat

. xtabond absEM4 womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(womenboard boardsize indepboard nonexecboard boardmeetings own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM4") tstat




. xtabond absEM5 womenboard boardsize own1 wsize tobinq tdta wz roa _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(womenboard own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtabond absEM5 womenboard  boardsize indepboard own1 wsize tobinq tdta wz roa _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(womenboard indepboard own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize indepboard own1 wsize tobinq tdta wz roa) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtabond absEM5 womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(boardsize indepboard nonexecboard own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize tobinq tdta wz roa econindex ) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtabond absEM5 womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(womenboard boardsize indepboard nonexecboard own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize indepboard nonexecboard own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtabond absEM5 womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(womenboard boardsize indepboard nonexecboard boardmeetings own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat

. xtabond absEM5 womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex _Ic_2 - _Ic_10, r lags(1)  noconstant endogenous(womenboard boardsize indepboard nonexecboard boardmeetings own1 roa, lag(0,2))
. outreg2 using "XTABOND.xls", append label keep(womenboard boardsize indepboard nonexecboard boardmeetings ge own1 wsize wmtb tdta wz roa econindex) addtext(Year FE, YES, Country FE, YES) addstat(Wald-test, e(chi2)) dec(4) ctitle("EM5") tstat








