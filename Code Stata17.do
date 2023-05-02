
******************************************************************************
*Impact of China Free Trade Zones on the Innovation Performance of Firms: 
/// Evidence from a Quasi-natural Experiment  
*****************************************************************************


****************************
* Empirical results * 
****************************

clear
cd C:\Users\ThinkPad\Desktop
use "Raw data.dta"
xtset cid year
xtdescribe 
summarize
global vars1 ROA Growth Tangible Leverage Cashflow Perwage Revenue FDI Telecom  
         


** Regression results using a time-varying DID model
global vars0  ROA Growth Tangible Leverage Cashflow   
global vars1  ROA Growth Tangible Leverage Cashflow Perwage Revenue FDI Telecom   
xtreg Patent FTZ i.year i.provid i.indid,fe vce(cluster cid)
est sto Patent0
xtreg Patent FTZ $vars0 i.year i.provid i.indid,fe vce(cluster cid)
est sto Patent1
xtreg Patent FTZ $vars1 i.year i.provid i.indid,fe vce(cluster cid)
est sto Patent2
xtreg Invent FTZ i.year i.provid i.indid,fe vce(cluster cid)
est sto Invent0
xtreg Invent FTZ $vars0 i.year i.provid i.indid,fe vce(cluster cid)
est sto Invent1
xtreg Invent FTZ $vars1 i.year i.provid i.indid,fe vce(cluster cid)
est sto Invent2
esttab  Patent0 Patent1 Patent2 Invent0 Invent1 Invent2, ///
                title("Baseline regression") /// 
                keep(FTZ $vars1 _cons) /// 
	            scalar(N r2_a) compress star(* 0.1 ** 0.05 *** 0.01) ///  
				b(%8.4f) ar2(%8.4f) t(%8.4f) nogap mtitles replace

esttab  Patent0 Patent1 Patent2 Invent0 Invent1 Invent2 using 基准回归.rtf, ///
                title("Baseline regression") /// 
                keep(FTZ $vars1 _cons) /// 
	            scalar(N r2_a) compress star(* 0.1 ** 0.05 *** 0.01) ///  
				b(%8.4f) ar2(%8.4f) t(%8.4f) nogap mtitles replace
			
** Robustness tests.

***Parallel trend test.
gen policy = year - fta_yr      
replace policy = -6 if policy >= 2000
replace policy = -6 if policy <= -6
replace policy = 3 if policy >= 3
gen policy_d = policy + 6

xtreg Patent ib6.policy_d i.year , fe r
test 0.policy_d 1.policy_d 2.policy_d 3.policy_d 4.policy_d  5.policy_d
test 7.policy_d 8.policy_d 9.policy_d 

coefplot,vertical baselevels drop(*.year _cons) msymbol(circle_hollow) ///
	yline(0,lp(dash) lwidth(vthin)) ylabel(-0.3(0.1)0.3) ytitle(a. Patent percentage changes) /// 
	xtitle(Years relative to FTZ establishment) xline(7, lp(dash) lwidth(vthin)) /// 
	addplot(line @b @at) ///
	ciopts(lpattern(dash) recast(rcap)) ///
	coeflabels(0.policy_d=-6 1.policy_d=-5 2.policy_d=-4 3.policy_d=-3 ///
	4.policy_d=-2 5.policy_d=-1 6.policy_d=0 7.policy_d=1 8.policy_d=2 9.policy_d=3) 
graph save scatter1,replace

*
xtreg Invent2 ib6.policy_d i.year , fe r
test 0.policy_d 1.policy_d 2.policy_d 3.policy_d 4.policy_d  5.policy_d
test 7.policy_d 8.policy_d 9.policy_d 
*
coefplot,vertical baselevels drop(*.year _cons) msymbol(circle_hollow) ///
	yline(0,lp(dash) lwidth(vthin)) ylabel(-0.6(0.2)0.5) ytitle(b. Invent percentage changes) /// 
	xtitle(Years relative to FTZ establishment) xline(7, lp(dash) lwidth(vthin)) /// 
	addplot(line @b @at) ///
	ciopts(lpattern(dash) recast(rcap)) ///
	coeflabels(0.policy_d=-6 1.policy_d=-5 2.policy_d=-4 3.policy_d=-3 ///
	4.policy_d=-2 5.policy_d=-1 6.policy_d=0 7.policy_d=1 8.policy_d=2 9.policy_d=3) 
graph save scatter2,replace

***Placebo test.1--Patent
clear
cd C:\Users\ThinkPad\Desktop
use "Raw data.dta"
xtset cid year
global vars1  ROA Growth Tangible Leverage Cashflow Perwage Revenue FDI Telecom

	tab year did
	xtset cid year
	gen branch_reform = year if did!=l.did & l.did!=.
	sort cid branch_reform 
	bys cid:replace branch_reform=branch_reform[1]
	save high_tec,replace
	append using high_tec
	save high_tec,replace
	append using high_tec
	save high_tec,replace
	append using high_tec
	save high_tec,replace 

	forvalue i=1/2000{    

	use high_tec , clear
	bys cid year : gen N=_n
	sort N cid year
	bys N : gen k = uniform()
	sort N cid year
	preserve
	duplicates drop N cid , force
	keep N cid k
	sort N k
	bys N : gen x = _n
	save tt,replace 
	restore
	drop k
	merge m:1 N cid using tt
	sort N year k
	gen after = 0
	sort N  year k
	replace after=1 if year==2010 &  x<0 
	replace after=1 if year==2011 &  x<0
	replace after=1 if year==2012 &  x<0
	replace after=1 if year==2013 &  x<0
	replace after=1 if year==2014 &  x<142
	replace after=1 if year==2015 &  x<153
	replace after=1 if year==2016 &  x<549
	replace after=1 if year==2017 &  x<653
	replace after=1 if year==2018 &  x<1167
    statsby _b[after]   _se[after] , clear by(N):xtreg Patent after $vars1 i.year,fe r                                             
	
	*append using tempp3       
	
	save tempp3,replace
	
	}  

	gen t= _stat_1/ _stat_2
	gen b=1 if t > -1.96 & t < 1.96
	*kdensity t, normal xline(-1.96 1.96)
	kdensity _stat_1 , normal 

	
***Placebo test.2--Invent

	forvalue i=1/2000{   

	use high_tec,clear
	bys cid year : gen N=_n
	sort N cid year
	bys N : gen k = uniform()
	sort N cid year
	preserve
	duplicates drop N cid,force
	keep N cid k
	sort N k
	bys N : gen x = _n
	save tt,replace 
	restore
	drop k
	merge m:1 N cid using tt
	sort N year k
	gen after = 0
	sort N  year k
	replace after=1 if year==2010 &  x<0 
	replace after=1 if year==2011 &  x<0
	replace after=1 if year==2012 &  x<0
	replace after=1 if year==2013 &  x<0
	replace after=1 if year==2014 &  x<142
	replace after=1 if year==2015 &  x<153
	replace after=1 if year==2016 &  x<549
	replace after=1 if year==2017 &  x<653
	replace after=1 if year==2018 &  x<1167
    statsby _b[after]   _se[after] , clear by(N):xtreg Invent after $vars1 i.year,fe r                                             
	
	append using tempp3    
	
	save tempp3 , replace
	
	}  

	gen t= _stat_1/ _stat_2
	gen b=1 if t > -1.96 & t < 1.96
	kdensity t, normal xline(-1.96 1.96)
	*kdensity _stat_1 , normal 					
	
		
							
***Excluding interference from other policies	***Substitution of dependent variables.
xtreg Patent FTZ $vars1 i.year i.provid i.indid if patent>1 & invent>6, fe vce(cluster cid)
est sto Patent
xtreg Invent FTZ $vars1 i.year i.provid i.indid if patent>1 & invent>6 , fe vce(cluster cid)
est sto Invent
			
xtset cid year  //psm
set seed 0715
gen ranorder = runiform()
sort ranorder
psmatch2 treat $vars1 , outcome(RD) logit neighbor(1) common caliper(0.0001) ties
pstest $vars1 , both graph
xtreg RD  FTZ $vars1 i.provid i.indid i.year if _support==1, fe 
est sto RD
xtset cid year
xtreg TPF FTZ $vars1 i.provid i.indid i.year, fe 
est sto TPF
			
esttab Patent Invent RD TPF , ///
                title("replacing dependent variables") /// 
                keep(FTZ _cons) /// 
	            scalar(N r2_a) compress star(* 0.1 ** 0.05 *** 0.01) ///  
				b(%8.4f) ar2(%8.4f) t(%8.4f) nogap mtitles replace




*************************************************
* Mediation path analysis
*************************************************

** Mediating effects of financing constraints.
xi:xtreg FC FTZ $vars1 i.year i.provid i.indid  , fe 
est store fac
xi:xtreg Patent FC FTZ $vars1 i.year i.provid i.indid  , fe 
est store xtreg1
xi: sgmediation Patent, mv(FC) iv(FTZ) cv( $vars1 i.year i.provid i.indid )
est store sobel1
xtreg Invent FC FTZ $vars1 i.year i.provid i.indid  , fe 
est store xtreg2
xi: sgmediation Invent, mv(FC) iv(FTZ) cv( $vars1 i.year i.provid i.indid )
est store sobel2

esttab   fac xtreg1 xtreg2 sobel1 sobel2  , ///
                title("Mediating effect of financing constraints")  /// 
                keep(FC FTZ  ) /// 
	            scalar(N r2_a) compress star(* 0.1 ** 0.05 *** 0.01) ///  
				b(%8.4f) ar2(%8.4f) t(%8.4f) nogap mtitles replace			
		
	
** Mediating effects of industrial agglomeration.
xi: xtreg HHI FTZ $vars1 i.year i.provid i.indid, fe
est sto HHI
xi: sgmediation Patent, mv(HHI) iv(FTZ) cv($vars1 i.year i.provid i.indid)	
est store sobel1
xi: sgmediation Invent, mv(HHI) iv(FTZ) cv($vars1 i.year i.provid i.indid)	
est store sobel2
												
esttab HHI sobel1 sobel2  , ///
                title("市场竞争hhi")  /// 
                keep( HHI FTZ _cons) /// 
	            scalar(N r2_a) compress star(* 0.1 ** 0.05 *** 0.01) ///  
				b(%8.4f) ar2(%8.4f) t(%8.4f) nogap mtitles replace
											
	
*************************************************
* Endogeneity and moderating effects
*************************************************

** Treatment of the endogeneity problem.		
***step1.检验工具变量是否有效，H0：工具变量Pipe、So2
clear
cd C:\Users\ThinkPad\Desktop
use "Raw data.dta"
global vars1  ROA Growth Tangible Leverage Cashflow Perwage Revenue FDI Telecom
tsset year cid
xtivreg Patent $vars1 (FTZ = So2 Pipe) , fe 
est sto iv1
xtivreg Invent $vars1 (FTZ = So2 Pipe) , fe 
est sto iv2

esttab iv1 iv2 , ///
	title("Using Pipe、So2、Savings for IVs") /// 
	keep( FTZ  _cons) /// 
	scalar(N r2_a) compress star(* 0.1 ** 0.05 *** 0.01) ///  
	b(%8.4f) ar2(%8.4f) t(%8.4f) nogap mtitles replace			
				
***step2.检验工具变量是否全部都有效，过度识别检验。H0:工具变量都是有效的，不存在过度识别的问题
xtoverid , r cluster(cid)
//检验结果：P-value =  0.0726，大于0.05说明不拒绝原假设，工具变量全部有效


	
**Moderating effects of contextual factors.
gen FTZ_Pergdp = FTZ * Pergdp		
xtreg Patent FTZ Pergdp FTZ_Pergdp $vars1 i.year i.provid i.indid, fe vce(cluster cid)  
est sto PergdpP
xtreg Invent FTZ Pergdp FTZ_Pergdp $vars1 i.year i.provid i.indid, fe vce(cluster cid)  
est sto PergdpI

gen FTZ_Subsidy = FTZ * Subsidy
xtreg Patent FTZ Subsidy FTZ_Subsidy $vars1 i.year i.provid i.indid, fe vce(cluster cid)  
est sto SubsidyP
xtreg Invent FTZ Subsidy FTZ_Subsidy $vars1 i.year i.provid i.indid, fe vce(cluster cid)  
est sto SubsidyI

esttab PergdpP PergdpI SubsidyP SubsidyI , ///
                title("Moderating effects of level of economic development and government subsidies") /// 
                keep(FTZ Pergdp FTZ_Pergdp Subsidy FTZ_Subsidy _cons) /// 
	            scalar(N r2_a) compress star(* 0.1 ** 0.05 *** 0.01) ///  
				b(%8.4f) ar2(%8.4f) t(%8.4f) nogap mtitles replace
			
		

	
	
exit
clear