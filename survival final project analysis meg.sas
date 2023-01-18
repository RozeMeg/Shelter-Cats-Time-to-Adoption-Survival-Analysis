/*Survival Analysis
Final Project
1 May 2020*/

*read in the data;
proc import datafile = "C:\Users\meg\Desktop\Courses\Spring 2020 Courses\Survival Analysis\animalsfinal.csv"
out = animals
dbms = csv
replace;
getnames = yes;
run;

/******************Model fitting*******************/

*just add in the all the variables because there aren't many of them;
*cats analysis;
*get subset with cats;
data cats;
set animals;
if dog = 0;
run;

*see Table 1 stats for estimates of survival mean and median using R;
*I attempted to use the method we learned in class with the formedian dataset
*But ran into processing issues because my computer is a Microsoft Surface Pro

*Model 1 - all factors and interactions - this is what I ran the diagnostics on;
*cox p-h cats;
ods graphics on;
ods output ParameterEstimates = overall;
ods output HazardRatios = hrtable;
proc phreg data = cats plots(overlay) = survival;
*set reference group to 0;
class stray(ref = '0') primarycolor(ref = 'OTHER') gender(ref = 'FEMALE');
model tte*adopted(0) = stray primarycolor gender stray*primarycolor stray*gender primarycolor*gender/ties = breslow rl;
baseline covariates = cats out=overall2 survival = _all_/diradj group = stray;
*tried doing the hazard ratios this way but it was too busy - calculating manually for easier plotting;
*hazardratio stray;
*hazardratio primarycolor;
*hazardratio gender;

*variables for diagnostics;
*apparently I have to make residual variables for every combination of the factor interactions;
OUTPUT OUT=outp 
survival=survival XBETA=xb   LOGSURV=snell
RESMART=mart
RESDEV=dev 
DFBETA=dfbetstray dfbetblack dfbetbrown dfbetgray dfbettan dfbetwhite dfbetmale dfbetunknown dfbetstrayblack 
dfbetstraybrown dfbetstraygray dfbetstraytan dfbetstraywhite dfbetstraymale dfbetstrayunk dfbetblackmale 
dfbetblackunk dfbetbrownmale dfbetbrownunk dfbetgraymale dfbetgrayunk dfbettanmale dfbettanunk dfbetwhitemale   
dfbetwhiteunk
WTRESSCH =resschstray  resschblack resschbrown resschgray resschtan resschwhite resschmale resschunknown 
resschstrayblack resschstraybrown resschstraygray resschstraytan resschstraywhite resschstraymale
resschstrayunk resschblackmale resschblackunk resschbrownmale resschbrownunk resschgraymale  resschgrayunk
resschtanmale resschtanunk resschwhitemale resschwhiteunk
LMAX=lmax
RESSCO=ressco;
run;
ods graphics off;

*adjusted survival curves for color;
proc phreg data = cats plots(overlay) = survival;
*set reference group to 0;
class stray(ref = '0') primarycolor(ref = 'OTHER') gender(ref = 'FEMALE');
model tte*adopted(0) = stray primarycolor gender stray*primarycolor stray*gender primarycolor*gender/ties = breslow rl;
baseline covariates = cats out=overall2 survival = _all_/diradj group = primarycolor;
run;

*adjusted survival curves for gender;
proc phreg data = cats plots(overlay) = survival;
*set reference group to 0;
class stray(ref = '0') primarycolor(ref = 'OTHER') gender(ref = 'FEMALE');
model tte*adopted(0) = stray primarycolor gender stray*primarycolor stray*gender primarycolor*gender/ties = breslow rl;
baseline covariates = cats out=overall2 survival = _all_/diradj group = gender;
run;

*add the hazard ratios to the parameter estimate table for the forest plot;
data forestdata;
set overall;
HazardRatio = exp(Estimate);
HRLowerCL = exp(Estimate - 1.96*StdErr);
HRUpperCL = exp(Estimate + 1.96*StdErr);
run;

*get rid of the anomalous brown*unknown interaction that blew up;
*doesn't seem to have converged;
data forest;
set forestdata;
if hruppercl >1000 then delete;
run;

*sort by descending p-value supposedly - all significant p-values end up on top though;
proc sort data = forest;
by descending probchisq;
run;

*forest plot of results;
*modified code from this paper: https://support.sas.com/resources/papers/proceedings10/195-2010.pdf;
title "Hazard Ratios and 95% CIs of Potential Factors Influencing Time to Adoption";
proc sgplot data=forest;
 scatter x=HazardRatio y= Label/ xerrorlower=hrlowercl
xerrorupper=hruppercl
markerattrs=HazardRatio
(symbol=DiamondFilled size=8);
 refline 1 / axis=x;

 xaxis label="Hazard Ratio and 95% CI " min=0;
 yaxis label="Factors";

run;
title;

*write forest data to a file that can be edited for the manuscript;
proc export 
  data=forestdata 
  dbms=xlsx 
  outfile="C:\Users\meg\Desktop\Courses\Spring 2020 Courses\Survival Analysis\hazardratiosfinal.xlsx"  
  replace;
run;

*only select the significant ones;
data forestdata;
set forestdata;
if probchisq > 0.05 then delete;
run;

*write forest data to a file that can be edited for the manuscript;
proc export 
  data=forestdata 
  dbms=xlsx 
  outfile="C:\Users\meg\Desktop\Courses\Spring 2020 Courses\Survival Analysis\table2sigfinal.xlsx"  
  replace;
run;

/*Attempted additional Cox PH models based on model diagnostics*/

*take out top outliers when applicable and see how much the estimates change;
*if they don't by >10%, then we can say it's not a matter of concern;
*demonstrate this with primary color = tan b/c this is a significant effect that has 8 influential points;
*not able to do this for all 25 terms due to time constraints - so I will just do this for primary color
*tan;

*delete based on IDs (see output);
data tandata;
set cats;
if animalid in (476144 274894 597720 377423 469204 609989 461135 569398 610524 512043) then delete;
run;

*rerun proc phreg;
*compare estimates in output to estimates from first regression and see if any have changed by >10% ;

proc phreg data = tandata plots(overlay) = survival;
*set reference group to 0;
class stray(ref = '0') primarycolor(ref = 'OTHER') gender(ref = 'FEMALE');
model tte*adopted(0) = stray primarycolor gender stray*primarycolor stray*gender primarycolor*gender/ties = breslow rl;
baseline covariates = cats out=overall2 survival = _all_/diradj group = primarycolor;
run;
*interpret results with caution - >10% change in the estimates for tan;
*but not going to use this as the model as I'd probably have to keep taking outliers out;

/*schoenfield residuals and adjusted curves for colors looked a little suspect
for gray and white - add time interactions to see if that changes things*/
ods output ParameterEstimates = adjtimeparams;
proc phreg data = cats plots(overlay) = survival;
*set reference group to 0;
class stray(ref = '0') primarycolor(ref = 'OTHER') gender(ref = 'FEMALE');
model tte*adopted(0) = stray primarycolor gender primarycolor*tte gender*tte stray*primarycolor stray*gender primarycolor*gender/ties = breslow rl;
baseline covariates = cats out=overall2 survival = _all_/diradj group = primarycolor;
run;

*adjusted survival curves for stray;
proc phreg data = cats plots(overlay) = survival;
*set reference group to 0;
class stray(ref = '0') primarycolor(ref = 'OTHER') gender(ref = 'FEMALE');
model tte*adopted(0) = stray primarycolor gender primarycolor*tte stray*primarycolor stray*gender primarycolor*gender/ties = breslow rl;
baseline covariates = cats out=overall2 survival = _all_/diradj group = stray;
run;

*adjusted survival curves for gender;
proc phreg data = cats plots(overlay) = survival;
*set reference group to 0;
class stray(ref = '0') primarycolor(ref = 'OTHER') gender(ref = 'FEMALE');
model tte*adopted(0) = stray primarycolor gender primarycolor*tte gender*tte stray*primarycolor stray*gender primarycolor*gender/ties = breslow rl;
baseline covariates = cats out=overall2 survival = _all_/diradj group = gender;
run;

   /*************************Cox-Snell Residuals********************/
   /*******************testing for overall fit of the model*********/
data outp;
set outp;
genres=-snell;
run;
proc lifetest data=outp outsurv=survres noprint;
time genres*adopted(0);
run;
data survres;
set survres;
lls=log(-log(survival));
loggenr=log(genres);
run;
proc sgplot data=survres;
reg y=lls x=loggenr;
run;

/**************************Deviance Residuals*****************************/
/************************testing for outliers*****************************/

proc sgplot data=outp;
scatter y=dev x=animalid;
 refline -2 2 /axis=y;
run;

/**************************DFBETA Residuals (score residuals)*****************************/
/************************testing for influential points*****************************/
/************************ 2/sqrt(57404)=0.008347548**********************************

DFBETAs are the difference between the estimated regression coefficient 
using all observations and that without the i-th individual. 
This can be useful for assessing the influence of an individual.
******************************************************************************/

proc sgplot data=outp;
scatter y=dfbetstray x=animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetblack x=animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetbrown x=animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetgray x=animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbettan x=animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y =dfbetwhite x=animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;


proc sgplot data=outp;
scatter y=dfbetmale x=animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetunknown x=animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetstrayblack x=animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetstraybrown x=animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetstraygray x=animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetstraytan x=animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetstraywhite x=animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetstraymale x=animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetstrayunk x=animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetblackmale x= animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetblackunk x= animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetbrownmale x= animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetbrownunk x= animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetgraymale x= animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetgrayunk x= animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbettanmale x= animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbettanunk x= animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetwhitemale x= animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;

proc sgplot data=outp;
scatter y=dfbetwhiteunk x= animalid;
 refline -0.008347548 0.008347548 /axis=y;
run;


proc univariate data=outp;
var dfbetstray dfbetblack dfbetbrown dfbetgray dfbettan dfbetwhite dfbetmale dfbetunknown dfbetstrayblack 
dfbetstraybrown dfbetstraygray dfbetstraytan dfbetstraywhite dfbetstraymale dfbetstrayunk dfbetblackmale 
dfbetblackunk dfbetbrownmale dfbetbrownunk dfbetgraymale dfbetgrayunk dfbettanmale dfbettanunk dfbetwhitemale   
dfbetwhiteunk;
id animalid;
run;

/********************* Weighted Schoenfeld Residuals to assess time trends and lack of proportionality.**********/

proc sgplot data=outp;
scatter y=resschstray x=tte;
run;
   
proc sgplot data=outp;
scatter y=resschblack x=tte;
run;

proc sgplot data=outp;
scatter y=resschbrown x=tte;
run;

proc sgplot data=outp;
scatter y=resschgray x=tte;
run;

proc sgplot data=outp;
scatter y=resschtan x=tte;
run;

proc sgplot data=outp;
scatter y=resschwhite x=tte;
run;
   
proc sgplot data=outp;
scatter y=resschmale x=tte;
run;

proc sgplot data=outp;
scatter y=resschunk x=tte;
run;
   
proc sgplot data=outp;
scatter y=resschstrayblack x=tte;
run;

proc sgplot data=outp;
scatter y=resschstraybrown x=tte;
run;

proc sgplot data=outp;
scatter y=resschgray x=tte;
run;

proc sgplot data=outp;
scatter y=resschtan x=tte;
run;

proc sgplot data=outp;
scatter y=resschwhite x=tte;
run;
   
proc sgplot data=outp;
scatter y=resschstraymale x=tte;
run;

proc sgplot data=outp;
scatter y=resschstrayunk x=tte;
run;

proc sgplot data=outp;
scatter y =resschblackmale x=tte;
run;

proc sgplot data=outp;
scatter y =resschblackunk x=tte;
run;

proc sgplot data=outp;
scatter y =resschbrownmale x=tte;
run;

proc sgplot data=outp;
scatter y =resschbrownunk x=tte;
run;

proc sgplot data=outp;
scatter y =resschgraymale x=tte;
run;

proc sgplot data=outp;
scatter y =resschgrayunk x=tte;
run;

proc sgplot data=outp;
scatter y =resschtanmale x=tte;
run;

proc sgplot data=outp;
scatter y =resschtanunk x=tte;
run;

proc sgplot data=outp;
scatter y =resschwhitemale x=tte;
run;

proc sgplot data=outp;
scatter y =resschwhiteunk x=tte;
run;
   


