data practpro.combined1;
length A $20 Ethnicity $30 PCP $20 PCP at Time of Procedure $25;
set practpro.dced practpro.dcelec practpro.aaelec;
run;

data practpro.combined2;
set practpro.combined1;
drop at Time of Procedure;
run;

*Proc Contents-->Proc Univariate
*Recode for Logistic Regression: ethnicity, PCP, Race, BMI, ASA Class, Comorbidities, public or private, discharge dispo
*Dependent-----Independent
PCP at time----Comorbidities
ASA Class-----BMI, Comorbidities
LOS----Comorbidities
Comorbidities----Zipcode;

proc contents data=practpro.combined2;
run;
proc univariate data=practpro.combined2;
run;


data practpro.combined2a;
length 'Admission Type'n $10;
set practpro.combined2;
if index(A, 'DCED') > 0 then 'Admission Type'n='ED';
else 'Admission Type'n= 'Elective';
if race= "Pacific Islander" then race="Other";
if race= "White/Pacific Islander" then race= "White";
if race= "Unknown" then race= .;
if race= "Black or African American/Other" then race= "Black or African American";
if 'BMI Group Name'n= '-' then 'BMI Group Name'n= .;
if 'BMI Group Name'n= 'Uknown' then 'BMI Group Name'n= .;
if Diabetes='yes' then Diabetes='Yes';
if Insurance= 'yes' then Insurance='Yes';
if "PCP at Time of Procedure"n= "Unknown" then 'PCP at Time of Procedure'n= .;
if "PCP at Time of Procedure"n= "Uknown" then 'PCP at Time of Procedure'n= .;
if "PCP at Time of Procedure"n= "Unknow" then 'PCP at Time of Procedure'n= .;
if 'Marital Status'n='M' then 'Marital Status'n= 'Yes';
if 'Marital Status'n='Unknown' then 'Marital Status'n= .;
run;
proc freq data= practpro.combined2a;
tables 'Admission Type'n *('Surgery Type'n Race Sex 'Marital Status'n 'County/Residence'n 'BMI Group Name'n 'ASA Class'n Diabetes Hypertension Smoking Hyperlipidemia Arthritis Insurance 'PCP at Time of Procedure'n 'Discharge Disposition'n);
run;
proc means data=practpro.combined2a;
class 'Admission Type'n;
var 'LOS Hours'n age;
run;
proc logistic data= practpro.combined2a;
class 'Surgery Type'n Race Sex 'Marital Status'n 'County/Residence'n 'BMI Group Name'n 'ASA Class'n Diabetes Hypertension Smoking Hyperlipidemia Arthritis Insurance 'PCP at Time of Procedure'n 'Discharge Disposition'n;
model 'Admission Type'n(event='ED')= 'Surgery Type'n 'LOS Hours'n Age Race Sex 'Marital Status'n 'County/Residence'n 'BMI Group Name'n 'ASA Class'n Diabetes Hypertension Smoking Hyperlipidemia Arthritis Insurance 'PCP at Time of Procedure'n 'Discharge Disposition'n;
run; 
proc freq data=practpro.combined2a;
    tables 'Admission Type'n * 'PCP at Time of Procedure'n / chisq;
run;

*LOS Hours (Length of Stay): The odds ratio is 1.006, with a CI (1.004, 1.008). 
Since the CI does not include 1 and the point estimate is slightly above 1, it indicates a 
statistically significant but small increase in the odds of ED visits for each additional hour of stay.

County/Residence (AA County vs. Washington DC): The odds ratio is 0.025 with a CI (0.004, 0.142),
indicating a significantly lower likelihood of ED visits for residents of AA County compared to Washington DC.*

P-Values: LOS Hours <0.0001 Arthritis<0.0005, 
Hyperlipidemia: Estimate of 2.138 with CI (1.165, 3.925), 
showing a significant increase in odds for this condition.

Arthritis: Estimate of 2.870 with CI (1.587, 5.189), indicating a significant increase in odds.
PCP No vs. Yes: Point estimate of 4.962 with a CI (2.259, 10.896), 
showing a significant increase in odds for those without a PCP at the time of the process.**;


PROC LOGISTIC DATA= practpro.combined2a;
CLASS 'BMI Group Name'n(ref='Normal Weight') 'PCP at Time of Procedure'n(ref='No') Hyperlipidemia(ref='No') Arthritis(ref='No') / PARAM=REF;
MODEL 'Admission Type'n(event='ED') = 'BMI Group Name'n 'PCP at Time of Procedure'n Hyperlipidemia Arthritis / SELECTION=STEPWISE INCLUDE=1;
ODDSRATIO 'BMI Group Name'n;
ODDSRATIO 'PCP at Time of Procedure'n;
ODDSRATIO Hyperlipidemia;
ODDSRATIO Arthritis;
RUN;

Proc logistic data=practpro.combined2a;
class 'County/Residence'n(ref='PG County') 'PCP at Time of Procedure'n(ref='Yes') /PARAM=reference;
model Hyperlipidemia(event='Yes')= 'County/Residence'n 'PCP at Time of Procedure'n;
oddsratio 'County/Residence'n;
oddsratio 'PCP at Time of Procedure'n;
run;

proc freq data=practpro.combined2a;
    tables 'ASA Class'n * 'Admission Type'n / chisq nocol nopercent norow;
run;

/* Compare ED vs Elective within ASA Class 4 using Logistic Regression */
proc logistic data=practpro.combined2a;
    class 'Admission Type'n(ref='Elective') 'ASA Class'n / param=ref;
    model 'Admission Type'n(event='ED') = 'ASA Class'n / link=logit;
    where 'ASA Class'n = "4 - Incapacitating Disease"; /* Filter to ASA Class 4 */
run; *Pr > Chisq= 0.1761>0.05 no signifigance;

proc logistic data=practpro.combined2a;
    class 'Admission Type'n(ref='Elective') 'ASA Class'n / param=ref;
    model 'Admission Type'n(event='ED') = 'ASA Class'n / link=logit;
    where 'ASA Class'n = "3 - Severe Systemic Disease"; /* Filter to ASA Class 4 */
run; *Pr > Chisq= 0.0001<0.05 signifigance;

proc logistic data=practpro.combined2a;
    class 'Admission Type'n(ref='Elective') 'ASA Class'n / param=ref;
    model 'Admission Type'n(event='ED') = 'ASA Class'n / link=logit;
    where 'ASA Class'n = "2 - Mild Systemic Disease"; /* Filter to ASA Class 4 */
run; *Pr > Chisq= 0.1761>0.05 no signifigance;

/* Subset for ASA Class 4 and compare ED vs Elective */
proc freq data=practpro.combined2a;
    tables 'Admission Type'n / chisq fisher;
    where 'ASA Class'n = "4 - Incapacitating Disease"; /* This filters for ASA Class 4 only */
run; *Similar to previous*;

proc freq data=practpro.combined2a;
    tables 'Admission Type'n / chisq fisher;
    where 'ASA Class'n = "3 - Severe Systemic Disease"; /* This filters for ASA Class 4 only */
run; *Similar to previous*;

proc freq data=practpro.combined2a;
    tables 'ASA Class'n * 'Discharge Disposition'n / chisq nocol nopercent norow;
run; *Significant*;

proc freq data=practpro.combined2a;
    tables 'Admission Type'n * 'Discharge Disposition'n / chisq fisher;
    where 'ASA Class'n = "4 - Incapacitating Disease"; /* This filters for ASA Class 4 only */
run;

*Admission status as a function of ASA score*;
proc logistic data= practpro.combined2a;
class 'ASA Class'n (ref="1 - Healthy") / param=reference;
model "Admission Type"n (event="ED")= "ASA Class"n;
run;
*resulted in quasi-separation, probably due to many ASA classes being only ED;
proc freq data= practpro.combined2a;
    tables 'ASA Class'n*"Admission Type"n / fisher nocol norow nopercent;
run;
*looks like 1-healthy are all in Elective;
proc logistic data=practpro.combined2a;
    class 'ASA Class'n (ref="2 - Mild Systemic Disease") / param=ref;
    model 'Admission Type'n(event='ED') = 'ASA Class'n;
    oddsratio 'ASA Class'n;
run;
*Categorical variables like ASA Class (2, 3, 4) have no inherent numeric order, 
so logistic regression needs a way to compare the effect of each level on the outcome (Admission Type). 
A reference level provides a baseline for these comparisons. Without a reference level, 
the model wouldn’t know how to make these comparisons 
because logistic regression coefficients represent differences relative to something.
In your case, choosing ASA Class 2 as the reference makes sense 
because it likely represents a moderate level of illness and 
is a logical comparison point for ASA Class 3 (severe) and ASA Class 4 (incapacitating).;

*Results from prior code for logistic*;
*ASA Class 3: Estimate: 1.6571, 
Positive log-odds suggest higher likelihood of ED admission compared to ASA Class 2
The Wald Chi-Square test is significant (p < 0.0001), confirming this effect.

ASA Class 4: Estimate: 2.9137,
Strongly positive log-odds suggest much higher likelihood of ED admission compared to ASA Class 2
Highly significant effect (p < 0.0001);

*Comparing ASA Class 2 only this time*;

*Comparing discharge dispo*;
proc freq data= practpro.combined2a;
    tables "Discharge Disposition"n*"Admission Type"n / nocol norow nopercent;
run;
proc logistic data=practpro.combined2a;
    class 'Discharge Disposition'n (ref="Home (Self Care)") / param=ref;
    model 'Admission Type'n(event='ED') = 'Discharge Disposition'n;
    oddsratio 'Discharge Disposition'n;
*Expired in-house Chisq:4.6220	pvalue:0.0316		Discharge Dispositio Expired in-house vs Home (Self Care)	Point Est:14.208	Wald CI: 1.264	159.689
Home Health Service	13.6763		0.0002		Discharge Dispositio Home Health Service vs Home (Self Care)	2.622	1.573	4.370
Discharge Dispositio	Other Rehab Facility	5.4916	0.0191	Discharge Dispositio Other Rehab Facility vs Home (Self Care)	4.736	1.290	17.391	
Discharge Dispositio	Other Skilled Nursing Facility	76.7890	<.0001		Discharge Dispositio Other Skilled Nursing Facility vs Home (Self Care)	6.569	4.312	10.009
Discharge Dispositio	12	79.9353	<.0001 (generally significant);

*Using ANOVA to compare LOS and Admission Type;
*Need to check for normality first;
proc univariate data=practpro.combined2a;
    class 'Admission Type'n;
    var 'LOS Hours'n;
    histogram / normal;
run; *data is not normally distributed;
*Run the log to try to normalize*;
data practpro.combined3a;
    set practpro.combined2a;
    logLOSHours = log('LOS Hours'n); /* Log transformation of LOS */
run;

proc anova data=practpro.combined3a;
    class 'Admission Type'n;  /* Categorical variable: Admission Status */
    model logLOSHours = 'Admission Type'n;  /* Transformed LOS variable */
run;
proc univariate data=practpro.combined3a;
    class 'Admission Type'n;
    var logLOSHours;
    histogram / normal;
    run;
*It is now more normally distibuted*;
*Checking to see if variance is equal amongst groups (homogeneity) i.e. Levenes Test >0.05;
proc glm data=practpro.combined3a;
    class 'Admission Type'n;
    model logLOSHours = 'Admission Type'n;
    means 'Admission Type'n / hovtest=levene;
run;
*Variance is equal amongst groups;
*Now doing ANOVA;
proc anova data=practpro.combined3a;
    class 'Admission Type'n;     /* Categorical variable: Admission Status */
    model logLOSHours = 'Admission Type'n; /* Continuous variable: LOS */
   means 'Admission Type'n / tukey; /* Request group means table */
run;
*F-value:249.21, pvalue <0.0001; 	

*Getting baseline characteristics;
/* Step 1: Generate frequencies and percentages for categorical variables */
proc freq data=practpro.combined3a;
    tables 'Admission Type'n * (Race Ethnicity Sex 'Marital Status'n 'Employment Status'n
                            'County/Residence'n 'BMI Group Name'n 'ASA Class'n
                            Diabetes Hypertension Smoking Arthritis Hyperlipidemia
                            Insurance 'PCP at Time of Procedure'n
                            'Public or Private Insurance'n 'Language Barrier'n
                            'Discharge Disposition'n) / nopercent nocum chisq;
run;
/* Step 2: Calculate means and standard deviations for continuous variables */
proc means data=practpro.combined3a mean stddev median min max;
    class 'Admission Type'n;
    var Age 'LOS Hours'n 'LOS Days'n;
run;

**Confounders**;

    proc logistic data=practpro.combined3ab;
   class 'PCP at Time of Procedure'n  (param=ref);
   model 'Admission Type'n(event='ED') = 'PCP at Time of Procedure'n ;
   where not missing('PCP at Time of Procedure'n); /* Exclude rows where Variable1 or Variable2 is missing */
run; **adjusted point estimate:5.962, CI:3.763	9.446;

proc logistic data=practpro.combined3ab;
   class hyperlipidemia arthritis 'PCP at Time of Procedure'n sex race insurance 
         diabetes hypertension 'ASA Class'n / param=ref ref=first missing;
   model 'Admission Type'n (event='ED') = hyperlipidemia arthritis 'PCP at Time of Procedure'n 
                                      age sex race insurance diabetes hypertension 'ASA Class'n;
   output out=results p=predicted;
run;
*Arthritis Yes vs No	Point Est: 0.326	CI:0.203	0.526
Hyperlipidemia Yes vs No	0.482	0.300	0.775;

proc freq data=practpro.combined3ab;
   tables 'PCP at time of procedure'n * 'BMI Group Name'n * hyperlipidemia * 'Admission Type'n  / chisq ;
run;

proc freq data=practpro.combined3ab;
tables sex * 'BMI Group Name'n * Diabetes * Hypertension * Hyperlipidemia * Smoking * 'ASA Class'n * Arthritis * 'Admission Type'n/ chisq;
run;

proc freq data=practpro.combined3ab;
tables race * insurance * hypertension * diabetes * "PCP at time of procedure"n * 'Admission Type'n / chisq;
run;


proc freq data=practpro.combined3a;
    tables 'Admission Type'n*(Hyperlipidemia Arthritis PCP 'Surgery Type'n Ethnicity Race Sex "Marital Status"n "County/Residence"n "BMI Group Name"n "ASA Class"n Diabetes Hypertension Smoking Insurance 'PCP at time of procedure'n "Public or Private Insurance"n "Language Barrier"n "Discharge Disposition"n   ) / chisq; /* Chi-square test */
run;

data practpro.combined4;
set practpro.combined3ab;
proc univariate data=practpro.combined4;
run;
*mean is 62.5 so use 63*;
data practpro.combined4;
set practpro.combined3ab;
if Age < 63 then AgeCat=0;*young;
if Age >= 63 then AgeCat=1;*old;
run;
proc logistic data=practpro.combined4;
class 'Admission Type'n (ref="Elective") / param=reference;
model 'Admission Type'n = AgeCat;
run;
*no signifigance*;
proc freq data=practpro.combined4;
tables hypertension * diabetes * arthritis * AgeCat * 'Admission Type'n / chisq;
run;
*no significance even after controlling*;

*ASA Class Analysis**;
proc freq data=practpro.combined4;
run; 

data practpro.combined5;
set practpro.combined4;
if 'ASA Class'n= '1 - Healthy' then 'ASA ClassCat1'n=1;
else 'ASA ClassCat1'n=0;
if 'ASA Class'n= '2 - Mild Systemic Disease' then 'ASA ClassCat2'n=1;
else 'ASA ClassCat2'n=0;
if 'ASA Class'n= '3 - Severe Systemic Disease' then 'ASA ClassCat3'n=1;
else 'ASA ClassCat3'n=0;
if 'ASA Class'n= '4 - Incapacitating Disease' then 'ASA ClassCat4'n=1;
else 'ASA ClassCat4'n=0;
run;

proc logistic data=practpro.combined5;
class 'Admission Type'n (ref='Elective') / param=reference;
model 'Admission Type'n = 'ASA ClassCat1'n;
run;
proc logistic data=practpro.combined5;
class 'Admission Type'n (ref='Elective') / param=reference;
model 'Admission Type'n = 'ASA ClassCat2'n;
run;
proc logistic data=practpro.combined5;
class 'Admission Type'n (ref='Elective') / param=reference;
model 'Admission Type'n = 'ASA ClassCat3'n;
run;
proc logistic data=practpro.combined5;
class 'Admission Type'n (ref='Elective') / param=reference;
model 'Admission Type'n = 'ASA ClassCat4'n;
run;
*2, 3 and 4 are significant;
data practpro.combined5;
set practpro.combined5;
if 'BMI Group Name'n = 'Underweight' then Underweight=1;
else Underweight=0;
if 'BMI Group Name'n = 'Normal Weight' then Normalweight=1;
else Normalweight=0;
if 'BMI Group Name'n = 'Overweight' then Overweight=1;
else Overweight=0;
if 'BMI Group Name'n = 'Obesity' then Obesity=1;
else Obesity=0;

**ASA CLASS 2**;
proc freq data=practpro.combined5;
tables   Underweight*Normalweight*Overweight*Obesity*	'ASA ClassCat2'n*"Admission Type"n / chisq;	
run;
*Associated*;
proc freq data=practpro.combined5;
tables   AgeCat*	'ASA ClassCat2'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Hypertension*	'ASA ClassCat2'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Diabetes*	'ASA ClassCat2'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Hyperlipidemia*	'ASA ClassCat2'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Arthritis*	'ASA ClassCat2'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Smoking*	'ASA ClassCat2'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Sex*	'ASA ClassCat2'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Race*	'ASA ClassCat2'n*"Admission Type"n / chisq;	
run;
*Associated;

proc logistic data=practpro.combined5;
 class Sex (ref="F") 'BMI Group Name'n(ref="Normal Weight") Hypertension(ref='No') Hyperlipidemia (ref='No') Diabetes(ref='No') Arthritis (ref='No') Smoking (ref='No') Race (ref='White'); /* Specify categorical confounders */
 model 'Admission Type'n(event="ED") = 'ASA ClassCat2'n Hypertension 'BMI Group Name'n AgeCat Sex  Diabetes Hyperlipidemia Arthritis Smoking Race;
run;


*ASA CLASS 3**;
proc freq data=practpro.combined5;
tables   Underweight*Normalweight*Overweight*Obesity*	'ASA ClassCat3'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   AgeCat*	'ASA ClassCat3'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Hypertension*	'ASA ClassCat3'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Diabetes*	'ASA ClassCat3'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Hyperlipidemia*	'ASA ClassCat3'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Arthritis*	'ASA ClassCat3'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Smoking*	'ASA ClassCat3'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Sex*	'ASA ClassCat3'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Race*	'ASA ClassCat3'n*"Admission Type"n / chisq;	
run;
*BMI Group Name=Obesity AgeCat=0, Controlling for BMI Group Name=Overweight AgeCat=0 Keep fpr BMI;
* Remove hypertension, Diabetes, hyperlipidemia, arthritis, smoking, Underweight, Normal Weight;
proc logistic data=practpro.combined5;
 class Sex (ref="F")  Race (ref='White'); /* Specify categorical confounders */
 model 'Admission Type'n(event="ED") = 'ASA ClassCat3'n Overweight Obesity AgeCat Sex Race;
run;

*ASA Class 4*;
proc freq data=practpro.combined5;
tables   Underweight*Normalweight*Overweight*Obesity*	'ASA ClassCat4'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   AgeCat*	'ASA ClassCat4'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Hypertension*	'ASA ClassCat4'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Diabetes*	'ASA ClassCat4'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Hyperlipidemia*	'ASA ClassCat4'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Arthritis*	'ASA ClassCat4'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Smoking*	'ASA ClassCat4'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Sex*	'ASA ClassCat4'n*"Admission Type"n / chisq;	
run;

proc freq data=practpro.combined5;
tables   Race*	'ASA ClassCat4'n*"Admission Type"n / chisq;	
run;
*Remove: Normal Weight, Underweight;
proc logistic data=practpro.combined5;
 class Sex (ref="F")  Race (ref='White') Hypertension(ref='No') Arthritis(ref='No') Smoking(ref='No');  /* Specify categorical confounders */
 model 'Admission Type'n(event="ED") = 'ASA ClassCat4'n Overweight Obesity AgeCat Sex Race Smoking Arthritis Hypertension;
run;

***30 day ed return;
proc freq data=practpro.combined5;
tables 'Has 30 Day ED Return'n * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined5;
tables 'ASA ClassCat4'n*'ASA ClassCat3'n*'ASA ClassCat2'n* 'Has 30 Day ED Return'n * 'Admission Type'n / chisq;
run;
*Remove Class4;
proc freq data=practpro.combined5;
tables Underweight*Normalweight*Overweight*Obesity* 'Has 30 Day ED Return'n * 'Admission Type'n / chisq;
run;
*Remove Underweight;
proc freq data=practpro.combined5;
tables Hypertension* 'Has 30 Day ED Return'n * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined5;
tables Hyperlipidemia* 'Has 30 Day ED Return'n * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined5;
tables Arthritis* 'Has 30 Day ED Return'n * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined5;
tables Smoking* 'Has 30 Day ED Return'n * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined5;
tables Diabetes* 'Has 30 Day ED Return'n * 'Admission Type'n / chisq;
run;
*Remove Smoking;
proc freq data=practpro.combined5;
tables AgeCat* 'Has 30 Day ED Return'n * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined5;
tables 'Discharge Disposition'n* 'Has 30 Day ED Return'n * 'Admission Type'n / chisq;
run;
*Recode Home(Self Care), Home Health Service;
proc freq data=practpro.combined5;
tables "PCP at Time of Procedure"n* 'Has 30 Day ED Return'n * 'Admission Type'n / chisq;
run;
*Remove;

data practpro.combined6;
set practpro.combined5;
if 'Discharge Disposition'n = "Home (Self Care)" then HomeSelfCareCat=1;
else HomeSelfCareCat=0;
if 'Discharge Disposition'n = "Home Health Service" then HomeHealthServiceCat=1;
else HomeHealthServiceCat=0;
run;

proc logistic data=practpro.combined6;
 class Diabetes(ref='No') Hyperlipidemia(ref='No') Hypertension(ref='No') Arthritis(ref='No');  /* Specify categorical confounders */
 model 'Admission Type'n(event="ED") = 'Has 30 Day ED Return'n 'ASA ClassCat3'n 'ASA ClassCat2'n HomeSelfCareCat HomeHealthServiceCat Normalweight Overweight Obesity AgeCat Diabetes Hyperlipidemia  Arthritis Hypertension;
run;

**Has 30 day IP Readmit;
proc freq data=practpro.combined6;
tables 'Has 30 Day IP readmit'n * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined6;
tables 'ASA ClassCat4'n*'ASA ClassCat3'n*'ASA ClassCat2'n* 'Has 30 Day IP readmit'n * 'Admission Type'n / chisq;
run;
*Remove Class2, Class4;

proc freq data=practpro.combined6;
tables Underweight*Normalweight*Overweight*Obesity* 'Has 30 Day IP readmit'n * 'Admission Type'n / chisq;
run;
*Remove Overweight, Normalweight,Underweight;
 
proc freq data=practpro.combined6;
tables Hypertension* 'Has 30 Day IP readmit'n * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined6;
tables Hyperlipidemia* 'Has 30 Day IP readmit'n * 'Admission Type'n / chisq;
run;
*Remove Hyperlipidemia;

proc freq data=practpro.combined6;
tables Arthritis* 'Has 30 Day IP readmit'n * 'Admission Type'n / chisq;
run;
*Remove Arthritis;

proc freq data=practpro.combined6;
tables Smoking* 'Has 30 Day IP readmit'n * 'Admission Type'n / chisq;
run;
*Remove Smoking;

proc freq data=practpro.combined6;
tables Diabetes* 'Has 30 Day IP readmit'n * 'Admission Type'n / chisq;
run;
*Remove Diabetes;

proc freq data=practpro.combined6;
tables AgeCat* 'Has 30 Day IP readmit'n * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined6;
tables HomeSelfCareCat* 'Has 30 Day IP readmit'n * 'Admission Type'n / chisq;
run;
*Remove;

proc freq data=practpro.combined6;
tables HomeHealthServiceCat* 'Has 30 Day IP readmit'n * 'Admission Type'n / chisq;
run;
*Remove;

proc freq data=practpro.combined6;
tables "PCP at Time of Procedure"n* 'Has 30 Day IP readmit'n * 'Admission Type'n / chisq;
run;

proc logistic data=practpro.combined6;
 class Hypertension(ref='No') "PCP at Time of Procedure"n(ref='No');  /* Specify categorical confounders */
 model 'Admission Type'n(event="ED") = 'Has 30 Day IP readmit'n Obesity AgeCat Hypertension "PCP at Time of Procedure"n 'ASA ClassCat3'n;
run;

*SNFs/Discharge Dispo Home Self Care;
proc freq data=practpro.combined6;
tables 'Discharge Disposition'n * 'Admission Type'n / chisq;
run;
*Recode Other Skilled Nursing Facility;
Data practpro.combined7;
set practpro.combined6;
if 'Discharge Disposition'n = 'Other Skilled Nursing Facility' then OSNFCat = 1;
else OSNFCat = 0;
run;
*Confounders: Age, comorbidities, marital status, ASA Class;
proc freq data=practpro.combined7;
tables AgeCat * HomeSelfCareCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables Diabetes * HomeSelfCareCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables Hypertension * HomeSelfCareCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables Hyperlipidemia * HomeSelfCareCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables Arthritis * HomeSelfCareCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables 'Marital Status'n * HomeSelfCareCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables 'ASA ClassCat4'n*'ASA ClassCat3'n*'ASA ClassCat2'n  * HomeSelfCareCat * 'Admission Type'n / chisq;
run;
*Remove ASA 4 out;
proc logistic data=practpro.combined7;
class Diabetes(ref="No") Hypertension(ref="No") Hyperlipidemia(ref="No") Arthritis(ref="No") 'Marital Status'n(ref="Yes");
model "Admission Type"n (event='ED') = HomeSelfCareCat  Diabetes  Hypertension  Hyperlipidemia  Arthritis  'Marital Status'n  'ASA ClassCat3'n 'ASA ClassCat2'n;
run;

*SNFs/Discharge Dispo Home Self Service;
proc freq data=practpro.combined7;
tables HomeHealthServiceCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables AgeCat * HomeHealthServiceCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables Diabetes * HomeHealthServiceCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables Hypertension * HomeHealthServiceCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables Hyperlipidemia * HomeHealthServiceCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables Arthritis * HomeHealthServiceCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables 'Marital Status'n * HomeHealthServiceCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables 'ASA ClassCat4'n*'ASA ClassCat3'n*'ASA ClassCat2'n  * HomeHealthServiceCat * 'Admission Type'n / chisq;
run;
*Remove AgeCat, Comorbidities, Marital Status, All ASA;
proc logistic data=practpro.combined7;
model "Admission Type"n (event='ED') = HomeHealthServiceCat;
run;
*SNFs/Discharge Dispo Home Self Service;
proc freq data=practpro.combined7;
tables OSNFCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables AgeCat * OSNFCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables Diabetes * OSNFCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables Hypertension * OSNFCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables Hyperlipidemia * OSNFCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables Arthritis * OSNFCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables 'Marital Status'n * OSNFCat * 'Admission Type'n / chisq;
run;
proc freq data=practpro.combined7;
tables 'ASA ClassCat4'n*'ASA ClassCat3'n*'ASA ClassCat2'n  * OSNFCat * 'Admission Type'n / chisq;
run;
proc logistic data=practpro.combined7;
class Diabetes(ref="No") Hypertension(ref="No") Hyperlipidemia(ref="No") Arthritis(ref="No") 'Marital Status'n(ref="Yes");
model "Admission Type"n (event='ED') = OSNFCat  AgeCat Diabetes  Hypertension  Hyperlipidemia  Arthritis  'Marital Status'n 'ASA ClassCat4'n 'ASA ClassCat3'n 'ASA ClassCat2'n;
run;