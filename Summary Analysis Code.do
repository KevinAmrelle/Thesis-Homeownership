clear
set more off
local year=2010
use "C:\Users\Kev\Desktop\Scfdata 4-5\Completed for Summary\2010f" , clear

*Same filters as in flavin aer pp paper
*keep if age>=24 & age<=89 & networth>=0

sort YY1
 
replace wgt=wgt/5

/*
For users who want to estimate only simple statistics such as sums,
means and medians ignoring the effects of imputation error on the
standard errors of these estimates, it will probably be sufficient to
divide the weights by 5.  Software to compute means and medians and
their associated standard errors with respect to imputation and
sampling error is provided in the section on sampling error later in
this codebook.
*/
 

/*HHOUSES label
  have owned principal residence: 1=yes, 0=no;
*/
label define hhouses 0 "No" 1 "Yes"


/* SEX label
 1.    *MALE
 2.    *FEMALE
 0.     Inap. (/no further persons)

*/ 

label define sex 0 "N/A" 1 "Male" 2 "Female"

/*Race lable prior to 1998

X5909      Are you Native American, Asian, Hispanic, black, white, or
           another race?

           (SHOW CARD 16)

           Code group
            1.  Native American/Eskimo/Aleut
            2.  Asian or Pacific Islander
            3.  Hispanic
            4.  Black or African-American
            5.  White
           -7.  Other
           ********************************************************
              FOR THE PUBLIC DATASET, CODES 1 AND 2 ARE COMBINED
              WITH -7. 



RACE label 1998 and after
*     1=white non-Hispanic, 2=black/African-American, 3=Hispanic,
      4=Asian (only available in internal data set, see codebook), 
      5=other;
*/
label define racecl 1 "white non-Hispanic" 2 "black/African-American" 3 "Hispanic" -7 "other"



/* EDUCATION label
*   education of the HH head, and categorical variable:
    1=no high school diploma/GED, 2=high school diploma or GED,
    3=some college, 4=college degree;
    EDUC=X5901;
    IF X5904 EQ 1 THEN EDCL=4;
    ELSE IF EDUC GE 13 THEN EDCL=3;
    ELSE IF (X5902 IN (1 2)) THEN EDCL=2;
    ELSE EDCL=1;
*/
label define edcl 1 "no high school diploma/GED" 2 "high school diploma or GED" 3 "some college" 4 "college degree"



/* MARRIED label

X8023(#1)       (Are you/Is your [RELATIONSHIP] currently married or
X105(#2)        living with a partner, separated, divorced,
X111(#3)        widowed, or (have you/has [he/she]) never been married?
X117(#4)        
X123(#5)        (NOTE: if R lives with a partner who is financially
X129(#6)        interdependent, this variable is always coded '2' for the
X135(#7)        head and partner.  The legal marital status of R and of the
X205(#8)        partner are given by X7372 and X7018 respectively.)
X211(#9)
X217(#10)            1.    *MARRIED
X223(#11)            2.    *LIVING WITH PARTNER
X229(#12)            3.    *SEPARATED
                     4.    *DIVORCED
                     5.    *WIDOWED
                     6.    *NEVER MARRIED
                     0.     Inap. (person age 17 or less; No Further persons)
*/
label define married 1 "married" 2 "LIVING WITH PARTNER" 3 "SEPARATED" 4 "DIVORCED" 5 "WIDOWED" 6 "NEVER MARRIED"



/*FAMSTRUCT label
    IF MARRIED NE 1 AND KIDS >= 1 THEN FAMSTRUCT=1;
    ELSE IF MARRIED NE 1 AND KIDS = 0 AND AGE<55 THEN FAMSTRUCT=2;
    ELSE IF MARRIED NE 1 AND KIDS = 0 AND AGE>54 THEN FAMSTRUCT=3;
    ELSE IF MARRIED EQ 1 AND KIDS >= 1 THEN FAMSTRUCT=4;
    ELSE IF MARRIED EQ 1 AND KIDS =0 THEN FAMSTRUCT=5;
    ELSE PUT "ERROR: UNCLASSIFIED FAMILY STRUCTURE " Y1= MARRIED= KIDS= AGE=;
*/
label define famstruct 1 "not married with >=1 kid" 2 "not married, no kids and younger then 55" 3 "not married, no kids and older then 55" 4 "married with >=1 kid" 5 "married with no kid"

/*Household Size Variable
X101            Number of people in the household according to the HHL.
                Excludes people included in the household listing who do
                not usually live there and who are financially independent.
                 
                TOTAL # OF PERSONS IN HHL:
                    12.     12 or more people
*/



/* HEALTH labels	
Would you say your spouse's health in
	general is excellent, good, fair, or poor?

                     1.    *Excellent
                     2.    *Good
                     3.    *Fair
                     4.    *Poor
                     0.     Inap. (no spouse/partner)
*/	
label define healthhh 0 "Inap." 1 "Excellent" 2 "Good" 3 "Fair" 4 "Poor"
label define healths 0 "Inap. (no spouse/partner)" 1 "Excellent" 2 "Good" 3 "Fair" 4 "Poor"







/* Full-time or Part-time work label
 Thinking about all your (husband/wife/partner/spouse)'s
      current work for pay, (do you/does he/does she/does he or she)
                consider (yourself/himself/herself/himself or herself) to be
                working full-time or part-time?
                
                IF R IS LAID OFF OR A SEASONAL WORKER, ASK ABOUT 
                'JOBS WHEN R IS WORKING'.

                     1.    *Full-time
                     2.    *Part-time
                     0.     Inap. (not doing and work for pay: X4105=5/
                            X4705=5; no spouse/partner;
                            volunteer work not considered a job:
                            X7591=5/X7589=5)
*/
label define workfthh 0 "Inap." 1 "Full-time" 2 "Part-time" 
label define workfts 0 "Inap." 1 "Full-time" 2 "Part-time" 



/*CREDIT labels

X407            In the past five years, has a particular lender or creditor
                turned down any request you (or your
                {husband/wife/partner}) made for credit, or not given you
                as much credit as you applied for?
                
                IF YES, PROBE: Were you turned down, or did you not get as much
                as you applied for?

                IF TURNED DOWN AND NOT AS MUCH CREDIT, ASK WHICH IS MORE
                RECENT.

                     1.    *Yes, turned down
                     3.    *Yes, not as much credit
                     5.    *No
                     0.     Inap. (no credit application in previous 5 years:
                            X7131=5)
X408            [Were you later able to obtain the full amount you or your
                (husband/wife/partner) requested by reapplying to the same
                institution or by applying elsewhere?/
                Were you later able to obtain the full amount you requested
                by reapplying to the same institution or by applying
                elsewhere?]

                     1.    *YES
                     3.    *Did Not Reapply
                     5.    *NO
                     0.     Inap. (no credit application in previous 5 years:
                            X7131=5; not turned down: X407=5)
X409            [Was there any time in the past five years that you or your
                (husband/wife/partner) thought of applying for credit at a
                particular place, but changed your mind because you thought
                you might be turned down?/
                Was there any time in the past five years that you thought
                of applying for credit at a particular place, but changed
                your mind because you thought you might be turned down?

                     1.    *YES
                     5.    *NO
*/

label define credit1 0 "Inap." 1 "Yes, turned down" 3 "Yes, not as much credit" 5 "No"
label define credit2 0 "Inap." 1 "Yes" 3 "Did Not Reapply" 5 "No"
label define credit3 1 "Yes" 5 "No"


/*

X4513(#1)       Including any self-employment and your (husband/wife/partner/
X5113(#2)       spouse)'s current job, for how many different employers
                (have you/has he/has she/has he or she) worked in full-time
                jobs lasting one year or more?
                
                TREAT SELF-EMPLOYMENT AS ONE EMPLOYER.
                
                NUMBER OF EMPLOYERS:
                    -1.     None
                     0.     Inap. (not doing any work for pay: X4105=5/
                            X4705=5; no spouse/partner; not
                            currently working full-time: X4511^=1/
                            X5111^=1; less than one year of
                            full-time employment: X4512<1/
                            X5112<1;
                            volunteer work not considered a job:
                            X7591=5/X7589=5)
*/
/* Income

X5702           In total, what was your (family's) annual income from
                wages and salaries in 2012, before deductions for taxes and
                anything else?

                WE WANT TOTAL INCOME FOR THE YEAR, NOT MONTHLY INCOME.

                INCLUDE OVERTIME, BONUSES, AND TIPS.

                IRS FORM 1040 LINE NUMBER: 7
                
                ANNUAL $ AMOUNT IN 2012:
                     0.     Inap. (no wage income: X5701^=1)
*/

/*Saving for a home- SAVRES4 dummy

reasons for saving: 1=cant save, 2=education, 3=family, 4=home,
    5=purchases, 6=retirement, 7=liquidity/the future, 8=investment,
    9=no particular reason;
*   NOTE: multiple saving reasons may be reported: here choosing only
    first (most important) reason;
*/


*Less than College Variable Creation
gen LessCollege = 0
replace LessCollege = 1 if edcl==1
replace LessCollege = 1 if edcl==2
label define LessCollege 0 "No" 1 "Yes"
*Some College Variable Creation
gen SomeCollege = 0
replace SomeCollege = 1 if edcl==3
label define SomeCollege 0 "No" 1 "Yes"
*College Degree Variable Creation
gen CollegeDegree = 0
replace CollegeDegree = 1 if edcl==4
label define CollegeDegree 0 "No" 1 "Yes"



*Single Male Variable Creation
gen male = 0
replace male = 1 if sex==1
replace male = 0 if sex==2
replace male = 0 if sex==0
generate single = 0
replace single = 1 if married==2
replace single = 1 if married==3
replace single = 1 if married==4
replace single = 1 if married==5
replace single = 1 if married==6
replace single = 1 if married==0
generate byte singlemale = male & single
label define singlemale 0 "No" 1 "Yes"
*Married Variable Creation
gen Married = 0
replace Married = 1 if married==1
label define Married 0 "No" 1 "Yes"
*Divorced Variable Creation
gen Divorced = 0
replace Divorced = 1 if married==4
label define Divorced 0 "No" 1 "Yes"



*AfricanAmerican Variable Creation
gen AfricanAmerican = 0
replace AfricanAmerican = 1 if race==2
label define AfricanAmerican 0 "No" 1 "Yes"
*Hispanic Variable Creation
gen Hispanic  = 0
replace Hispanic  = 1 if race==3
label define Hispanic  0 "No" 1 "Yes"
*Other Race (Including Asian) Variable Creation
gen OtherRace = 0
replace OtherRace = 1 if race==-7
label define OtherRace 0 "No" 1 "Yes"

*Credit Constrained Variable Creation
gen cc1 = 0
replace cc1 = 1 if credit1==1
replace cc1 = 1 if credit1==3
gen cc2 = 0
replace cc2 = 1 if credit2==3
replace cc2 = 1 if credit2==5
gen cc3 = 0
replace cc3 = 1 if credit3==1
generate byte creditconstrained = cc1 & cc2 & cc3
label define creditconstrained 0 "No" 1 "Yes"
*Not Credit Constrained Variable Creation
gen ncc1 = 0
replace ncc1 = 1 if credit1==5
*replace ncc1 = 1 if credit1==0
*gen ncc2 = 0
*replace ncc2 = 1 if credit2==1
*replace ncc2 = 1 if credit2==0
gen ncc3 = 0
replace ncc3 = 1 if credit3==5
generate byte notcreditconstrained = cc1 & cc3
label define notcreditconstrained 0 "No" 1 "Yes"


gen not_cc_NEW=0
*1989 and 1992
*replace not_cc_NEW=1 if credit1==5 & credit3==5
*After or during 1995
replace not_cc_NEW=1 if credit1==5 | (credit1==0 & credit3==5) & year











*Age Group Dummy Variable Creation
recode age(min/34=1)(35/54=2)(55/max=3), gen(age_group)
label define age_group 1 "Age x Dummy for under 35" 2 "Age x Dummy for between 35 and 55" 3 "Age X Dummy for over 55"
*Age x Dummy for under 35 Variable Creation
gen age1 = 0
replace age1 = 1 if age<=34
*Age x Dummy for between 35 and 55 Variable Creation
gen age2 = 0
replace age2 = 1 if age>34 & age<=55
*Age X Dummy for over 55 Variable Creation
gen age3 = 0
replace age3 = 1 if age>55
gen agejue1=age1*age
gen agejue2=age2*age
gen agejue3=age3*age




*Homeownership by race 
gen HO_White = 0
replace HO_White=1 if hhouses==1 & race==1
gen HO_White1= (HO_White)/(race==1)
gen HO_Black = 0
replace HO_Black=1 if hhouses==1 & race==2
gen HO_Black1= (HO_Black)/(race==2)
gen HO_Hispanic = 0
replace HO_Hispanic=1 if hhouses==1 & race==3
gen HO_Hispanic1= (HO_Hispanic)/(race==3)
*HomeSavings by race from 1998 and after
gen HS_Owner = 0
replace HS_Owner=1 if SAVRES4==1 & hhouse==1
gen HS_Owner1= (HS_Owner)/(SAVRES4)
gen HS_Renter = 0
replace HS_Renter=1 if SAVRES4==1 & hhouse==0
gen HS_Renter1= (HS_Renter)/(hhouse==0)
gen HS_White = 0
replace HS_White=1 if HS_Renter==1 & race==1
gen HS_WR = 0
replace HS_WR=1 if race==1 & hhouse==0
gen HS_White1= (HS_White)/(HS_WR)
gen HS_Black = 0
replace HS_Black=1 if HS_Renter==1 & race==2
gen HS_BR = 0
replace HS_BR=1 if race==2 & hhouse==0
gen HS_Black1= (HS_Black)/(HS_BR)
gen HS_Hispanic = 0
replace HS_Hispanic=1 if HS_Renter==1 & race==3
gen HS_HR = 0
replace HS_HR=1 if race==3 & hhouse==0
gen HS_Hispanic1= (HS_Hispanic)/(HS_HR)
*Not Credit constrained by race from 1998 and after 
gen NCC_White = 0
replace NCC_White=1 if not_cc_NEW==1 & race==1
gen NCC_White1= (NCC_White)/(race==1)
gen NCC_Black = 0
replace NCC_Black=1 if not_cc_NEW==1 & race==2
gen NCC_Black1= (NCC_Black)/(race==2)
gen NCC_Hispanic = 0
replace NCC_Hispanic=1 if not_cc_NEW==1 & race==3
gen NCC_Hispanic1= (NCC_Hispanic)/(race==3)


*Homeownership by Age group from 1998 and after
gen HO_Age1p = 0
replace HO_Age1p=1 if hhouses==1 & age1==1
gen HO_Age1= (HO_Age1p)/(age1==1)
gen HO_Age2p = 0
replace HO_Age2p=1 if hhouses==1 & age2==1
gen HO_Age2= (HO_Age2p)/(age2==1)
gen HO_Age3p = 0
replace HO_Age3p=1 if hhouses==1 & age3==1
gen HO_Age3= (HO_Age3p)/(age3==1)


*Head in bad health Variable Creation
gen hhbadhealth = 0
replace hhbadhealth = 1 if healthhh==4
*Spouse in bad health Variable Creation
gen sbadhealth = 0
replace sbadhealth = 1 if healths==4



*Jobtime <1 year Variable Creation
gen jobtimeshort = 0
replace jobtimeshort = 1 if jobtime==0
replace jobtimeshort = 1 if jobtime==-1
*Jobtime >1 year Variable Creation
gen jobtimelong = 0
replace jobtimelong = 4 if jobtimeshort==0





*Head works full-time Variable Creation
gen headft = 0
replace headft = 1 if workfthh==1
*Spouse works full-time Variable Creation
gen spouseft = 0
replace spouseft = 1 if workfts==1
*Spouse works part-time Variable Creation
gen spousept = 0
replace spousept = 1 if workfts==2




*Adjust Income for inflation in $2001
/*replace income = income*1.427 if year == 1989
replace income = income*1.262 if year == 1992
replace income = income*1.161 if year == 1995
replace income = income*1.086 if year == 1998
*/


*Adjust Income for inflation in $2013
replace income = income*1.879 if year == 1989
replace income = income*1.660 if year == 1992
replace income = income*1.529 if year == 1995
replace income = income*1.429 if year == 1998
replace income = income*1.316 if year == 2001
replace income = income*1.316 if year == 2001
replace income = income*1.233 if year == 2004
replace income = income*1.124 if year == 2007
replace income = income*1.068 if year == 2010
*/

drop if income >= 1000000
*drop if income < 0
replace income = income/1000
gen income2=income^2
replace jobtime = 0 if jobtime == -1

order hhouses not_cc_NEW LessCollege SomeCollege CollegeDegree Married singlemale Divorced agejue1 agejue2 agejue3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime

summarize hhouses not_cc_NEW LessCollege SomeCollege CollegeDegree Married singlemale Divorced agejue1 agejue2 agejue3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime  [aweight=wgt],d
/*mean hhouses not_cc_NEW LessCollege SomeCollege CollegeDegree Married singlemale Divorced agejue1 agejue2 agejue3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime [aweight=wgt]
regress hhouses not_cc_NEW LessCollege SomeCollege CollegeDegree Married singlemale Divorced agejue1 agejue2 agejue3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime [aweight=wgt]
summarize hhouses HO_White1 HO_Black1 HO_Hispanic1 [aweight=wgt],d
summarize HO_Age1 HO_Age2 HO_Age3 [aweight=wgt],d
summarize SAVRES4 HS_White1 HS_Black1 HS_Hispanic1 [aweight=wgt],d 
*/


/*
*Replicating JUE Table 3
regress hhouses SomeCollege CollegeDegree Married singlemale Divorced agejue1 agejue2 agejue3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime
probit hhouses i.SomeCollege i.CollegeDegree i.Married i.singlemale i.Divorced agejue1 agejue2 agejue3 hhsize i.AfricanAmerican i.Hispanic i.OtherRace i.hhbadhealth i.sbadhealth income income2 i.headft i.spouseft i.spousept jobtime if replicate_no==1
margins [aweight=wgt], dydx(SomeCollege CollegeDegree Married singlemale Divorced agejue1 agejue2 agejue3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime) noesample
*/

*Replicating JUE Table 4
regress hhouses SomeCollege CollegeDegree Married singlemale Divorced agejue1 agejue2 agejue3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime
heckprobit hhouses i.SomeCollege i.CollegeDegree i.Married i.singlemale i.Divorced agejue1 agejue2 agejue3 hhsize i.AfricanAmerican i.Hispanic i.OtherRace i.hhbadhealth i.sbadhealth income income2 i.headft i.spouseft i.spousept jobtime if replicate_no==1, select(not_cc_NEW = i.SomeCollege i.CollegeDegree i.Married i.singlemale i.Divorced agejue1 agejue2 agejue3 hhsize i.AfricanAmerican i.Hispanic i.OtherRace i.hhbadhealth i.sbadhealth income income2 i.headft i.spouseft i.spousept jobtime) 
*biprobit hhouses not_cc_NEW i.SomeCollege i.CollegeDegree i.Married i.singlemale i.Divorced agejue1 agejue2 agejue3 hhsize i.AfricanAmerican i.Hispanic i.OtherRace i.hhbadhealth i.sbadhealth income income2 i.headft i.spouseft i.spousept jobtime if replicate_no==1
margins [aweight=wgt], dydx(SomeCollege CollegeDegree Married singlemale Divorced agejue1 agejue2 agejue3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime) noesample
*/


*Shift-Share Analysis
/*
summarize hhouses not_cc_NEW LessCollege SomeCollege CollegeDegree Married singlemale Divorced agejue1 agejue2 agejue3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime  [aweight=wgt], d, if year==2001
predict yhat, xb if year==2004
summarize yhat
*/





*bysort year: summarize hhouses not_cc_NEW [aweight=wgt],d
*probit hhouses SomeCollege CollegeDegree Married singlemale Divorced agejue1 agejue2 agejue3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime
/*bysort year: probit hhouses notcreditconstrained LessCollege SomeCollege CollegeDegree Married singlemale Divorced age1 age2 age3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime
probit hhouses notcreditconstrained LessCollege SomeCollege CollegeDegree Married singlemale Divorced age1 age2 age3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime if year <= 2007
probit hhouses notcreditconstrained LessCollege SomeCollege CollegeDegree Married singlemale Divorced age1 age2 age3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime if year > 2007
*/
*ttest not_cc_NEW LessCollege SomeCollege CollegeDegree Married singlemale Divorced agejue1 agejue2 agejue3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime, by (hhouses)
*regress hhouses SomeCollege CollegeDegree Married singlemale Divorced agejue1 agejue2 agejue3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime
*probit hhouses SomeCollege CollegeDegree Married singlemale Divorced agejue1 agejue2 agejue3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime
*mfx
*margins, dydx(SomeCollege CollegeDegree Married singlemale Divorced agejue1 agejue2 agejue3 hhsize AfricanAmerican Hispanic OtherRace hhbadhealth sbadhealth income income2 headft spouseft spousept jobtime), [aweight=wgt]


