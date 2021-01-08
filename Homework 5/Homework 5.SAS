

/*Q#1 (a)*/
Data Q1_A;
INFILE datalines DSD missover;
INPUT ID $ GENDER $ DOB mmddyy10. /HEIGHT WEIGHT;
format DOB mmddyy10.;
DATALINES;
001,M,06/14/1944
68,155
002,F,12/25/1967
52,99
003,M,07/04/1983
72,128
004,M,08/05/1982
70,115
005,F,09/13/1975
56,113
;

/*(b)*/
/*create data set */
DATA Q1_B;
INPUT ID $ 1-3 DOB mmddyy10.  SEX $ 12 HEIGHT 13-14 WEIGHT 15-17;
format DOB mmddyy10.;
if SEX = 'F' then output; 
DATALINES;
01204/04/77M69110
01103/06/80F55 99
01002/08/74F58120
00906/05/86M76160
00803/03/79F51 95
;


/*(c)*/
/*merge*/
proc sort data=Q1_A;
by ID;
run;

proc sort data=Q1_B;
by ID;
run;

DATA Q1_ALL;
merge Q1_A Q1_B(rename=(SEX=GENDER));
run;


/*(d)*/
DATA Q1_FINAL;
set Q1_ALL;
AGE = round(('01MAR2011'D-DOB)/365.25,1);
if AGE<=60 then Group ='Young';
else if AGE >60 then Group='Old';
drop DOB;
run;

options linesize=80 pagesize=60 pageno=1 nodate;
ods pdf file = 'Q1_final.pdf';
proc print data=Q1_FINAL;
Title 'Qeustion1 (d)';
run;
ods pdf close;


/*Question #2*/
/*(a)*/
/*Create a dataset*/
Data Q2_Asub;
INPUT ID $ SCORE1 SCORE2 @@;
DATALINES;
001 N/A 97   003 9999 85   002 98 9999
004 9999 86  005 60 N/A     006 100 100  
008 98 9999  010 N/A N/A  009 87 98
011 85 59
;

/*convert N/A to 0 and 9999 to 100*/
DATA Q2_A;
set Q2_Asub;
ARRAY SCORE[2];
Do i=1 to 2;
if SCORE[i]='N/A' then SCORE[i]=0;
else if SCORE[i]=9999 then SCORE[i]=100;
END;
DROP i;
run;

/*(b)*/
/*sort Q1_final and Q2_A by id*/
proc sort DATA =Q1_FINAL;
by ID;
run;

proc sort DATA=Q2_A;
by ID;
run;

/*merge*/
DATA Q2_FINAL;
merge Q1_FINAl(IN=Q1) Q2_A(IN=Q2);
by ID;
if Q1=1 AND Q2=1;
run;

/* print output*/
ods pdf file = 'Q2_final.pdf';
proc print data=Q2_FINAL;
Title 'Qeustion2 (d)';
run; 
ods pdf close;


/*Question #3*/
/*(a)*/
ods pdf file='Q3_(a).pdf';
proc univariate data=Q2_FINAL;
var HEIGHT;
histogram;
title 'question 3(a)';
run;
ods pdf close;

/*From the histgram HEIGHT is not normally distributed.*/
ods pdf file='Q3_(a)2.pdf';
ODS TRACE ON;
PROC TTEST DATA = Q2_FINAL;
var HEIGHT;
run;
ODS TRACE OFF;
ods pdf close;


/*From the histgram and qqplot, we can say that HEIGHT is normally distributed.*/

/*(b)*/
/*count frequency*/
Proc freq data=Q2_FINAL;
tables gender;
where SCORE1 >60 or SCORE2>60;
title 'Q3_b frequency table';
run;


/*(c)*/
/*create a new variable*/
DATA Q3_c;
set Q2_FINAL;
if GENDER='F' and Group='Young' then Gender_Group='Female and Young';
else if GENDER='F' and Group='Old' then Gender_Group='Female and Old';
else if GENDER='M' and Group='Young' then Gender_Group='Male and Young';
else if GENDER = 'M' and Group='Old' then Gender_Group='Male and Old';
run;

/*plot*/
proc sort data =Q3_c;
by Gender_Group;
run;


proc gplot data=Q3_c;
*/by Gender_Group;
title 'q3_(c) plot';
plot SCORE1*SCORE2=Gender_Group;
plot2 SCORE1*SCORE2;
run;


/*question 4*/
/*txt file*/
proc import datafile="Downloads/homeworkdata.txt" out=mydata dbms=dlm replace;
getnames=yes;
run;

/*excel file*/
proc import datafile="Documents/homeworkdata.xlsx" out=exceldata dbms=xlsx replace;
getnames=yes;
run;
