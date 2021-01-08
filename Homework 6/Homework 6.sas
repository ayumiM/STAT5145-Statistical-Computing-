

/*question 1*/
proc import DATAFILE="C:\Users\mutohai\Downloads\Data_Home_Work_6_Problems_1NEW.xls" out=SES DBMS=xls replace;
SHEET="SES";
GETNAMES=YES;
RUN;

proc print data=SES;
run;

/*a*/
%let Devar=Math;
%let Invar=Social_Studies;

proc reg data=SES;
model &Devar=&Invar;
run;

/*(b)*/
%let R=Reading;

proc means data=SES MEDIAN;
var Reading;
output out=testmedian Median=rbar;
run;


/*store median value*/
/*used symput in here*/
DATA _null_;
set testmedian;
call symput("ReadingM", rbar);
run;

%put &ReadingM;


/*(c)*/
/*used symget to get median score. I got it in question 2 with using symput*/
%macro find;
DATA q1_c;
set SES;
score=symget('ReadingM');
if (Reading ge score) then output;
run;
%mend;

%find;



/* question 2*/
/*(a)*/
proc import datafile="Downloads/Data_Home_Work_6_Problem_2.xls" out=Survey dbms=xls replace;
Getnames=yes;
run;

/*(b)*/
Data q2_b;
set Survey;
ARRAY NUM[6] Y1-Y6;
ARRAY CHAR[6] $ Q1-Q6;
Do i=1 to 6;
if NUM[i] = 1 then CHAR[i]='Strongly Disagree';
else if NUM[i] = 2 then CHAR[i]='Disagree';
else if NUM[i] = 3 then CHAR[i]='Neutral';
else if NUM[i] = 4 then CHAR[i]='Agree';
else if NUM[i] = 5 then CHAR[i]='Strongly Agree';
else if NUM[i] = 6 then CHAR[i]='Missing';
else if NUM[i] = . then CHAR[i]=.;
end;
drop i;
run;

/*(c)*/
DATA q2_c;
retain Student Y1-Y6 sum Q1-Q6;
set q2_b;
sum=sum(Y1,Y2,Y3,Y4,Y5,Y6);
run;

/*(d)*/
proc format;
value Likertfmt 1='Strongly Disagree'
2='Disagree'
3='Neutral'
4='Agree'
5='Strongly Agree'
6='Missing'
;
run;

/*frequency table*/
proc freq data=q2_c;
title 'Y1 frequency table';
Table Y1;
format Y1 Likertfmt.;
run;




/*question 3*/

/*I created 5 data and tried each one*/
/*the last one works but does not iterate till 100*/


*First one. not working;
%macro modifyscore;
%do i=1 %to 100;
data DATA&i;
set DATA&i;
%if &i lt 50 %then %let score=%eval(&score*10);
%else %if &i ge 51 %then %let score=%eval(&score/10);
%end;
%mend modifyscore;

%modifyscore;

*no error but only get the last value;
%macro modifyscore;
%do i=1 %to 5;
data DATA&i;
set DATA&i;
%if (&i lt 2) %then %do;
call symputx("anotherscore&i",SCORE&i*10);
SCORE=symget("antoherscore");
*%put &anotherscore;
%end;
%else %do;
call symputx("anotherscore&i",SCORE&i/10);
SCORE=symget("antoherscore");
*%put &anotherscore;
%end;
%end;
%mend modifyscore;

%modifyscore;

%put &anotherscore.;



/*works*/
/*kinda working, but when I tried it with 5 data sets, it only iterated till 4*/
/*So I guess it only iterates till 99 not 100*/
%macro test;
%do i=1 %to 100;
Data DATA&i;
set DATA&i;
%if (&i le 50) %then %do;
SCORE = SCORE*10;
%end;
%else %if (&i ge 51 AND &i le 100) %then %do;
SCORE=SCORE/10;
%end;
%end;
%mend; 

%test;
