/*question 4  (a)*/
proc import datafile="Downloads/Data_ExamData.xls" out=Survey dbms = xls replace;
Getnames = yes;
run;

*(b);
/*NUM array for Y1-Y6. Char array for new variables Q1-Q6*/
Data Q4_b;
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
/*Use retain to change column variable order*/
DATA Q4_c;
retain Student Y1 Y2 Y3 Y4 Y5 Y6 Y1toY6sum Q1 Q2 Q3 Q4 Q5 Q6;
set Q4_b;
Y1toY6sum=sum(Y1, Y2, Y3, Y4, Y5, Y6);
run;

/*(d)*/
/* create format*/
proc format;
value Likertfmt 1='Strongly Disagree'
2='Disagree'
3='Neutral'
4='Agree'
5='Strongly Agree'
6='Missing'
;
run;

/*create frequency table for Y1*/
proc freq DATA = Survey;
title 'Y1 frequency table';
Tables Y1;
format Y1 Likertfmt.;
run;
