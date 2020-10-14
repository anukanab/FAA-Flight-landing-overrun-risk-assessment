/*Chapter 1 code : Data exploration and cleaning */

PROC IMPORT OUT= WORK.FAA1 DATAFILE= "/home/u49592956/FAA1.xls" 
            DBMS=xls REPLACE;
     SHEET="FAA1"; 
     GETNAMES=YES;
RUN;
PROC CONTENTS data=FAA1;
RUN;

PROC IMPORT OUT= WORK.FAA2 DATAFILE= "/home/u49592956/FAA2.xls" 
            DBMS=xls REPLACE;
     SHEET="FAA2"; 
     GETNAMES=YES;
RUN;
PROC CONTENTS data=FAA2;
RUN;

Data FAA_merged;
SET FAA1 FAA2;
Run;
options missing = ' '; /*deleting all empty rows*/
data FAA;
   set FAA_merged;
   if missing(cats(of _all_)) then delete;
run;
PROC PRINT data=faa;
Run;

/*Summary statistics*/
Title1 "Summary Statistics";
Title2 "Unprocessed merged data";
options nolabel;
PROC Means DATA=FAA N nmiss MIN MAX MEAN MEDIAN STDDEV RANGE;
  VAR duration no_pasg speed_ground speed_air height pitch distance;
Run;
PROC Means DATA=FAA noprint;
  output out=missings (drop=_type_ _FREQ_) nmiss= ;
RUN ; 
proc print data=missings;

proc transpose data=missings 
               out= missings;
run;
proc print data= missings;
run;
Data FAA_missing;
 set missings (rename=(_NAME_=Variable));
 percent_missing=(COL1/950)*100;
 drop COL1;
run;
proc print data=faa_missing;
Title1 "Percent of missing values";
Title2 "Unprocessed merged data";
run; 

/*Check duplicate data*/
Proc sort data=FAA dupout=duplicate nodupkey;
by aircraft no_pasg speed_ground speed_air height pitch distance;
run;

/*Remove duplicate data*/
Proc sort data=FAA out=FAA_unique nodupkey;
by aircraft no_pasg speed_ground speed_air height pitch distance; /* Since there is no duration column in FAA2, I am ignoring that column here*/
run;

/*Check for summary statistics again*/
Title1 "Summary Statistics";
Title2 "Merged data : Unique records";
options nolabel;
PROC Means DATA=FAA_unique N nmiss MIN MAX MEAN MEDIAN STDDEV RANGE;
  VAR duration no_pasg speed_ground speed_air height pitch distance;
RUN ; 
PROC Means DATA=FAA_unique noprint;
  output out=missings_unique (drop=_type_ _FREQ_) nmiss= ;
RUN ; 
proc print data=missings_unique;

proc transpose data=missings_unique 
               out= missings_unique;
run;
proc print data= missings_unique;
run;
Data FAA_missing_unique;
 set missings_unique (rename=(_NAME_=Variable));
 percent_missing=(COL1/850)*100;
 drop COL1;
run;
proc print data=faa_missing_unique;
Title1 "Percent of missing values";
Title2 "Merged data : Unique records";
run; 

PROC univariate data=faa_unique;
Run;   /*Check for distribution and abnormal values*/

/*percent of abnormal values*/
Data FAA_unique_clean;
Set FAA_unique;
IF duration < 40 and duration ^=.  Then duration_abnormal = 1; 
IF duration >= 40 Then duration_abnormal = 0;
IF height < 6 and height ^=.  THEN height_abnormal = 1;
ELSE height_abnormal = 0;
IF 0 < speed_ground < 30 or speed_ground > 140 Then speed_ground_abnormal = 1;
ELSE speed_ground_abnormal = 0;
IF 0 < speed_air < 30 or speed_air > 140 then speed_air_abnormal = 1;
ELSE speed_air_abnormal = 0;
IF distance > 6000 then OverRun = 1;
ELSE OverRun = 0;
Run;
PROC print data=FAA_unique_clean;
Run;
proc summary data= faa_unique_clean;
var duration_abnormal speed_ground_abnormal speed_air_abnormal height_abnormal OverRun;
output out=total (drop=_type_ _FREQ_) sum=;
run;
proc transpose data = total 
               out = total;
run;
proc print data = total;
run;
Data total_abPCN;
 set total;
 rename _NAME_=Variable; 
 percent_abnormal = (COL1/850)*100;
 drop COL1;
run;
proc print data = total_abPCN;
Title "Percentage of abnormal values";
run;

/*Delete abnormal observations*/
Data FAA_unique_clean;
Set FAA_unique;
IF duration < 40 and duration ^=.  then delete; 
IF height < 6 and height ^=.  then delete;
IF speed_ground < 30 and speed_ground ^=.  then delete;
IF speed_ground > 140 then delete;
IF speed_air < 30 and speed_air ^=.  then delete;
IF speed_air > 140 then delete;
IF distance > 6000 then delete;
Run;
PROC print data = FAA_unique_clean;
Run;
proc summary data = faa_unique_clean;
var duration no_pasg speed_ground speed_air height pitch distance;
run;

Title1 "Summary Statistics";
Title2 "Clean data_Final";
options nolabel;
PROC Means DATA = FAA_unique_clean N nmiss MIN MAX MEAN MEDIAN STDDEV RANGE;
  VAR duration no_pasg speed_ground speed_air height pitch distance;
RUN ; 
PROC Means DATA=FAA_unique_clean noprint;
  output out=missings_unique_clean (drop=_type_ _FREQ_) nmiss= ;
RUN ; 
proc print data=missings_unique_clean;

proc transpose data=missings_unique_clean 
               out= missings_unique_clean;
run;
proc print data= missings_unique_clean;
run;
Data FAA_missing_unique_clean;
 set missings_unique_clean (rename=(_NAME_=Variable));
 percent_missing=(COL1/831)*100;
 drop COL1;
run;
proc print data=faa_missing_unique;
Title1 "Percent of missing values : data_Final";
run; 

/*Plot each variable to check distribution*/
PROC UNIVARIATE DATA = FAA_unique_clean;
VAR _numeric_;
options nolabel;
HISTOGRAM _numeric_ / NORMAL; /* HISTOGRAM variables / <OPTIONS>*/
PROBPLOT _numeric_ / NORMAL; /* PROBPLOT variables / <OPTIONS>*/
Run;

PROC MEANS data = faa_unique_clean T PRT ;
VAR speed_air distance; /*normality check*/
Run;

/*Check for differences between aircraft models*/
Title1 "Summary Statistics";
Title2 "Clean data_Final";
options nolabel;
PROC Means DATA = FAA_unique_clean N nmiss MIN MAX MEAN MEDIAN STDDEV RANGE;
  VAR duration no_pasg speed_ground speed_air height pitch distance;
  By aircraft;
RUN ;

/*Chapter 2 code : Descriptive Analysis */

/*X-Y plots for the independent variables to distance*/

proc sgplot data = FAA_unique_clean;
 title duration of flight in relation to landing distance;
 scatter x=duration y=distance / group=aircraft;
run;
proc sgplot data = FAA_unique_clean;
 title number of passengers in relation to landing distance;
 scatter x=no_pasg y=distance / group=aircraft;
run;
proc sgplot data = FAA_unique_clean;
 title speed_ground of flight in relation to landing distance;
 scatter x=speed_ground y=distance / group=aircraft;
run;
proc sgplot data = FAA_unique_clean;
 title speed_air of flight in relation to landing distance;
 scatter x=speed_air y=distance / group=aircraft;
run;
proc sgplot data = FAA_unique_clean;
 title height of landing in relation to landing distance;
 scatter x=height y=distance / group=aircraft;
run;
proc sgplot data = FAA_unique_clean;
 title pitch of landing in relation to landing distance;
 scatter x=pitch y=distance / group=aircraft;
run;

proc sgplot data = FAA_unique_clean;
 title speed_ground of flight in relation to speed_air;
 scatter x=speed_ground y=speed_air / group=aircraft;
run;

/*Correlation analysis on FAA_unique_clean data*/
proc corr data = faa_unique_clean;
var duration no_pasg speed_ground speed_air height pitch distance;
title Pairwise correlation coefficients for all variables;
run;

proc plot data=FAA_unique_clean;
plot distance*speed_air='$' distance*height='*' distance*pitch='&' / overlay;
run;

proc corr data = faa_unique_clean;
by aircraft;
var duration no_pasg speed_ground speed_air height pitch distance;
title Pairwise correlation coefficients for all variables;
run;

/*ttest by aircraft*/

PROC TTEST DATA=faa_unique_clean;
CLASS aircraft;
VAR distance;
TITLE TTEST FOR COMPARING THE MEANS OF landing distance by aircraft make;
RUN;

/*Chapter 3 code : Statistical modeling */

/*create new table with binary values for aircraft*/
data FAA_final;
set faa_unique_clean;
IF aircraft = 'airbus' then aircraft_mk = 0;
ELSE aircraft_mk = 1;
Run;
PROC print data=FAA_final;
Run;

/*regression model*/

proc reg data = faa_final;
model distance = aircraft_mk speed_air height pitch / r ;
title Regression analysis of distance to risk factors;
output out=diagnostics r=residual; /*model checking*/
run;

proc reg data = faa_final;
model distance = aircraft_mk speed_air height / vif ;
title Regression analysis of distance to risk factors;
output out=diagnostics r=residual; /*model checking*/
run;
proc ttest data = diagnostics;
run;

