/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Thursday, January 08, 2015     TIME: 3:24:15 PM
PROJECT: ChabonS_SAS_assignment4_18_sas
PROJECT PATH: P:\QAC\qac200\students\schabon\Assignments\ChabonS_SAS_assignment4_18_sas.egp
---------------------------------------- */


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (MYDATA)   */
%LET _CLIENTTASKLABEL='Assign Project Library (MYDATA)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\schabon\Assignments\ChabonS_SAS_assignment4_18_sas.egp';
%LET _CLIENTPROJECTNAME='ChabonS_SAS_assignment4_18_sas.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA  "P:\QAC\qac200\students\schabon" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: limitedmeps   */
%LET _CLIENTTASKLABEL='limitedmeps';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\schabon\Assignments\ChabonS_SAS_assignment4_18_sas.egp';
%LET _CLIENTPROJECTNAME='ChabonS_SAS_assignment4_18_sas.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 3:24:01 PM
   By task: limitedmeps

   Input Data: P:\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsFormeps_fullyr_2012);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsFormeps_fullyr_2012(LABEL="Contents Details for meps_fullyr_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsFormeps_fullyr_2012
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsFormeps_fullyr_2012 OUT=WORK.CONTContentsFormeps_fullyr_2012;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsFormeps_fullyr_2012
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Assignment2   */
%LET _CLIENTTASKLABEL='Assignment2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\schabon\Assignments\ChabonS_SAS_assignment4_18_sas.egp';
%LET _CLIENTPROJECTNAME='ChabonS_SAS_assignment4_18_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.FILTER_FOR_MEPS_FULLYR_2012_0001);

PROC SQL;
   CREATE TABLE WORK.FILTER_FOR_MEPS_FULLYR_2012_0001 AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.REGION12, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRETIRE, 
          t1.CANCERDX, 
          t1.FAMINC12, 
          t1.TTLP12X, 
          t1.TOTEXP12, 
          t1.TOTTCH12, 
          t1.ERTEXP12, 
          t1.RXSLF12, 
          t1.MCARE12, 
          t1.MCAID12, 
          t1.MNHLTH31, 
          t1.MNHLTH42, 
          t1.MNHLTH53, 
          t1.PRVEV12, 
          t1.RXTOT12, 
          t1.BMINDX53, 
          t1.PCS42, 
          t1.MCS42, 
          t1.ADPRX42, 
          t1.SFFLAG42, 
          t1.ADRISK42, 
          t1.ADLANG42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADINSB42, 
          t1.ADGENH42, 
          t1.ADINSA42, 
          t1.ADCMPY42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADSMOK42, 
          t1.ADOVER42, 
          t1.ADPWLM42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADMALS42, 
          t1.ADSOCA42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADCAPE42, 
          t1.K6SUM42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.ADSAD42, 
          t1.ADREST42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADDRBP42, 
          t1.PHQ242, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.ADNDCR42, 
          t1.ADILCR42, 
          t1.ADSPRF42, 
          t1.ADEGMC42, 
          t1.ADNSMK42, 
          t1.ADHECR42, 
          t1.ADFHLP42, 
          t1.ADSPEC42, 
          t1.ADRTCR42, 
          t1.ADFFRM42, 
          t1.ADRTWW42, 
          t1.ADILWW42, 
          t1.ADPRTM42, 
          t1.ADRESP42, 
          t1.ADEZUN42, 
          t1.ADINST42, 
          t1.ADTLHW42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADAPPT42
      FROM EC100005.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Code For Assignment2complete   */
%LET SYSLAST=WORK.FILTER_FOR_MEPS_FULLYR_2012_0001;
%LET _CLIENTTASKLABEL='Code For Assignment2complete';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\schabon\Assignments\ChabonS_SAS_assignment4_18_sas.egp';
%LET _CLIENTPROJECTNAME='ChabonS_SAS_assignment4_18_sas.egp';
%LET _SASPROGRAMFILE=;

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 11:04:25 AM
   By task: Assignment2complete

   Input Data: Local:WORK.FILTER_FOR_MEPS_FULLYR_2012_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForFILTER_FOR_MEPS_F);
TITLE "data set attributes for limited data set";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";


PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=WORK.FILTER_FOR_MEPS_FULLYR_2012_0001;

RUN;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One-Way Frequencies limited meps 1/8   */
%LET _CLIENTTASKLABEL='One-Way Frequencies limited meps 1/8';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\schabon\Assignments\ChabonS_SAS_assignment4_18_sas.egp';
%LET _CLIENTPROJECTNAME='ChabonS_SAS_assignment4_18_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 3:24:02 PM
   By task: One-Way Frequencies limited meps 1/8

   Input Data: Local:WORK.FILTER_FOR_MEPS_FULLYR_2012_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.FILTER_FOR_MEPS_FULLYR_2012_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.AGE12X, T.SEX, T.RACETHX, T.REGION12, T.CANCERDX, T.FAMINC12, T.MARRY12X, T.TTLP12X, T.TOTTCH12, T.RXSLF12, T.MCARE12, T.MCAID12, T.MNHLTH31, T.MNHLTH42, T.MNHLTH53, T.PRVEV12, T.RXTOT12, T.BMINDX53, T.PCS42, T.MCS42, T.ADPRX42
		     , T.SFFLAG42, T.ADRISK42, T.ADLANG42, T.ADDAYA42, T.ADCLIM42, T.ADINSB42, T.ADGENH42, T.ADINSA42, T.ADCMPY42, T.ADCMPM42, T.ADCMPD42, T.ADSMOK42, T.ADOVER42, T.ADPWLM42, T.ADMWLM42, T.ADPAIN42, T.ADPALS42, T.ADMALS42
		     , T.ADSOCA42, T.ADNRGY42, T.ADDOWN42, T.ADCAPE42, T.K6SUM42, T.ADEFRT42, T.ADWRTH42, T.ADSAD42, T.ADREST42, T.ADNERV42, T.ADHOPE42, T.ADDRBP42, T.PHQ242, T.ADINTR42, T.ADDPRS42, T.ADNDCR42, T.ADILCR42, T.ADSPRF42, T.ADEGMC42
		     , T.ADNSMK42, T.ADHECR42, T.ADFHLP42, T.ADSPEC42, T.ADRTCR42, T.ADFFRM42, T.ADRTWW42, T.ADILWW42, T.ADPRTM42, T.ADRESP42, T.ADEZUN42, T.ADINST42, T.ADTLHW42, T.ADLIST42, T.ADEXPL42, T.ADAPPT42, T.TOTEXP12, T.ERTEXP12, T.EVRETIRE
		     , T.EDUYRDEG
	FROM WORK.FILTER_FOR_MEPS_FULLYR_2012_0001(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One Way frequencies Limited Meps data 1/8/15";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) Sophie  Chabon";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES AGE12X / MISSPRINT  SCORES=TABLE;
	TABLES SEX / MISSPRINT  SCORES=TABLE;
	TABLES RACETHX / MISSPRINT  SCORES=TABLE;
	TABLES REGION12 / MISSPRINT  SCORES=TABLE;
	TABLES CANCERDX / MISSPRINT  SCORES=TABLE;
	TABLES FAMINC12 / MISSPRINT  SCORES=TABLE;
	TABLES MARRY12X / MISSPRINT  SCORES=TABLE;
	TABLES TTLP12X / MISSPRINT  SCORES=TABLE;
	TABLES TOTTCH12 / MISSPRINT  SCORES=TABLE;
	TABLES RXSLF12 / MISSPRINT  SCORES=TABLE;
	TABLES MCARE12 / MISSPRINT  SCORES=TABLE;
	TABLES MCAID12 / MISSPRINT  SCORES=TABLE;
	TABLES MNHLTH31 / MISSPRINT  SCORES=TABLE;
	TABLES MNHLTH42 / MISSPRINT  SCORES=TABLE;
	TABLES MNHLTH53 / MISSPRINT  SCORES=TABLE;
	TABLES PRVEV12 / MISSPRINT  SCORES=TABLE;
	TABLES RXTOT12 / MISSPRINT  SCORES=TABLE;
	TABLES BMINDX53 / MISSPRINT  SCORES=TABLE;
	TABLES PCS42 / MISSPRINT  SCORES=TABLE;
	TABLES MCS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRX42 / MISSPRINT  SCORES=TABLE;
	TABLES SFFLAG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRISK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLANG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDAYA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCLIM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSB42 / MISSPRINT  SCORES=TABLE;
	TABLES ADGENH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSMOK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADOVER42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPAIN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSOCA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNRGY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDOWN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCAPE42 / MISSPRINT  SCORES=TABLE;
	TABLES K6SUM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEFRT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADWRTH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSAD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADREST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNERV42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHOPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDRBP42 / MISSPRINT  SCORES=TABLE;
	TABLES PHQ242 / MISSPRINT  SCORES=TABLE;
	TABLES ADINTR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDPRS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNDCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPRF42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEGMC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNSMK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHECR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFHLP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPEC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFFRM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRTM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRESP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEZUN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADTLHW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLIST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEXPL42 / MISSPRINT  SCORES=TABLE;
	TABLES ADAPPT42 / MISSPRINT  SCORES=TABLE;
	TABLES TOTEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES ERTEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES EVRETIRE / MISSPRINT  SCORES=TABLE;
	TABLES EDUYRDEG / MISSPRINT  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: recoded variables    */
%LET _CLIENTTASKLABEL='recoded variables ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\schabon\Assignments\ChabonS_SAS_assignment4_18_sas.egp';
%LET _CLIENTPROJECTNAME='ChabonS_SAS_assignment4_18_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_FILTER_FOR_MEPS_F_0001);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_FILTER_FOR_MEPS_F_0001 AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.REGION12, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRETIRE, 
          t1.CANCERDX, 
          t1.FAMINC12, 
          t1.TTLP12X, 
          t1.TOTEXP12, 
          t1.TOTTCH12, 
          t1.ERTEXP12, 
          t1.RXSLF12, 
          t1.MCARE12, 
          t1.MCAID12, 
          t1.MNHLTH31, 
          t1.MNHLTH42, 
          t1.MNHLTH53, 
          t1.PRVEV12, 
          t1.RXTOT12, 
          t1.BMINDX53, 
          t1.PCS42, 
          t1.MCS42, 
          t1.ADPRX42, 
          t1.SFFLAG42, 
          t1.ADRISK42, 
          t1.ADLANG42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADINSB42, 
          t1.ADGENH42, 
          t1.ADINSA42, 
          t1.ADCMPY42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADSMOK42, 
          t1.ADOVER42, 
          t1.ADPWLM42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADMALS42, 
          t1.ADSOCA42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADCAPE42, 
          t1.K6SUM42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.ADSAD42, 
          t1.ADREST42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADDRBP42, 
          t1.PHQ242, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.ADNDCR42, 
          t1.ADILCR42, 
          t1.ADSPRF42, 
          t1.ADEGMC42, 
          t1.ADNSMK42, 
          t1.ADHECR42, 
          t1.ADFHLP42, 
          t1.ADSPEC42, 
          t1.ADRTCR42, 
          t1.ADFFRM42, 
          t1.ADRTWW42, 
          t1.ADILWW42, 
          t1.ADPRTM42, 
          t1.ADRESP42, 
          t1.ADEZUN42, 
          t1.ADINST42, 
          t1.ADTLHW42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADAPPT42, 
          /* general_health */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL=" HEALTH IN GENERAL " AS general_health, 
          /* health_limits */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL=" HLTH LIMITS MOD ACTIVITIES" AS health_limits, 
          /* climbing_stairs */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL=" HLTH_LIMITS_CLIMBING_STAIRS1" AS climbing_stairs, 
          /* accomplishes_less_phys_probs */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="SAQ 4WKS:ACCMP LESS B/C PHY PRBS" AS accomplishes_less_phys_probs, 
          /* work_limit_phys_prob */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL="4WKS:WORK LIMT B/C PHY PROBS" AS work_limit_phys_prob, 
          /* accomplish_less_mental_prob */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="ACCMP LESS B/C MNT PRBS" AS accomplish_less_mental_prob, 
          /* work_limit_mental_probs */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL=" 4WKS:WORK LIMT B/C MNT PROBS" AS work_limit_mental_probs, 
          /* pain_limits_work */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="4WKS:PAIN LIMITS NORMAL WORK" AS pain_limits_work, 
          /* felt_calm_peace */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="4WKS: FELT CALM/PEACEFUL" AS felt_calm_peace, 
          /* had_energy */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL=" 4WKS: HAD A LOT OF ENERGY" AS had_energy, 
          /* downhearted */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="4WKS: FELT DOWNHEARTED/DEPR " AS downhearted, 
          /* health_stopped_soc */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="4WKS: HLTH STOPPED SOC ACTIV" AS health_stopped_soc
      FROM WORK.FILTER_FOR_MEPS_FULLYR_2012_0001 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: one way frequencies Final Meps Categories   */
%LET _CLIENTTASKLABEL='one way frequencies Final Meps Categories';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\schabon\Assignments\ChabonS_SAS_assignment4_18_sas.egp';
%LET _CLIENTPROJECTNAME='ChabonS_SAS_assignment4_18_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 3:24:02 PM
   By task: one way frequencies Final Meps Categories

   Input Data: Local:WORK.FILTER_FOR_MEPS_FULLYR_2012_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.FILTER_FOR_MEPS_FULLYR_2012_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.SEX, T.AGE12X, T.RACETHX, T.REGION12, T.MARRY12X, T.CANCERDX, T.MCARE12, T.MCAID12, T.EDUYRDEG, T.EVRETIRE, T.TTLP12X, T.TOTEXP12, T.PRVEV12, T.ADHOPE42, T.BMINDX53, T.ERTEXP12, T.ADGENH42, T.ADILCR42, T.ADRISK42, T.ADSMOK42
		     , T.ADLANG42, T.ADNERV42, T.ADTLHW42, T.ADHECR42
	FROM WORK.FILTER_FOR_MEPS_FULLYR_2012_0001 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES SEX /  SCORES=TABLE;
	TABLES AGE12X /  SCORES=TABLE;
	TABLES RACETHX /  SCORES=TABLE;
	TABLES REGION12 /  SCORES=TABLE;
	TABLES MARRY12X /  SCORES=TABLE;
	TABLES CANCERDX /  SCORES=TABLE;
	TABLES MCARE12 /  SCORES=TABLE;
	TABLES MCAID12 /  SCORES=TABLE;
	TABLES EDUYRDEG /  SCORES=TABLE;
	TABLES EVRETIRE /  SCORES=TABLE;
	TABLES TTLP12X /  SCORES=TABLE;
	TABLES TOTEXP12 /  SCORES=TABLE;
	TABLES PRVEV12 /  SCORES=TABLE;
	TABLES ADHOPE42 /  SCORES=TABLE;
	TABLES BMINDX53 /  SCORES=TABLE;
	TABLES ERTEXP12 /  SCORES=TABLE;
	TABLES ADGENH42 /  SCORES=TABLE;
	TABLES ADILCR42 /  SCORES=TABLE;
	TABLES ADRISK42 /  SCORES=TABLE;
	TABLES ADSMOK42 /  SCORES=TABLE;
	TABLES ADLANG42 /  SCORES=TABLE;
	TABLES ADNERV42 /  SCORES=TABLE;
	TABLES ADTLHW42 /  SCORES=TABLE;
	TABLES ADHECR42 /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder   */
%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\schabon\Assignments\ChabonS_SAS_assignment4_18_sas.egp';
%LET _CLIENTPROJECTNAME='ChabonS_SAS_assignment4_18_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_FILTER_FOR_MEPS_F_0002);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_FILTER_FOR_MEPS_F_0002 AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.REGION12, 
          t1.MARRY12X, 
          t1.EDUYRDEG, 
          t1.EVRETIRE, 
          t1.CANCERDX, 
          t1.FAMINC12, 
          t1.TTLP12X, 
          t1.TOTEXP12, 
          t1.TOTTCH12, 
          t1.ERTEXP12, 
          t1.RXSLF12, 
          t1.MCARE12, 
          t1.MCAID12, 
          t1.MNHLTH31, 
          t1.MNHLTH42, 
          t1.MNHLTH53, 
          t1.PRVEV12, 
          t1.RXTOT12, 
          t1.BMINDX53, 
          t1.PCS42, 
          t1.MCS42, 
          t1.ADPRX42, 
          t1.SFFLAG42, 
          t1.ADRISK42, 
          t1.ADLANG42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADINSB42, 
          t1.ADGENH42, 
          t1.ADINSA42, 
          t1.ADCMPY42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADSMOK42, 
          t1.ADOVER42, 
          t1.ADPWLM42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADMALS42, 
          t1.ADSOCA42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADCAPE42, 
          t1.K6SUM42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.ADSAD42, 
          t1.ADREST42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADDRBP42, 
          t1.PHQ242, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.ADNDCR42, 
          t1.ADILCR42, 
          t1.ADSPRF42, 
          t1.ADEGMC42, 
          t1.ADNSMK42, 
          t1.ADHECR42, 
          t1.ADFHLP42, 
          t1.ADSPEC42, 
          t1.ADRTCR42, 
          t1.ADFFRM42, 
          t1.ADRTWW42, 
          t1.ADILWW42, 
          t1.ADPRTM42, 
          t1.ADRESP42, 
          t1.ADEZUN42, 
          t1.ADINST42, 
          t1.ADTLHW42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADAPPT42, 
          /* marrital_status */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
            END) LABEL="MARRY12X" AS marrital_status, 
          /* education_level */
            (CASE 
               WHEN -1 = t1.EDUYRDEG THEN .
               WHEN -7 = t1.EDUYRDEG THEN .
               WHEN -8 = t1.EDUYRDEG THEN .
               WHEN -9 = t1.EDUYRDEG THEN .
               ELSE t1.EDUYRDEG
            END) LABEL="EDUYRDEG " AS education_level, 
          /* retirement_status */
            (CASE 
               WHEN -1 = t1.EVRETIRE THEN .
               WHEN -7 = t1.EVRETIRE THEN .
               WHEN -8 = t1.EVRETIRE THEN .
               WHEN -9 = t1.EVRETIRE THEN .
               ELSE t1.EVRETIRE
            END) LABEL="EVRETIRE" AS retirement_status, 
          /* cancer_diagnosis */
            (CASE 
               WHEN -7 = t1.CANCERDX THEN .
               WHEN -8 = t1.CANCERDX THEN .
               WHEN -9 = t1.CANCERDX THEN .
               ELSE t1.CANCERDX
            END) LABEL="CANCERDX" AS cancer_diagnosis
      FROM WORK.FILTER_FOR_MEPS_FULLYR_2012_0001 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder1   */
%LET _CLIENTTASKLABEL='Query Builder1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\schabon\Assignments\ChabonS_SAS_assignment4_18_sas.egp';
%LET _CLIENTPROJECTNAME='ChabonS_SAS_assignment4_18_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_FILTER_FOR_MEPS_F_0002);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_FILTER_FOR_MEPS_F_0002 AS 
   SELECT /* family_income */
            (CASE 
               WHEN -10794 = t1.FAMINC12 THEN .
               WHEN -10794 = t1.FAMINC12 THEN .
               WHEN -15612 = t1.FAMINC12 THEN .
               WHEN -15612 = t1.FAMINC12 THEN .
               WHEN -19924 = t1.FAMINC12 THEN .
               WHEN -19924 = t1.FAMINC12 THEN .
               WHEN -3772 = t1.FAMINC12 THEN .
               WHEN -3772 = t1.FAMINC12 THEN .
               WHEN -4246 = t1.FAMINC12 THEN .
               WHEN -4246 = t1.FAMINC12 THEN .
               WHEN -9063 = t1.FAMINC12 THEN .
               WHEN -9063 = t1.FAMINC12 THEN .
               ELSE t1.FAMINC12
            END) LABEL="FAMINC12" AS family_income, 
          /* ind_income */
            (CASE 
               WHEN -1000 = t1.TTLP12X THEN .
               WHEN -11459 = t1.TTLP12X THEN .
               WHEN -1200 = t1.TTLP12X THEN .
               WHEN -12002 = t1.TTLP12X THEN .
               WHEN -1289 = t1.TTLP12X THEN .
               WHEN -1500 = t1.TTLP12X THEN .
               WHEN -15982 = t1.TTLP12X THEN .
               WHEN -18132 = t1.TTLP12X THEN .
               WHEN -18938 = t1.TTLP12X THEN .
               WHEN -22919 = t1.TTLP12X THEN .
               WHEN -26861 = t1.TTLP12X THEN .
               WHEN -2890 = t1.TTLP12X THEN .
               WHEN -3772 = t1.TTLP12X THEN .
               WHEN -38074 = t1.TTLP12X THEN .
               WHEN -4246 = t1.TTLP12X THEN .
               WHEN -600 = t1.TTLP12X THEN .
               WHEN -6981 = t1.TTLP12X THEN .
               WHEN -7283 = t1.TTLP12X THEN .
               WHEN -8051 = t1.TTLP12X THEN .
               WHEN -9063 = t1.TTLP12X THEN .
               WHEN -9903 = t1.TTLP12X THEN .
               ELSE t1.TTLP12X
            END) LABEL="PERSONS TOTAL INCOME" AS ind_income, 
          /* mental_health */
            (CASE 
               WHEN -1 = t1.MNHLTH31 THEN .
               WHEN -7 = t1.MNHLTH31 THEN .
               WHEN -8 = t1.MNHLTH31 THEN .
               WHEN -9 = t1.MNHLTH31 THEN .
            END) LABEL="PERCIEVED MENTAL HEALTH 3/1" AS mental_health
      FROM WORK.FILTER_FOR_MEPS_FULLYR_2012_0001 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
