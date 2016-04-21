/* ********************************************************************** */
/*                                                                        */
/*      Program:        ts2oh.c                                           */
/*      Description:    The ts2oh program version ob5-r26.0 reads a       */
/*                      PRDUTIL tsdata report and writes OH DATACARD      */
/*                      time series.        				  */
/*                      The program can also read prior-date OH DATACARD  */
/*                      time series, merge the data with a PRDUTIL        */
/*                      report, and write updated OH DATACARD time        */
/*                      series.                                           */
/*                      This program uses few pointers and is written     */
/*                      to be Fortranish.                                 */
/*                                                                        */
/*                      It includes 2 date functions, rjulmdy_1900 and    */
/*                      rmdyjul_1900 (which call lower level routines     */
/*                      ddgcj_c.c, ddgdj2_c.c, ddgjc_c.c, ddgjd2_c.c).    */
/*                                                                        */
/*      Modified by:    Loubna Bousaidi on 04/22/04                       */
/*      Modified by:    Dave St. on 2005-09-15                            */
/*      Modified by:    Xiaobiao Fan for DR19007 on 05/14/2007            */
/*                      TS2OH aborts during run when REGULAR DATA is NONE */
/*                                                                        */
/* ********************************************************************** */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <dirent.h>
#include <sys/stat.h>

extern int rjulmdy_1900(long, short *);
extern int rmdyjul_1900(short *, long *);


FILE *prdutilinput=NULL;
FILE *olddatacardinput=NULL;
FILE *output=NULL;
FILE *filelistinput=NULL;

DIR *dirp;
char type[5] = "";
char units[5] = "";
char format[80];
char fortformat[5];
char cfortformat[8];
char string[200] = ""; /*buffer where the prdutil report is parsed*/
char dcstring[200] = ""; /*buffer where the old datacards are parsed*/
char fileliststring[200] = ""; /*buffer where the list of old datacards to open is parsed*/
char lastheaderstring[200] = "";
char station[13] = "x";
char last_station[13] = "x";
char fromfile[14] = "x";
char outfilename[200] = "";
char olddatafilename[200] = "";
char filelistname[128] = "oldcards.dat";
char bmonth[3] = "" ;
char emonth[3] = "";
char eday[3] = "";
char bday[3] = "";
char byear[5] = "";
char eyear[5] = "";
char name[25] = "";
char decade[3] = "";
char cmonth[3] = "";
char dimension[4] = "";
char timestep[3] ="";
char valbuffer[10] = "";
char cardday[3] = "";
char cyear[5] = "";
char cday[3] = "";
char movestring[50] = "";
char commandstring[80] = "";
char *p1;
char *p2;
char *p3;
float prdvalues[500000];
float dcvalues[500000];
long prdvaljdate[500000];
short prdhour[500000];
short dchour[500000];
long dcvaljdate[500000];
long max = 500000;
int itimestep;
int icardday = 0;
long numofprds = 0;
long numofdcs = 0;
int iout,iprd,iold,iorf = 0;
long i;
int ii,iii,iiii,precision;
long irec = 0;
short mdy[3];
short savemdy[3];
short iday,stationlen,isixhour;
long jdate,iray,jdatestart,jdateend,jdatefirstofmonth,jdatetest;
short si,oldmonth;
int future,ialign,itrans,itranssw;
float fmissing;
char tmp_yy[3], tmp_yyyy[5];
int Y2Kflag=0;  /* Y2K fix, jerryG, 10/28/99 */
int last_yy=0, yy=0, yyyy=0, my_yy=0, c;
int begin_yy=0; /* fix 1899 problem with empty station, jerryG, 03/08/00 */
struct dirent *de; /* add for large N datacard read problem, jerryG, 03/17/00 */
char fname[128] = "";
char substring[64] = "";
int len, offset;

int ts2oh_main(int argc,const char ** argv)
{

  printf("ts2oh translator version ob8.1 03/20/07\n");
  if (argc == 1) {
    printf("\n");
    printf("------general information---------------------------------------------------\n");
    printf("\n");
    printf("ts2oh reads a PRDUTIL tsdata report and writes OH DATACARD time series.\n");
    printf("ts2oh can also read prior-date OH DATACARD time series, merge the data with a PRDUTIL report, \nand write updated OH DATACARD time series.\n");
    printf("Note: make sure the datacard files that you want to merge with a PRDUTIL report \nare in the same directory as the ts2oh program.\n");
    printf("There is no limit to the number of stations and data types in the PRDUTIL report.\n");
    printf("Each reported station-type can contain up to 500,000 values.\n");
    printf("---\n");
    printf("The format of the output datacard file name will be:\n");
    printf("STATIONID.DATATYPE.YYYYMMDD.YYYYMMDD.datacard    for example: ADAM4.STG.19960401.19960508.datacard\n");
    printf("The dates will reflect the period of record in the file.  The .datacard extension is necessary.\n");
    printf("When merging, ts2oh first examines the station-types it finds in the PRDUTIL report.\n");
    printf("Then it looks in the current directory for a corresponding datacard file.\n");
    printf("The directory can contain any number of files, but if it contains more than one datacard file of the \nSAME station-type combination, ts2oh will halt with an error message.\n");
    printf("In other words, for any one invocation of ts2oh, the station-types must be unique in the report \nand in the directory of files to be merged. They don't all have to match, however.\n");
    printf("---\n");
    printf("If a station-type is in the PRDUTIL report but does not exist in the directory of to-be merged files,\na new station-type datacard file will be created.\n");
    printf("If a station-type is a datacard file for merge input but no matching station-type is in the PRDUTIL \nreport, a message will be generated and the file will be copied to outdir to be with the other normal output.\n");
    printf("During the merge phase, new time series data values from the PRDUTIL report will replace \ntime-matching old datacard data values EXCEPT WHEN the new PRDUTIL data is a **NONE** value (-999) \nat the beginning of the report.\n");
    printf("---\n");
    printf("ts2oh will produce new datacard files and place them in a subdirectory called outdir.\n");
    printf("Between each cycle, manually review and rotate the datacard files from the outdir directory to the \ncurrent directory.\n");
    printf("---\n");
    printf("To get started, place the ts2oh executable and prdutil reports in an empty directory.\n");
    printf("Use the name of a prdutil tsdata report as a command argument.\n");
    printf("EXAMPLE command sequence:\n");
    printf("ts2oh cycle1.txt         (initial cycle)\n");
    printf("mv ./outdir/*.datacard . (move output so that ts2oh can use it as input)\n");
    printf("ts2oh cycle2.txt         (next cycle merges)\n");
    printf("rm *.datacard            (remove input datacards just used)\n");
    printf("mv ./outdir/*.datacard . (move output so that ts2oh can use it as input)\n");
    printf("ts2oh cycle3.txt         (repetez...)\n");
    printf("rm *.datacard\n");
    printf("mv ./outdir/*.datacard .\n");
    printf("---\n");
    printf("Messages are written to standard output.  Use the > character to direct them to a file if necessary.\n");
    printf("ts2oh places the day of the month in the datacard format column that is normally used for the card \nsequence number.  It is checked on input.\n");
    printf("Future data is ignored.\n");
    printf("Notice that since you provide the name of the PRDUTIL report as a command argument, more than one\nreport can be in the input directory.\n");
    printf("ts2oh reads and writes one-column formatted datacards.\n");
    printf("Long station IDs are truncated to 10 chars.\n");
    printf("Precision will be taken from the PRDUTIL report, not the old datacards, and will end up as F9.something.\n");
    printf("Precision more than 6 decimal places is ignored.\n");
    printf("During a merge, the timestep of the station-type must match.\n");
    printf("ts2oh returns 0 on a normal exit and -1 on an exit taken by an error. Stdout messages always preceed error exits.\n");
    printf("If ts2oh halts on an error, all output files from that instance should be thrown out.\n");
    printf("In this early version, ts2oh determines dimension (L or L3) simply by looking at the first character of\nthe TYPE= in the TSDATA report.  If it's a C, the ts2oh takes it to mean the dimension is cubic.\n");
    printf("Suggestions for changes or enhancements to ts2oh are welcome.\n");
    printf("----------------------------------------------------------------------------\n");
    printf("Y2K Note:  If you use 1900s data and 2000s data in the same file,\n");
    printf("make sure the minimum difference between the two last two digits of the centuries' year is 40 years.\n");
    printf("For example: >=1940 & >=2000 is OK, but <=1939 & >=2000 is not. jerryG, 11/02/99\n");
    return(-1);
  }

  if (NULL==(prdutilinput=fopen(argv[1], "r"))) {
    printf("Error opening prdutil input file %s\n", argv[1]);
    if ( prdutilinput != NULL ) { fclose(prdutilinput); prdutilinput=NULL; }
    return(-1);
  }

  strcpy(format,"%9.1f");
  strcpy(fortformat,"F9.1");
  strcpy(units,"????");
  strcpy(type,"????");
  i=3;

  /* get a list of old datacard files the prdutil tsdata *.ts report is to be merged with */
  /* this will be needed to open them since they are "unknown" */

  /* begin fix for large N datacard read problem, jerryG, 03/17/00 */
  if (NULL==(filelistinput=fopen(filelistname,"w"))) {
    printf("Cannot open infile %s to write", filelistname);
    exit(1);
  }

  dirp = opendir(".");
  if ( dirp != NULL ) {
    while (  ( de=readdir(dirp)) != NULL ) {
      strcpy(fileliststring, de->d_name);
      len = strlen(fileliststring);
      if (len > 26) {
	offset = len-8;
	strncpy(substring, fileliststring+offset, 8);
	if (strcmp(substring, "datacard") == 0)
	  fprintf(filelistinput, "%s\n", fileliststring);
      }
    }
  }
  if ( filelistinput != NULL ) { fclose(filelistinput); filelistinput=NULL; }
  /* end fix for large N datacard read problem, jerryG, 03/17/00 */

  if (system("cd outdir") != 0) system("mkdir outdir");

  while (fgets(string, sizeof(string), prdutilinput))  {
    irec++;

    /* verify the prdutilinput file format */
    if (irec == 1 &&
	strncmp(string+1,"NWSRFS FORECAST SYSTEM - PROGRAM PRDUTIL",40) != 0) {
      printf("The input file does not seem to be output from NWSRFS FORECAST SYSTEM - PROGRAM PRDUTIL\n");
      printf("Halted on Error.\n");
      return(-1);
    }

    /*obtain some information for the header*/
    if (strncmp(string+1,"TIME SERIES ID=",15)==0 && strcmp(string,lastheaderstring) != 0) {

      strcpy(lastheaderstring,string);

      /*go ahead and re-initialize all these vars for each new station-type*/

      jdate=0;
      jdatestart=0;
      jdateend=0;
      jdatefirstofmonth=0;
      iray=0;
      future=0;
      numofprds=0;
      numofdcs=0;
      mdy[3]=0;
      iday=0;
      stationlen=0;
      isixhour=0;
      si=0;
      oldmonth=0;
      future=0;
      ialign=0;
      fmissing=0;
      prdvalues[0]=0; /* array element 0 is checked for significance later on... */
      dcvalues[0]=0;
      dcvaljdate[0]=0;
      prdvaljdate[0]=0;
      prdhour[0]=0;
      dchour[0]=0;

      strcpy(dimension,"L");
      strcpy(byear,"9999");
      strcpy(eyear,"0000");
      strcpy(bmonth,"99");
      strcpy(bday,"99");
      strcpy(emonth,"00");
      strcpy(eday,"00");

      for (i=0;i<=132;i++) {
	if (strncmp(string+i,"UNITS=",6) == 0)
	  strncpy(units,string+i+6,4);
	if (strncmp(string+i,"TYPE=",5) == 0)
	  strncpy(type,string+i+5,4);
	if (strncmp(string+i,"TIME STEP=",10) == 0)
	  strncpy(timestep,string+i+10,2);
          itimestep = atoi(timestep);
	if (strncmp(string+i,"TIME SERIES ID=",15) == 0) {
	  strcpy(last_station, station); /* 4 Y2K, jerryG, 11/02/99 */
	  strncpy(station,string+i+15,10);
	  stationlen=strlen(station)-1;
	  strncpy(station-1+strlen(station),"",1);
	  if (strcmp(last_station, "x") == 0)
	    strcpy(last_station, station); /* init 4 Y2K, jerryG, 11/02/99 */
	}
	if (strncmp(string+i,"DESCRIPTION=",12) == 0) {
	  strncpy(name,string+i+12,20);
	}
	if (strncmp(units,"C",1) == 0) {
	  strcpy(dimension,"L3");
	}
	if (strncmp(string+i,"TIME STEP=",10) == 0) {
	  if (strncmp(string+i+11,"6",1) !=0 && strncmp(string+i+11,"1",1) !=0 &&
	      strncmp(string+i+10,"24",2) !=0 && /* add 24, jerryG 11/03/99 */
	      strncmp(string+i+10,"12",2) !=0) { /* add 12, jerryG 09/01/00 */
	    printf("ts2oh only deals with 1, 6, 12, or 24 hour data. %s %s is neither. Halted. %s\n",
		   station,type,string+i+11);
	    return(-1);
	  }
	}
      }
      for(i=0;i<=strlen(station);i++) {
	if (strncmp(station+i," ",1)==0){
	  strncpy(station+i,"\0",1);
	break;
	}
      }
      for(i=0;i<=strlen(last_station);i++) { /* save last 4 Y2K, jerryG, 11/02/99 */
	if (strncmp(last_station+i," ",1)==0){
	  strncpy(last_station+i,"\0",1);
	break;
	}
      }
      strcpy(outfilename,"outdir/");
      strcat(outfilename,station);
      strcat(outfilename,".");
     for(i=0;i<=strlen(type);i++) {
	if (strncmp(type+i," ",1)==0){
	  strncpy(type+i,"\0",1);
	break;
	}
      }
      strcat(outfilename,type);
    }

    /* now get the dates and values from the tsdata report */

    strcpy(tmp_yy, "");
    strncpy(tmp_yy,string+7,2);
    c = tmp_yy[0];
    Y2Kflag=0; /* must reset Y2Kflag, jerryG, 09/13/00 */
    if (isdigit(c)) {
      yy = atoi(tmp_yy);
      if (yy >= 0 && yy < 50) /* This line and next line added; commented out above section */
	Y2Kflag=1;           /*      MJB/ABRFC - 1/31/2001     */
        last_yy = yy;
    }
    if (Y2Kflag)
      strncpy(cyear,"20",2);
    else
      strncpy(cyear,"19",2);
      strncpy(cyear+2,string+7,2);
    /* end Y2K stuff */

    strncpy(decade,string+7,2);
    strncpy(cmonth,string+1,2);
    strncpy(cday,string+4,2);

    mdy[0]=atoi(cmonth);
    mdy[1]=atoi(cday);
    mdy[2]=atoi(cyear);

    if (strncmp(string+1,"FUTURE",6)==0) future = 1;

    /* if a valid date and 6 hour data*/
    if (itimestep == 6 || itimestep == 12 || itimestep == 24) {
      i=rmdyjul_1900(mdy,&jdate);
      savemdy[0]=mdy[0];
      savemdy[1]=mdy[1];
      savemdy[2]=mdy[2];
    }
    if (itimestep == 1) {
      i=rmdyjul_1900(mdy,&jdate);
      if (i == 0) {
	savemdy[0]=mdy[0];
	savemdy[1]=mdy[1];
	savemdy[2]=mdy[2];
      }
    }
    if (itimestep == 1 && strncmp(string,"      ",6) == 0 && strncmp(string+14,"          ",11) != 0) {
      mdy[0]=savemdy[0];
      mdy[1]=savemdy[1];
      mdy[2]=savemdy[2];
      i=rmdyjul_1900(mdy,&jdate);
    }

    if (i==0 && future == 0) {
      /* get the first year */
      if (strcmp(byear,"9999")==0) strcpy(byear,cyear);
      /* get the first month */
      if (strcmp(bmonth,"99")==0) strcpy(bmonth,cmonth);
      /* get the first day */
      if (strcmp(bday,"99")==0) strcpy(bday,cday);

      /* do some checking to be sure of the expected format of the report */
      if (itimestep != 1 && itimestep != 6 && itimestep != 12 && itimestep != 24) {
                                /* add 24, jerryG, 11/03/99 , add 12, 09/01/00 */
	printf("The time step for station %s is not 1, 6, 12 or 24.  Halted.\n",station);
	return(-1);
      }
      if (itimestep == 1) {
	if (strncmp(string+10,"13Z",3) != 0 && strncmp(string+1,"   ",3) !=0) {
	  printf("1-hour data for station %s is not 13Z aligned.  Halted.\n",station);
	  return(-1);
	}
      }
      if (itimestep == 6) {
	if (strncmp(string+10,"18Z",3) != 0) {
	  printf("6-hour data for station %s is not 18Z aligned.  Halted.\n",station);
	  return(-1);
	}
	if (numofprds == 0 && strncmp(string+50,"/",1) !=0) {
	  printf("ts2oh expects a forward slash mark in column 51 of the first record of values of the PRDUTIL report.\n");
	  printf("ts2oh thinks the values are not in the expected places or the format is otherwise suspect.\n");
	  printf("Record dump is:%s\n  Halted.\n",string);
	  return(-1);
	}
      }
      if (itimestep == 12) { /* add 12, jerryG, 09/01/00 */
	if (strncmp(string+10,"00Z",3) != 0 && strncmp(string+1,"   ",3) !=0) {
	  printf("12-hour data for station %s is not 00Z aligned.  Halted.\n",station);
	  return(-1);
	}
      }
      if (itimestep == 24) { /* add 24, jerryG, 11/03/99 */
	if (strncmp(string+10,"12Z",3) != 0 && strncmp(string+1,"   ",3) !=0) {
	  printf("24-hour data for station %s is not 12Z aligned.  Halted.\n",station);
	  return(-1);
	}
      }
      if (strlen(string) < 132) {
	printf("The PRDUTIL input file contains a short record somewhere in %s %s.\n",station,
	       type);
	printf("The report may have been manually edited.\n");
	printf("ts2oh will only accept PRDUTIL formatted data which is always 132 bytes per record.\n");
	printf("Record dump is:%s\n  Halted.\n",string);
	return(-1);
      }

      /* translate some characters to blanks*/
      for(ii=0;ii<=132;ii++) {
	if (strncmp(string+ii,"/",1)==0) strncpy(string+ii," ",1);
	if (strncmp(string+ii,"*",1)==0) strncpy(string+ii," ",1);
      }

      if (itimestep == 24) ii=12;  /* if timestep = 24, add 12, jerryG, 11/03/99
				      the first value will be for the period 12z to 12z and is referred to as 12z */
      if (itimestep == 12) ii=12;  /* if timestep = 12, add 12, jerryG, 09/06/00
				      the first value will be for the period 12z to 12z and is referred to as 12z */
      if (itimestep == 6) ii=18;  /* if timestep = 6,
				     the first value will be for the period 12z to 18z and is referred to as 18z */
      if (itimestep == 1) ii=13;  /* if timestep = 1,
				     the first value will be for the period 12z to 13z and is referred to as 13z */
      if (itimestep == 1 && strncmp(string,"             ",13) == 0 && strncmp(string+14,"           ",
									       11) != 0) {
	ii=1;
      }

      /* loop through and get the values across this record */
      for(i=14;i<132-9;i=i+9) {
	if (numofprds==0) precision=0;
	if (strncmp(string+i,"         ",9)!=0) {
	  strncpy(valbuffer,string+i,9);

	  /* determine the precision */
	  if (i == 14) {
	    if (strncmp(valbuffer+7,".",1) ==0) {
	      strcpy(cfortformat,"%9.1f\n");
	      strncpy(fortformat,"F9.1",4);
	      precision=1;
	    }
	    if (strncmp(valbuffer+6,".",1) ==0) {
	      strcpy(cfortformat,"%9.2f\n");
	      strncpy(fortformat,"F9.2",4);
	      precision=2;
	    }
	    if (strncmp(valbuffer+5,".",1) ==0) {
	      strcpy(cfortformat,"%9.3f\n");
	      strncpy(fortformat,"F9.3",4);
	      precision=3;
	    }
	    if (strncmp(valbuffer+4,".",1) ==0) {
	      strcpy(cfortformat,"%9.4f\n");
	      strncpy(fortformat,"F9.4",4);
	      precision=4;
	    }
	    if (strncmp(valbuffer+3,".",1) ==0) {
	      strcpy(cfortformat,"%9.5f\n");
	      strncpy(fortformat,"F9.5",4);
	      precision=5;
	    }
	    if (strncmp(valbuffer+2,".",1) ==0) {
	      strcpy(cfortformat,"%9.6f\n");
	      strncpy(fortformat,"F9.6",4);
	      precision=6;
	    }
	    if (precision==0) {
	      strncpy(fortformat,"F9.0",4);
	      strcpy(cfortformat,"%9.0f\n");
	    }
	  }

	  for (iiii=0;iiii<=5;iiii++) {
	    if (strncmp(valbuffer+iiii,"NONE",4)==0) {
	      strncpy(valbuffer," -998    ",9);
	    }
	  }

	  /* figure julian date for 6 hour data */
	  if (itimestep == 24) { /* add 24, jerryG, 11/03/99 */
	    prdvalues[numofprds]=atof(valbuffer);
	    /* prdhour[numofprds]=ii; fix 4 24, jerryG, 02/23/00 */
	    prdhour[numofprds]=24;
	    prdvaljdate[numofprds]=jdate;
	    numofprds++;
	    jdate++; /*begin next day in record*/
	  }
	  if (itimestep == 12) { /* add 12, jerryG, 09/01/00 */
	    prdvalues[numofprds]=atof(valbuffer);
	    ii=12;
	    if (i==14) {
	      ii=0;   /*begin first day in record*/
	    }
	    if (i==32) {
	      jdate++; /*begin second day in record*/
	      ii=0;
	    }
	    if (i==50) {
	      jdate++; /*begin third day in record*/
	      ii=0;
	    }
	    if (i==68) {
	      jdate++; /*begin fourth day in record*/
	      ii=0;
	    }
	    if (i==86) {
	      jdate++; /*begin fifth day in record*/
	      ii=0;
	    }
	    if (i==104) {
	      jdate++; /*begin sixth day in record*/
	      ii=0;
	    }
	    prdhour[numofprds]=ii;
	    prdvaljdate[numofprds]=jdate;
	    numofprds++;
	  }
	  if (itimestep == 6) {
	    prdvalues[numofprds]=atof(valbuffer);
	    if (i==32) {
	      jdate++; /*begin second day in record*/
	    }
	    if (i==68) {
	      jdate++; /*begin third day in record*/
	    }
	    if (i==104) {
	      jdate++; /*begin fourth day in record*/
	    }
	    if (ii > 24) ii=6;
	    prdhour[numofprds]=ii;
	    prdvaljdate[numofprds]=jdate;
	    numofprds++;
	    ii=ii+6;
	  }
	  /* figure julian date for 1 hour data */
	  if (itimestep == 1) {
	    prdvalues[numofprds]=atof(valbuffer);
	    if (ii > 24) ii=1;
	    if (ii == 1) jdate++;
	    prdhour[numofprds]=ii;
	    prdvaljdate[numofprds]=jdate;
	    numofprds++;
	    ii++;
	  }

	  if (numofprds > max) {
	    printf("Values array was topped. Too many years of data for %s. Aborted.\n",
		   station);
	    return(-1);
	  }
	}
      }
    }

    /* if done with this station, write header and values in datacard format */
    if ((strncmp(string+1,"------",6)==0 || strncmp(string+1,"******",6)==0) && strncmp(outfilename,"",
											1) !=
	0) {
      /* ------------------------------------------------------------------------------------------------*/
      /* look for an old file to merge into the data collected from the report*/

      iprd++;

      numofdcs=0;

      if (NULL!=(filelistinput=fopen(filelistname, "r"))) {
				/* look to see if the station.type is unique */
				/* maybe later, put somthing here to do something with more than one*/
	i=0;
	while (fgets(fileliststring, sizeof(fileliststring), filelistinput))  {
	  if (strncmp(station,fileliststring,strlen(station))==0 &&
	      strncmp(type,fileliststring+strlen(station)+1,strlen(type))==0 &&
	      strncmp(fileliststring+strlen(station),".",1)==0 &&
	      strncmp(fileliststring+strlen(station)+strlen(type)+1,".",1)==0) i++;
	  if (i > 1) {
	    printf("There is more than one input datacard file for station %s type %s. Halted.\n",
		   station,type);
	  }
	}
	rewind (filelistinput);
	while (fgets(fileliststring, sizeof(fileliststring), filelistinput))  {
	  /* look for a match with the data processed from the tsdata report */
	  /* this code only looks for one matching file name. It could be modified to open the file
	     and check the tsheader or other internal fields. */
	  if (strncmp(station,fileliststring,strlen(station))==0 &&
	      strncmp(type,fileliststring+strlen(station)+1,strlen(type))==0 &&
	      strncmp(fileliststring+strlen(station),".",1)==0 &&
	      strncmp(fileliststring+strlen(station)+strlen(type)+1,".",1)==0) {
	    /* now load the values into an array */
	    strcpy(olddatafilename,fileliststring);
	    strncpy(olddatafilename+strlen(olddatafilename)-1,"\0",1);
	    if (NULL!=(olddatacardinput=fopen(olddatafilename, "r"))) {
	      printf("This old file was found and opened: %s",fileliststring);
	      iold++;
	      numofdcs=0;
	      irec=0;
	      iday=1;
	      isixhour=0;
	      oldmonth=0;
	      while (fgets(dcstring, sizeof(dcstring), olddatacardinput))  {
		/* code folded from here */
		if (strncmp(dcstring,"$",1) != 0) {
		  /* code folded from here */
		  /* code folded from here */
		  irec++;
		  if (irec > 2) {
		    /* check units for match*/
		    if (irec==1) {
		      if (strncmp(units,dcstring+24,3) != 0)
			printf("Units mismatch, cant merge station %s type %s. Halted.\n",station,type);
		      return(-1);
		    }
		    if (irec==2) {
		      if (strncmp(dcstring+20,"1",1) != 0 || strncmp(dcstring+21,"1",1) != 0) {
			printf("Input datacard file %s must be one column format and aligned on day 1 0Z. Halted.\n",
			       olddatafilename);
			return(-1);
		      }
		    }
		    isixhour++;
		    if (numofdcs > max) {
		      printf("Values array topped. Aborted.\n");
		      return(-1);
		    }

		    strncpy(valbuffer,"          ",10);
		    strncpy(valbuffer,dcstring+20,10);
		    dcvalues[numofdcs]=atof(valbuffer);
		    strncpy(cmonth,dcstring+12,2);
		    mdy[0]=atoi(cmonth);
		    if (mdy[0] != oldmonth) {
		      isixhour=1;
		      iday=1;
		    }
		    if (isixhour > 24/itimestep) {
		      isixhour=1;
		      iday++;
		    }

		    /* check that the series begins on the first day of the month (asisthe format rule)*/
		    if (numofdcs == 0 && strncmp(dcstring+18,"01",2) != 0) {
		      printf("the input datacard file %s does not contain day values in the sequence column\nor the first day is not 01.\n",olddatafilename);
		      return(-1);
		    }

		    /* and has 4 values per day*/
		    strncpy(cardday,dcstring+18,2);
		    icardday=atoi(cardday);
		    if (icardday != iday) {
		      printf("The input datacard file %s has incorrect day sequence numbers somewhere around uncommented record %d.\n",
			     olddatafilename,irec);
		      /* add 24, jerryG, 11/03/99, add 12, 09/01/00 */
		      if (itimestep==24) printf("Perhaps there are more or less than 1 value for a day or a day is not present.\n");
		      if (itimestep==12) printf("Perhaps there are more or less than 2 values for a day or a day is not present.\n");
		      if (itimestep==6) printf("Perhaps there are more or less than 4 values for a day or a day is not present.\n");
		      if (itimestep==1) printf("Perhaps there are more or less than 24 values for a day or a day is not present.\nA particular station/type's datacard and entry in the prdutil tsdata report must share the same timestep.\n");
		      return(-1);
		    }

		    /*continue normal*/
		    dchour[numofdcs]=(isixhour*itimestep);
		    if (itimestep==12) {  /* fix for 12 hour, jerryG, 09/06/00 */
		      if (isixhour%2==0)
			dchour[numofdcs]=18; /* to make iii>24, line 881 */
		      else
			dchour[numofdcs]=12;
		    }

		    if (dchour[numofdcs] == 0) dchour[numofdcs]=24;
		    mdy[1]=iday;
		    strncpy(decade,dcstring+14,2);
		    /* fix 4 Y2K, jerryG, 10/28/99 */
		    strncpy(tmp_yy,decade,2);
		    yy = atoi(tmp_yy);
		    if (yy==0 && last_yy==99) /* look for crossover into Y2K */
		      strncpy(cyear,"20",2);
		    else
		      strncpy(cyear,"19",2);
		    if (yy<40 && last_yy<40)  /* test for both files >= 2000 */
		      strncpy(cyear,"20",2);
		    last_yy = atoi(tmp_yy);

		    strcpy(cyear+2,decade);
		    mdy[2]=atoi(cyear);
		    if (mdy[2] < 1900) {
		      printf("Year problem in %s Year %d. Aborted.\n",olddatafilename,mdy[2]);
		      return(-1);
		    }
		    ii=rmdyjul_1900(mdy,&jdate);
		    if (ii !=0 ) {
		      printf ("Error in rmdyjul_1900. %s. month:%s day:%d year:%s Aborted.\n",olddatafilename,cmonth,iday,cyear);
		      return(-1);
		    }
		    dcvaljdate[numofdcs]=jdate;
		    /*printf("%f %d %d %d\n",dcvalues[numofdcs],iday,mdy[0],jdate);*/
		    oldmonth=mdy[0];
		    numofdcs++;
		  }
		  /* unfolding */
		  /* unfolding */
		}
		/* unfolding */
	      }
              if ( olddatacardinput != NULL ) { fclose(olddatacardinput); olddatacardinput=NULL; }
	    }
	    else {
	      printf("Could not open the old input file %s.\n",olddatafilename);
	      return(-1);
	    }
	  }
	}
      }
      else {
	printf("Could not open a temp file that is a list of stations to be merged.\n");
      }
      if ( filelistinput != NULL ) { fclose(filelistinput); filelistinput=NULL; }

      /* --make the file name, open the file for write, merge the data---------------------------------*/
      /* there is always significant numofprds to get here */
      if (numofdcs!=0) numofdcs=numofdcs-1;
      if (numofprds!=0) numofprds=numofprds-1;  /*align with the end of the arrays*/
      jdateend=0;
      jdatestart=0;
      if (prdvaljdate[numofprds] < dcvaljdate[numofprds] && numofdcs > 0) {
	printf("the input file %s starts on a date later than the corresponding station/type in the prdutil report. Halted.\n",
	       olddatafilename);
	return(-1);
      }

     if (numofprds != 0) {       //cfan    05/14/2007

       /*figure what the last date will be*/
       jdateend=prdvaljdate[numofprds];
       if (dcvaljdate[numofdcs] > jdateend) jdateend=dcvaljdate[numofdcs];
       if (rjulmdy_1900(jdateend,mdy) < 0) {
         printf("Error in rjulmdy_1900 1 trying to make MMDDYYYY.  Program aborted.\n");
         return(-1);
       }

       sprintf(eyear,"%04d",mdy[2]);
       sprintf(emonth,"%02d",mdy[0]);
       sprintf(eday,"%02d",mdy[1]);

       /*figure what the first date will be*/
       jdatestart=prdvaljdate[0];
       if (dcvaljdate[0] < jdatestart && dcvaljdate[0] > 0) jdatestart=dcvaljdate[0];
       if (rjulmdy_1900(jdatestart,mdy) < 0) {
 	 printf("Error in rjulmdy_1900 2 trying to make MMDDYYYY.  Program aborted.\n");
	 return(-1);
       }
       sprintf(byear,"%04d",mdy[2]);
       sprintf(bmonth,"%02d",mdy[0]);
       sprintf(bday,"%02d",mdy[1]);

       strcat(outfilename,".");
       strcat(outfilename,byear);
       strcat(outfilename,bmonth);
       strcat(outfilename,"01");  /* for bday */
       strcat(outfilename,".");
       strcat(outfilename,eyear);
       strcat(outfilename,emonth);
       strcat(outfilename,eday);
       strcat(outfilename,".datacard");
       begin_yy = atoi(byear);
        if (begin_yy >= 1900) { /* fix 1899 problem, jerryG, 03/08/00 */
  	  if (NULL==(output=fopen(outfilename, "w"))) {
	    printf("Error opening file: %s  - Program aborted.\n", outfilename);
            if ( prdutilinput != NULL ) { fclose(prdutilinput); prdutilinput=NULL; }
            if ( output != NULL ) { fclose(output); output=NULL; }
	    return(-1);
	  }
        }

      /* if there are no old datacards, then the data from the prdutil report must be aligned to day one*/
        if (numofdcs==0) {
	  printf("No datacard time series found to merge with %s %s in the report file.\n",
	       station,type);
	  mdy[1]=1;
	  if (rmdyjul_1900(mdy,&jdatefirstofmonth) < 0) {
	    printf ("Error in rmdyjul_1900 at finding first of month.\n");
	    return(-1);
	  }
        }

        fmissing=-999;


        strncpy(fromfile,argv[1],13);
        if (begin_yy >= 1900) { /* fix 1899 problem, jerryG, 03/08/00 */
	  printf("Writing new file to outdir:  %s\n",outfilename);
	  fprintf(output,"$OH datacard format:\n");
	  fprintf(output,"$FromFile     Type Dim  Unit  Stp StationID  StationDesc (header card 1)\n");
	  fprintf(output,"$m  yyyy mm   yyyy  col format (header card 2)\n");
	  fprintf(output,"$StationID  mmyy day datavalue (values n cards)\n");
	  fprintf(output,"%-13s %-4s %-2s   %-4s %-2s   %-12s      %s\n",
		fromfile,type,dimension,units,timestep,station,name);
	  fprintf(output,"%s  %s %s   %s  1   %s\n",bmonth,byear,emonth,eyear,fortformat);

	  iout++;

	/* if there is no old input to cause the data to start at the first of the month, that is,
	   if this data is only from the prdutil report, then pad with missing values from day one of month*/
	  if (numofdcs == 0) {
	    for(i=jdatefirstofmonth;i<=jdatestart;i++) {
	      if (rjulmdy_1900(i,mdy) < 0) {
	        printf("Error in rjulmdy_1900 trying to make MMDDYYYY.  Program aborted.\n");
	        return(-1);
	      }
	    /*account for the hours*/
	      for (ii=itimestep;ii<=24;ii=ii+itimestep) {
	        if (ii==prdhour[0] && jdatestart==i) break;
	        if (itimestep==24 && jdatestart==i) break; /* 24-hour fix, jerryG, 11/03/99 */
	        if (itimestep==12 && jdatestart==i) break; /* 12-hour fix, jerryG, 09/06/00 */
	      /*strcpy(format,"%-12s%02d%2d  %02d");, fix 4 yy<10, jerryG, 10/29/99 */
	        strcpy(format,"%-12s%02d%02d  %02d");
	        strcat(format,cfortformat);
	      /* Y2K fix, jerryG, 10/28/99, mdy[2]-1900-> my_yy */
	        if (mdy[2] < 2000)
	  	  my_yy = mdy[2]-1900;
	        else
	  	  my_yy = mdy[2]-2000;
	        fprintf(output,format, station,mdy[0],my_yy,mdy[1],fmissing);
	      }
	    }
	    for(i=0;i<=numofprds;i++) {
	      if (rjulmdy_1900(prdvaljdate[i],mdy) < 0) {
	        printf("Error in rjulmdy_1900 trying to make MMDDYYYY.  Program aborted.\n");
	        return(-1);
	      }
	    /* Y2K fix, jerryG, 10/28/99, mdy[2]-1900-> my_yy */
	      if (mdy[2] < 2000)
	        my_yy = mdy[2]-1900;
	      else
	        my_yy = mdy[2]-2000;
	      strcpy(format,"%-12s%02d%02d  %02d");
	      strcat(format,cfortformat);
	      if (prdvalues[i] == -998) prdvalues[i] = -999; /* -998 is none*/
	      fprintf(output,format,
		    station,mdy[0],my_yy,mdy[1],prdvalues[i]);
	    }
	  }
        }

      }    //cfan  05/14/2007

      itrans = 0; /*for none transition*/
      itranssw = 0; /*for none transition*/

      /*if merging prd into old input, do the compare and merge the time series,
	giving preference to the newer data from the prdutil report
	for now this assumes that the old datacards will have an earlier starting date*/
      if (numofdcs > 0) {
				/*roll up to the new data*/
	for(i=0;i<=numofdcs;i++) {
	  /*if the new data is **NONE**, continue with the old if possible*/
	  if (dcvaljdate[i] == prdvaljdate[0] && dchour[i] == prdhour[0]) itranssw=1;
	  if (itranssw == 1 && prdvalues[itrans] != -998) break;
	  if (itranssw == 1 && prdvalues[itrans] == -998) itrans++;

	  if (rjulmdy_1900(dcvaljdate[i],mdy) < 0) {
	    printf("Error in rjulmdy_1900 trying to make MMDDYYYY.  Program aborted.\n");
	    return(-1);
	  }
	  /* Y2K fix, jerryG, 10/28/99, mdy[2]-1900-> my_yy */
	  if (mdy[2] < 2000)
	    my_yy = mdy[2]-1900;
	  else
	    my_yy = mdy[2]-2000;
	  strcpy(format,"%-12s%02d%02d  %02d");
	  strcat(format,cfortformat);
	  fprintf(output,format,
		  station,mdy[0],my_yy,mdy[1],dcvalues[i]);
	}

				/* see if there's a gap between when the old ts ends and the prd-new ts starts
				   and fill it if there is */
	if (dcvaljdate[numofdcs] <= prdvaljdate[0]) {
	  /* figure the first hour */
	  iii=dchour[numofdcs]+itimestep;
	  if (iii > 24) iii=itimestep;
	  ialign=dcvaljdate[numofdcs];
	  if (iii==itimestep) ialign=ialign+1;

	  for(i=ialign;i <= prdvaljdate[0];i++) {
	    if (rjulmdy_1900(i,mdy) < 0) {
	      printf("Error in rjulmdy_1900 trying to make MMDDYYYY.  Program aborted.\n");
	      return(-1);
	    }

	    /* account for the hours */
	    for (ii=iii;ii<=24;ii=ii+itimestep) {
	      /* correct from ii== to ii>=, jerryG, 02/08/00 */
	      /* if (ii==prdhour[0] && prdvaljdate[0]==i) break; */
	      if (ii>=prdhour[0] && prdvaljdate[0]==i) break;
	      strcpy(format,"%-12s%02d%02d  %02d");
	      strcat(format,cfortformat);
	      /* Y2K fix, jerryG, 10/28/99, mdy[2]-1900-> my_yy */
	      if (mdy[2] < 2000)
		my_yy = mdy[2]-1900;
	      else
		my_yy = mdy[2]-2000;
	      fprintf(output,format,
		      station,mdy[0],my_yy,mdy[1],fmissing);
	    }

	    /*reset that initial hour for the rest of the loop*/
	    /*iii=6;*/
	    iii=itimestep;
	  }
	}

				/* merge the new data */
	for(i=itrans;i<=numofprds;i++) {
	  if (rjulmdy_1900(prdvaljdate[i],mdy) < 0) {
	    printf("Error in rjulmdy_1900 trying to make MMDDYYYY.  Program aborted.\n");
	    return(-1);
	  }
	  strcpy(format,"%-12s%02d%02d  %02d");
	  strcat(format,cfortformat);
	  if (prdvalues[i] == -998) {
	    prdvalues[i] = -999; /* -998 is none*/
	    printf("A **NONE** value that was NOT at the beginning of the report cycle for %s %s was set to missing.\n",
		   station,type);
	  }
	  /* Y2K fix, jerryG, 10/28/99, mdy[2]-1900-> my_yy */
	  if (mdy[2] < 2000)
	    my_yy = mdy[2]-1900;
	  else
	    my_yy = mdy[2]-2000;
	  fprintf(output,format,
		  station,mdy[0],my_yy,mdy[1],prdvalues[i]);
	}
      }

      if (begin_yy >= 1900) /* fix 1899 problem, jerryG, 03/08/00 */
        if ( output != NULL ) { fclose(output); output=NULL; }
      strcpy(outfilename,"");
      numofprds=0;
      numofdcs=0;
      strncpy(valbuffer,"          ",10);
    }

    for(i=0;i<=132;i++) {
      strncpy(string+i," ",1);
    }
  }
  /****************************************/
  /* last but not least, check for stations in the file list but not in the report*/

  if (NULL!=(filelistinput=fopen(filelistname, "r"))) {
    printf("...looking for orphan input datacard files...\n");
    while (fgets(fileliststring, sizeof(fileliststring), filelistinput))  {
      p1=strchr(fileliststring,'.');
      p2=fileliststring;
      strncpy(station,fileliststring,p1-p2);
      strcpy(station+(p1-p2),"\n");

      p3=strchr(fileliststring+strlen(station),'.');
      strncpy(type,fileliststring+strlen(station),p3-p1);
      strcpy(type+(p3-p1)-1,"\n");
      iii=0;

      /* now compare against the report data*/
      rewind (prdutilinput);
      while (fgets(string, sizeof(string), prdutilinput))  {
	if (strncmp(string+1,"TIME SERIES ID=",15)==0) {
	  for (i=15;i<=132-10;i++) {
	    if (strncmp(string+i,station,strlen(station)-1) == 0 &&
		strncmp(string+i+strlen(station)-1," ",1)==0) {
	      for (ii=i;ii<=132-10;ii++) {
		/* code folded from here */
		if (strncmp(string+ii,type,strlen(type)-1) == 0 &&
		    strncmp(string+ii+strlen(type)-1," ",1)==0) {
		  /* code folded from here */
		  iii=1;
		  /* unfolding */
		}
		/* unfolding */
	      }
	    }

	  }
	}
      }
      if (iii ==0) {
	printf("There is no data in the PRDUTIL TSDATA report %s for input file %s",
	       argv[1],fileliststring);
	p1=strchr(fileliststring,'\n');
	p2=fileliststring;
	strncpy(fileliststring+(p1-p2),"\0",1);
	strcpy(movestring,"cp ");
	strcat(movestring,fileliststring);
	strcat(movestring," outdir");
	system(movestring);
	printf("Issuing command %s\n",movestring);
	iorf++;
      }
    }
  }
  /****************************************/

  /* normal end*/
  if ( filelistinput != NULL ) { fclose(filelistinput); filelistinput=NULL; }
  if ( prdutilinput != NULL ) { fclose(prdutilinput); prdutilinput=NULL; }
  if ( output != NULL ) { fclose(output); output=NULL; }

  /*system ("rm tmpmr1234");*/
  strcpy(commandstring,"rm ");
  strcat(commandstring,filelistname);
  system(commandstring);

  printf("%d station/types in prdutil report.\n%d prdutil-to-input-datacard matches (per station/type).\n%d new merged datacard files output.\n%d datacard input files without matches in the prdutil report; carried over to outdir.\n",
	 iprd,iold,iout,iorf);
  return(0);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/calb/src/ts2oh_main/RCS/ts2oh.c,v $";
 static char rcs_id2[] = "$Id: ts2oh.c,v 1.18 2007/03/20 17:36:55 dsa Exp $";}
/*  ===================================================  */

}
