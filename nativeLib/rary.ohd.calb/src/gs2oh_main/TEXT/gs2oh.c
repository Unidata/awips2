/* ***************************************************************** */
/*                                                            	     */
/*      Program:        gs2oh.c                                      */
/*	Description:    The gs2oh program version 2.2 converts USGS  */
/*                      time series to OH Datacard format. It will   */
/*                      convert only Daily mean streamflow data.     */
/*                      The output can be in 1-column, 4-column or   */
/*                      6-column format.                             */
/*                                                                   */
/*                      It includes 2 date functions, rjulmdy_1900   */
/*                      and rmdyjul_1900 (which call lower level     */
/*                      routines ddgcj_c.c, ddgdj2_c.c, ddgjc_c.c,   */
/*                      ddgjd2_c.c).                                 */
/*                                                                   */
/*      Modified by:    Loubna Bousaidi on 04/10/04                  */
/*      Modified by:    Dave St. on 2005-09-15                       */
/*                                                                   */
/* ***************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define MAX_LINE  200
#define TRUE    1
#define FALSE   0

extern int rjulmdy_1900(long, short *);
extern int rmdyjul_1900(short *, long *);

FILE *input;
FILE *output;
char string[MAX_LINE];
char answer;
float conval= 1;
long nextline=0;
char units[5];
char type[5];
char dim[3];
char name[27];
char station[9];
char format[5];
char fortformat[5];
short convertsw=0;
int i=3 ;
int nIndex, bdate=0;
char bmonth[3];
char emonth[3];
char byear[5];
char eyear[5];
long ii, istart=0;
char decade[3];
char cmonth[3];
char cyear[5];
char cday[3];
char cvalue[20];
long jdate, prevjdate;
float value[40], repvalue;
long b;
short mdy[3];           /* mdy[0]=month, mdy[1]=day, mdy[2]=year */
short mpy[3];           /* mpy[0]=month, mpy[1]=day, mpy[2]=year */
int my_yr=0;
short idays;

void print_data();
void print_missing_data();
void print_header();
void get_input();

int gs2oh_main(int argc, const char ** argv)
{

   if (argc == 1)
   {
      printf("\n");
      printf("gs2oh translator version ob8.1 03/20/07\n");
      printf("Return Code 8 on an error, Zero on a normal exit.\n");
      printf("Metric conversion switch can now be -m as well as -cms.\n");
      printf("Program will be converting only USGS Daily mean streamflow time\n");
      printf("series data to OH Datacard format data.\n");
      printf("This program converts USGS time series to a one column, 4 column\n");
      printf("or 6 column OH Datacard format accordingly to the user choice.\n");
      printf("All USGS data may be obtained from http://nwis.waterdata.usgs.gov/usa/nwis \n");
      printf("\n");
      printf("Usage: gs2oh infile outfile -option -option\n");
      printf("Options: -m  Converts English units to Metric units. Default is English units.\n");
      printf("         -Fn.n Prints values in the specified Fortran format.  Default is F9.3\n");
      printf("               n must be < 10\n");
      exit(8);
   }


   if (NULL==(input=fopen(argv[1], "r")))
   {
      printf("Error: Can't open file %s!\n", argv[1]);
      exit(8);
   }

   if (NULL==(output=fopen(argv[2], "w")))
   {
      printf("Error opening output file %s\n", argv[2]);
      exit(8);
   }

   printf("\n****************************************************\n");
   printf("Make sure that you enter at the command line\n");
   printf(" gs2ohe input_file output_file [-option] [-option] \n");
   printf("The output can be in 1-column, 4-column or 6-column OH Datacard format.\n");
   printf("Please type one of the following numbers 1, 4 or 6 and press enter: ");

   do
   {
     answer=getchar();
   }
   while( answer != '1' && answer != '4' && answer != '6');

   /* initialise data type, units, dim and format*/

  strcpy(fortformat,"F9.3");
  strcpy(units,"????");
  strcpy(type,"????");
  strcpy(dim, "??");
  strcpy(format,"%9.3f");
  repvalue = -999;

  /* read the arguments for the conversion from English units to Metric and also */
  /* to specify the fortran format used.*/
  while ((i <=4) && (argv[i] !=NULL))
  {
    if (strcmp(argv[i],"-cms")==0 || strcmp(argv[i], "-CMS")==0  || strcmp(argv[i], "-M")==0  || strcmp(argv[i], "-m")==0)
    {
       convertsw=1;
    }

    if (strncmp(argv[i], "-F",2)==0 ||  strncmp(argv[i], "-f",2)==0)
    {
       strncpy(format+1,argv[i]+2,3);
       strncpy(format+4,"f", 1);
       strncpy(fortformat+1,argv[i]+2,3);
    }

    i++;
  }

  get_input();

  fclose(input);
  fclose(output);
  exit(1);

}
/* ********************************************************************** */
/* void get_input() function is used to read the USGS input file data     */
/* ********************************************************************** */

void get_input()
{
  while (fgets(string, MAX_LINE , input))
  {
    nextline++;
    if (strlen(string)>200)
    {
       printf("error in input file, rec#%d. Length is %d\n",nextline,strlen(string));
       exit(8);
    }

    if (nextline==1 && strstr(string, "# U.S. Geological Survey") == NULL)
    {
       printf("The input file doesn't seem to be a USGS time series.\n");
       exit(8);
    }

    print_header();

    if ((strncmp(string,"USGS",4)==0)&& (strncmp(string,"#",1)!=0))
    {
       istart++;
       strncpy(decade,string+16,2);
       decade[2]='\0';
       strncpy(cyear,string+14,4);
       cyear[4]='\0';
       strncpy(cmonth,string+19,2);
       cmonth[2]='\0';
       strncpy(cday,string+22,2);
       cday[2]='\0';
       strncpy(cvalue,string+24,20);
       cvalue[20]='\0';

   /*reinitialize then check for missing day slots and make a record for them*/

       mdy[0]=atoi(cmonth);
       mdy[1]=atoi(cday);
       mdy[2]=atoi(cyear);


       b=rmdyjul_1900(mdy,&jdate);

       if (istart == 1)
           prevjdate = jdate - (mdy[1]);

       while ( prevjdate+1< jdate )
       {
         if (rjulmdy_1900(prevjdate+1,mpy) < 0)
         {
	    printf("Error in rjulmdy_1900 trying to make MMDDYYYY.  Program aborted.\n");
            exit(8);
	 }

	    value[mpy[1]] = repvalue;

	 if (mpy[2]-1900 >=100)
	     my_yr = mpy[2]-2000;
	 else
	     my_yr = mpy[2]-1900;


          if (answer == '1' || answer == '4' || answer == '6')
	  {
	     print_missing_data();
             prevjdate++;
	  }
       }


       value[mdy[1]] = atof(cvalue);
       value[mdy[1]] = value[mdy[1]]*conval;

       if (mdy[2]-1900 >=100)
	  my_yr = mdy[2]-2000;
       else
          my_yr = mdy[2]-1900;

       if (answer == '1' || answer == '4' || answer == '6')
       {
	  print_data();
          prevjdate = jdate;
       }
    }
  }
}

/* *********************************************************************** */
/* void print_header() function is used to print the header for the output */
/* *********************************************************************** */

void print_header()
{
  if (strstr(string, "#") !=NULL)
  {
     if (nextline<23 || nextline>30)
        fprintf(output, "$%s", string);

  /*obtain the data type */

     if (strstr(string,"daily mean streamflow data.") !=NULL)
	  strcpy(type,"QME ");

     if (strstr(string, "#  USGS") !=NULL)
	  strncpy(name,string+17,26);
          name[26]= '\0';

     if (strstr(string, "#  USGS") !=NULL)
          strncpy(station,string+8,9);
          station[9]='\0';

  /* cfsd to cmsd for Discharge */

    if (strstr(string,"cubic-feet per-second") !=NULL)
    {
	  strcpy(units,"CFSD");
	  strcpy(dim,"L3");
	  if (convertsw==1)
	  {
	     conval=.02831;
	     strcpy(units,"CMSD");
	  }
    }

  /*print the header*/
    if ( nextline  == 21)
    {
	if (strncmp(units,"????",4)==0 && strncmp(dim,"??",2)==0)
	{
	    printf("Data unit cannot be determined from the USGS header information.\n");
	    printf("Please check your USGS input file.\n");
            exit(8);
	}

        fprintf(output,"$**** The OH DATACARD format follows:\n");
        fprintf(output,"              %-4s %-4s %-4s 24   %-14s%s  %s\n",type,dim,units,station,name,fortformat);
    }
}
}

/*********************************************************************** */
/*      void print_data() function is called whenever the user selects   */
/*      1, 4 or 6-column OH Datacard format. It prints the data values.  */
/*********************************************************************** */

void print_data()
{
  switch ( answer)
  {
    case '1':
      fprintf(output,"%-11s %02d%02d   0", station,mdy[0],my_yr);
      fprintf(output,format,value[mdy[1]]);
      fprintf(output,"\n");
      break;

   case '4':
     if (mdy[1]==1 || mdy[1]==5 || mdy[1]==9 || mdy[1]==13 || mdy[1]==17 || mdy[1]==21 || mdy[1]==25 || mdy[1]==29)
	fprintf(output,"%-11s %02d%02d   0", station,mdy[0],my_yr);
        fprintf(output,format,value[mdy[1]]);

     if (mdy[0]==2)
     {
	idays=28;
	if (((!(mdy[2]%4)&&(mdy[2]%100))||!mdy%400))
	      idays=29;
	      if (mdy[1]==29)
	          fprintf(output,"\n");
     }

     if ((mdy[0]==4 || mdy[0]==6 || mdy[0]==9 || mdy[0]==11) && (mdy[1]==30))
     {
	 idays=30;
	 fprintf(output,"\n");
     }

     if ( mdy[1]==4 || mdy[1]==8 || mdy[1]==12 || mdy[1]==16 || mdy[1]==20 || mdy[1]==24 ||mdy[1]==28 || mdy[1]==31)
	 fprintf(output,"\n");

    break;

   case '6':
     if (mdy[1]==1 || mdy[1]==7 || mdy[1]==13 || mdy[1]==19 || mdy[1]==25 || mdy[1]==31)
        fprintf(output,"%-11s %02d%02d   0", station,mdy[0],my_yr);
        fprintf(output,format,value[mdy[1]]);

     if (mdy[0]==2)
     {
	idays=28;
        if (((!(mdy[2]%4)&&(mdy[2]%100))||!mdy%400))
	      idays=29;;
	      if (mdy[1]==29)
		 fprintf(output,"\n");
	else
	      idays=28;
	      if ((mdy[1]==28) && (mdy[2]%4) !=0)
	         fprintf(output,"\n");
     }

     if (mdy[0]==4 || mdy[0]==6 || mdy[0]==9 || mdy[0]==11)
	idays=30;
     if ( mdy[1]==6 || mdy[1]==12 || mdy[1]==18 || mdy[1]==24 || mdy[1]==30 || mdy[1]==31)
	fprintf(output,"\n");

     break;

  default:
     printf("no options were selected");
  }
}
/*********************************************************************** */
/*      void print_missing_data() function is called whenever the user   */
/*      selects 1, 4 or 6-column OH Datacard format. It prints the       */
/*      missing data values.                                             */
/*********************************************************************** */

void print_missing_data()
{
  switch ( answer)
  {
    case '1':
      fprintf(output,"%-11s %02d%02d   0", station,mpy[0],my_yr);
      fprintf(output,format,value[mpy[1]]);
      fprintf(output,"\n");
      break;

   case '4':
     if (mpy[1]==1 || mpy[1]==5 || mpy[1]==9 || mpy[1]==13 || mpy[1]==17 || mpy[1]==21 || mpy[1]==25 || mpy[1]==29)
	fprintf(output,"%-11s %02d%02d   0", station,mpy[0],my_yr);
        fprintf(output,format,value[mpy[1]]);

     if (mpy[0]==2)
     {
	idays=28;
	if (((!(mpy[2]%4)&&(mpy[2]%100))||!mpy%400))
	      idays=29;
	      if (mpy[1]==29)
          	 fprintf(output,"\n");
     }

     if ((mpy[0]==4 || mpy[0]==6 || mpy[0]==9 || mpy[0]==11) && (mpy[1]==30))
     {
         idays=30;
	 fprintf(output,"\n");
     }

     if ( mpy[1]==4 || mpy[1]==8 || mpy[1]==12 || mpy[1]==16 || mpy[1]==20 || mpy[1]==24 || mpy[1]==28 || mpy[1]==31 )
	fprintf(output,"\n");

     break;

   case '6':
     if (mpy[1]==1 || mpy[1]==7 || mpy[1]==13 || mpy[1]==19 || mpy[1]==25 || mpy[1]==31)
        fprintf(output,"%-11s %02d%02d   0", station,mpy[0],my_yr);
        fprintf(output,format,value[mpy[1]]);

     if (mpy[0]==2)
     {
          idays=28;
          if (((!(mpy[2]%4)&&(mpy[2]%100))||!mpy%400))
                idays=29;
	  	if (mpy[1]==29)
                   fprintf(output,"\n");
	  else
		idays=28;
	        if ((mpy[1]==28) && (mpy[2]%4) !=0)
	           fprintf(output,"\n");
     }

     if (mpy[0]==4 || mpy[0]==6 || mpy[0]==9 || mpy[0]==11)
        idays=30;
     if (mpy[1]==6 || mpy[1]==12 || mpy[1]==18 || mpy[1]==24 || mpy[1]==30 || mpy[1]==31)
     fprintf(output,"\n");

     break;
  }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/calb/src/gs2oh_main/RCS/gs2oh.c,v $";
 static char rcs_id2[] = "$Id: gs2oh.c,v 1.16 2007/03/20 17:16:26 dsa Exp $";}
/*  ===================================================  */

}


