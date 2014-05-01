/*==================================================================*/
/*  FUNCTIONS CONTAINED IN THIS FILE:   get_dates()                 */
/*                                      GetCurrentDate(             */
/*                                      GetEarlierDate(             */
/*==================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <stdlib.h>
#include <time.h>

#include "post_stage3.h"
#include "postX.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

/********************************************************************/
/*  FUNCTION NAME:   get_dates()                                    */
/*       FUNCTION:   starting with first date and hour, find        */
/*                     earlier date and hours                       */
/*********************************************************************

Function type:
   void

Called by function:
   main

Functions called:
   GetCurrentDate
   GetEarlierDate

******************************************** BEGIN get_dates ***************/

void get_dates(argdate)
   struct tm   argdate;
{
 int          i;
   
 /* dates is defined in stage3.h as date_struct *dates, NUMHRS is defined in
 stage3.h and was computed from read_papref.c. dstring is defined as char ** in
 stage3.h. GetCurrentDate() and GetEarlierDate() are defined in stage3.h*/
 
 dates = (date_struct *)malloc(NUMHRS*sizeof(date_struct));

 dstring = (char **) malloc((NUMHRS)*sizeof(char *));
 for (i = 0; i < NUMHRS; i++)
    dstring[i] = (char *)malloc(200*sizeof(char));

 for (i=0; i<NUMHRS; i++)
 {
    if (i== 0)
    {
       GetCurrentDate(argdate);
       dates[i] = curdate;
    }
    else
       dates[i] = GetEarlierDate(i);

    strcpy(dstring[i], dates[i].ldate);
    }
}

/********************************************* END get_dates ***************/



/**********************************************/
/*  FUNCTION NAME:  GetCurrentDate()          */
/*       FUNCTION:  fills date structures     */
/*                  date may be specified on command line or current system time  */
/*                    may be used                                                 */
/**********************************************

Function type:
   void

Called by function:
   get_dates

Functions called:
   time
   gmtime

******************************************** BEGIN GetCurrentDate **********/

void GetCurrentDate(argdate)
struct tm   argdate;
{

   struct tm    *t_gm;
   time_t     tnow;
   char         curmonth[4];
   /* curdate is defined in stage3.h, numarg is defined in stage3.h and obtained
   from main_post.c*/
   
 if (date_given != 1)
 {
    time(&tnow);
    t_gm = gmtime(&tnow);
    curdate.hour = t_gm->tm_hour ;
    curdate.month = t_gm->tm_mon + 1;
    curdate.day = t_gm->tm_mday;
    curdate.year = t_gm->tm_year;
    curdate.year = curdate.year + 1900;

    strftime(curdate.ldate,20,"%b %d %Hz",t_gm);
    strftime(curdate.lldate,20,"%b %d %Y %Hz",t_gm);
   /* sprintf(curdate.cdate,"%02d%02d%4d%02d", 
            curdate.month, curdate.day,curdate.year, curdate.hour);*/
	    
    sprintf(curdate.cdate,"%04d%02d%02d%02d", 
            curdate.year, curdate.month,curdate.day, curdate.hour);	    
	    
    sprintf(curdate.dttm, "%d-%02d-%02d %02d:00:00",
            curdate.year,curdate.month,curdate.day,curdate.hour);  	            
 }
 else 
 {   
    /*-------------------------------------------*/
    /*  date and time specified on command line  */
    /*-------------------------------------------*/

    curdate.hour = argdate.tm_hour;
    curdate.day = argdate.tm_mday;
    curdate.month = argdate.tm_mon;
    curdate.year = argdate.tm_year;

    curmonth[3]='\0';
    if(curdate.month == 1)
      strcpy(curmonth,"Jan");
    else if (curdate.month == 2)
      strcpy(curmonth,"Feb");
    else if (curdate.month == 3)
      strcpy(curmonth,"Mar");
    else if (curdate.month == 4)
      strcpy(curmonth,"Apr");
    else if (curdate.month == 5)
      strcpy(curmonth,"May");
    else if (curdate.month == 6)
      strcpy(curmonth,"Jun");
    else if (curdate.month == 7)
      strcpy(curmonth,"Jul");
    else if (curdate.month == 8)
      strcpy(curmonth,"Aug");
    else if (curdate.month == 9)
      strcpy(curmonth,"Sep");
    else if (curdate.month == 10)
      strcpy(curmonth,"Oct");
    else if (curdate.month == 11)
      strcpy(curmonth,"Nov");
    else if (curdate.month == 12)
      strcpy(curmonth,"Dec");

    sprintf(curdate.ldate,"%3s %02d %02dz",curmonth,curdate.day,
                                         curdate.hour);
    sprintf(curdate.lldate,"%3s %02d %4d %02dz",curmonth,curdate.day,
                                              curdate.year,curdate.hour);    					       
   /* sprintf(curdate.cdate,"%02d%02d%4d%02d", 
            curdate.month, curdate.day,curdate.year, curdate.hour);*/
    
    sprintf(curdate.cdate,"%04d%02d%02d%02d", 
            curdate.year, curdate.month,curdate.day, curdate.hour);	    
    sprintf(curdate.dttm, "%d-%02d-%02d %02d:00:00",
            curdate.year,curdate.month,curdate.day,curdate.hour);
 }  
}

/********************************************* END GetCurrentDate **********/



/***************************************************************************/
/*  FUNCTION NAME:   GetEarlierDate()                                      */
/*       FUNCTION:                                                         */
/***************************************************************************

Function type:
   date_struct structure

Called by function:
   get_dates

******************************************** BEGIN GetEarlierDate **********/

date_struct GetEarlierDate(hrs)
   int                  hrs;
{
   date_struct      xdate;
   int               ndaysprev;
   static int          ndays[12] = { 31,28,31,30,31,30,31,31,30,31,30,31};
   static char         *cmonth[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
				    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

 xdate = curdate;
 xdate.hour = curdate.hour - hrs;
 if (xdate.hour < 0)
    {
    ndaysprev = 1;
    xdate.hour = xdate.hour + 24;

    if (xdate.hour < 0)
       {
       xdate.hour = xdate.hour + 24;
       ndaysprev = 2;
       }

    if (xdate.hour < 0)
       {
       xdate.hour = xdate.hour + 24;
       ndaysprev = 3;
       }

    xdate.day = xdate.day - ndaysprev;

    if (xdate.day < 1)
       {
       xdate.month = xdate.month-1;

       if (xdate.month == 2 && xdate.year%4 == 0)
	  xdate.day = 29;
       else if (xdate.month == 0)
	  {
	  xdate.month = 12;
	  xdate.year = xdate.year - 1;
	  xdate.day = ndays[xdate.month-1] + xdate.day;
	  }
       else
	  xdate.day = ndays[xdate.month-1] + xdate.day;
       }
    }

 sprintf(xdate.ldate, "%3s %02d %02dz",      
         cmonth[xdate.month-1], xdate.day, xdate.hour);
 sprintf(xdate.lldate,"%3s %02d %4d %02dz",
         cmonth[xdate.month-1],xdate.day,xdate.year, xdate.hour);
 sprintf(xdate.dttm, "%d-%02d-%02d %02d:00:00",
         xdate.year,xdate.month,xdate.day,xdate.hour);
 /*sprintf(xdate.cdate,"%02d%02d%4d%02d", 
         xdate.month, xdate.day, xdate.year, xdate.hour);*/
	 
  sprintf(xdate.cdate,"%04d%02d%02d%02d", 
         xdate.year, xdate.month, xdate.day, xdate.hour);	 
 
 return xdate;
}

/********************************************* END GetEarlierDate **********/
