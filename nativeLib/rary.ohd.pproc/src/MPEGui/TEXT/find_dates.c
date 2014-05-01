/*=========================================================================*/
/*                         FILE NAME:  find_dates.c                        */
/*                                                                         */
/*  FUNCTIONS CONTAINED IN THIS FILE:  get_dates_RFCW                           */
/*                                     GetCurrentDate_RFCW                      */
/*                                     GetEarlierDate_RFCW                      */
/*=========================================================================*/

/*~~~INCLUDE FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include <List.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "find_dates.h"
#include "get_last_times.h"
#include "mpe_log_utils.h"
#include "stage3.h"
#include "rfcwide.h"
#include "RWResult.h"
#include "time_convert.h"
#include "sqlca.h"
#include "gageqc_defs.h"
#include "time_defs.h"


char save_last[MAX_GAGEQC_DAYS * HOURS_PER_DAY][25];
char exec_last[MAX_GAGEQC_DAYS * HOURS_PER_DAY][25];
char save_manual[MAX_GAGEQC_DAYS * HOURS_PER_DAY][25];

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

/*************************************************************/
/*  FUNCTION NAME:   get_dates_RFCW                               */
/*       FUNCTION:   set up data for dates window            */
/**************************************************************

Function type:
   void

Called by function:
   display_date_window
   show_dates_RFCW

Functions called:
   GetCurrentDate_RFCW 
   GetEarlierDate_RFCW

******************************************** BEGIN get_dates_RFCW ***************/

void get_dates_RFCW ( )
{
   date_struct          current_date;
   extern int           dates_struct_count;
   int          	i ;
   int			comparison_result ;
   int                  search_for_current_date;
   int                  status;
   extern char		RFC [ ] ;
//   static const char	str1 [ 20 ] = "      n/a      "; 
//   static const char	str2 [ 22 ] = "      n/a      ";
   char         	where_rwr [ BUFSIZ ] ;
   char *		obs_time = NULL ; 
   char         	my_ansi [ 20 ] ;
   RWResult *		pRWResult = NULL ;
   RWResult *		pRWResultNode = NULL ;

   
   search_for_current_date = 0;

   if ( dates != NULL )
   {
      /* If the dates structure is not NULL, first store the entry pointed
         to by the dates struct count so that it can be modified once
         the new dates are loaded into the dates array. */ 
      current_date = dates [ dates_struct_count ];
      search_for_current_date = 1;
      free ( dates ) ;
      dates = NULL ;
   }

   dates = ( date_struct * ) malloc ( MAX_GAGEQC_DAYS * HOURS_PER_DAY * sizeof ( date_struct ) ) ;
   
   if ( dates == NULL )
   {
      flogMessage ( stderr , "In routine \"get_dates_RFCW\":\n"
                         "Could not allocate memory for the \"dates\"\n"
                         "variable.  Cannot display date information.\n" ) ;
      return ;
   }

   for(i=0;i<(MAX_GAGEQC_DAYS * HOURS_PER_DAY);i++)
   {
   	dates[i].month=0;
	dates[i].day=0;
	dates[i].year=0;
	dates[i].hour=0;

	memset(dates[i].cdate,'\0',11);
	memset(dates[i].ldate,'\0',20);
	memset(dates[i].lldate,'\0',20);
	memset(dates[i].dttm,'\0',22);
   }
   
   for (i=0; i<(MAX_GAGEQC_DAYS * HOURS_PER_DAY); i++)
    {
    /*     get current date and time                                */
    /*--------------------------------------------------------------*/
    if (i== 0)
    {
      GetCurrentDate_RFCW();
      dates[i] = curdate;
      
    } 

    /*--------------------------------------------------------------*/
    /*     get list of earlier dates                                */
    /*--------------------------------------------------------------*/
    else
    {
      GetEarlierDate_RFCW(i);
      dates[i] = xdate;
      
    }

   }  /* for loop */

   /*----------------------------------------------------------------*/
   /*     read last_save_time and last_exec_time from RWResult table */
   /*----------------------------------------------------------------*/

   sprintf ( where_rwr , "WHERE rfc = '%s' AND obstime BETWEEN "
             "'%s' AND '%s' order by obstime desc", RFC, xdate.dttm, 
             curdate.dttm ) ;

   pRWResult = GetRWResult (where_rwr);

   if ( pRWResult == NULL )
   {
      flogMessage ( stderr , "\nIn routine \"get_dates\":\n"
                         "Could not retrieve last times "
                         "the \"RWResult\" table.\n" ) ;
      return ;
   }

   if ( SQLCODE != 0 )
   {
      flogMessage ( stderr , "\nIn routine \"get_dates\":\n"
                         "Informix error %ld encountered while attempting to\n"
                         "select in RWResult table. Query: \"%s\".\n",
                         SQLCODE , where_rwr ) ;
      FreeRWResult ( pRWResult ) ;
      pRWResult = NULL ;
   }
    else
   {
       pRWResultNode = ( RWResult * ) ListFirst ( & pRWResult->list ) ;

   }      

   for (i=0; i<(MAX_GAGEQC_DAYS * HOURS_PER_DAY); i++)     
   {
        obs_time = dates[i].dttm ;
	if (pRWResultNode != NULL)
	{
	   yearsec_dt_to_ansi( pRWResultNode->obstime, my_ansi);
	   comparison_result = strcmp (obs_time, my_ansi) ;      
	   if ( comparison_result < 0 )
	   {
		memset(save_last[i],'\0',25);
		memset(save_manual[i],'\0',25);
		memset(exec_last[i],'\0',25);
		strcpy(save_last[i],"NA");
		strcpy(exec_last[i],"NA");
		strcpy(save_manual[i],"NA");
                pRWResultNode = ( RWResult * ) ListNext( & pRWResultNode->node);
	   }
	   else if ( comparison_result == 0)
	   {
		yearsec_dt_to_ansi( pRWResultNode->last_save_time, my_ansi);
		memset(save_last[i],'\0',25);
                strcpy(save_last[i],my_ansi);
		yearsec_dt_to_ansi( pRWResultNode->last_exec_time, my_ansi);
	        memset(exec_last[i],'\0',25);
		strcpy(exec_last[i],my_ansi);
		memset(save_manual[i],'\0',25);
		if(!strcmp(pRWResultNode->auto_save,"T"))
		{
			strcpy(save_manual[i],"NO");
		}
		else
		{
			strcpy(save_manual[i],"YES");
		}
		pRWResultNode = ( RWResult * ) ListNext( & pRWResultNode->node);
	   }
	   else
	   {     
		memset(save_last[i],'\0',25);
		memset(save_manual[i],'\0',25);
		memset(exec_last[i],'\0',25);
		strcpy(save_last[i],"NA");
		strcpy(exec_last[i],"NA");
		strcpy(save_manual[i],"NA");
	   }
        }
        else
	{
		memset(save_last[i],'\0',25);
		memset(save_manual[i],'\0',25);
		memset(exec_last[i],'\0',25);
		strcpy(save_last[i],"NA");
		strcpy(exec_last[i],"NA");
		strcpy(save_manual[i],"NA");
	}
   }   

   /* If the current date needs to be found, look for it. */
   if ( search_for_current_date == 1 )
   {
      for ( i = 0; i < ( MAX_GAGEQC_DAYS * HOURS_PER_DAY ); ++i )     
      {
         status = strcmp ( current_date.cdate, dates[i].cdate );
         
         if ( status == 0 )
         {
            break;
         }
      }

      if ( i <  ( MAX_GAGEQC_DAYS * HOURS_PER_DAY ) )
      {
         dates_struct_count = i;
      }
      else
      {
         dates_struct_count = ( MAX_GAGEQC_DAYS * HOURS_PER_DAY ) - 1;
      }
   }

   FreeRWResult ( pRWResult ) ;
   pRWResult = NULL ;

}
/********************************************* END get_dates_RFCW ***************/


/***************************************************************************/
/*  FUNCTION NAME:   GetCurrentDate_RFCW                                   */
/*       FUNCTION:   determine first date and time and place it in curdate */
/*                    structure                                            */
/***************************************************************************

Function type:
   void

Called by function:
   get_dates_RFCW

Functions called:
   time
   gmtime

   date_supplied = number of arguments from command line - determines whether
                   first date in choose dates window is the current date/time
                   or a date/time set by the user.

******************************************** BEGIN GetCurrentDate_RFCW **********/

void GetCurrentDate_RFCW()
{
   struct tm    *t_local;
   time_t       tnow;
   char         curmonth[4];

 if ( date_supplied == 0 )
 {
    time(&tnow);
    t_local = gmtime(&tnow);
    curdate.hour = t_local->tm_hour ;
    curdate.month = t_local->tm_mon + 1;
    curdate.day = t_local->tm_mday;
    curdate.year = t_local->tm_year;
    curdate.year = curdate.year + 1900;

    strftime(curdate.cdate,11,"%Y%m%d%H",t_local);
    strftime(curdate.ldate,20,"%b %d %Hz",t_local);
    strftime(curdate.lldate,20,"%b %d %Y %Hz",t_local);
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

    sprintf(curdate.cdate,"%4d%02d%02d%02d",curdate.year,curdate.month,
                                         curdate.day,curdate.hour);
    sprintf(curdate.ldate,"%3s %02d %2dz",curmonth,curdate.day,
                                         curdate.hour);
    sprintf(curdate.lldate,"%3s %02d %4d %2dz",curmonth,curdate.day,
                                              curdate.year,curdate.hour);
 }

 sprintf(curdate.dttm, "%d-%02d-%02d %02d:00:00",curdate.year,curdate.month,curdate.day,curdate.hour);
}

/********************************************* END GetCurrentDate_RFCW **********/



/***************************************************************************/
/*  FUNCTION NAME:   GetEarlierDate_RFCW                                        */
/*       FUNCTION:   find list of earlier dates and times and place in     */
/*                    dates structure                                      */
/***************************************************************************

Function type:
   void

Called by function:
   get_dates_RFCW

Functions called:
   none

******************************************** BEGIN GetEarlierDate_RFCW **********/

void GetEarlierDate_RFCW(int hrs)
{
   struct tm current_datetime ;
   struct tm * pDatetime = NULL ;
   time_t current_date_in_ticks ;
   time_t previous_date_in_ticks ;
   
   /*--------------------------------------------------------------*/
   /*     set xdate to current date - number of hours back from    */
   /*     current                                                  */
   /*--------------------------------------------------------------*/
   xdate = curdate;

   current_datetime.tm_year = xdate.year - 1900 ;
   current_datetime.tm_mon = xdate.month - 1 ;
   current_datetime.tm_mday = xdate.day ;
   current_datetime.tm_hour = xdate.hour ;
   current_datetime.tm_min = 0 ;
   current_datetime.tm_sec = 0 ;

   current_date_in_ticks = gm_mktime ( & current_datetime ) ;

   if ( current_date_in_ticks == ( time_t ) -1 )
   {
      flogMessage ( stderr , "\nIn routine \"GetEarlierDate_RFCW\":\n"
                         "The attempt to convert the \"current datetime\"\n"
                         "to ticks has failed. The values in the\n"
                         "current_date structure are: year %4d,\n"
                         "month %02d, day %02d, and hour %02d.\n" ,
                         xdate.year , xdate.month , xdate.day , xdate.hour ) ;
      return ;
   }

   /* Subtract the number of hours back from the current time of 
      the selected product.  Note that 3600 is the number of seconds
      in an hour. */
   previous_date_in_ticks = current_date_in_ticks - ( hrs * 3600 ) ;
   pDatetime = gmtime ( & previous_date_in_ticks ) ;

   if ( pDatetime == NULL )
   {
      flogMessage ( stderr , "\nIn routine \"GetEarlierDate_RFCW\":\n"
                         "The attempt to convert the previous time\n"
                         "ticks value into a \"struct tm\" has failed.\n"
                         "The previous datetime ticks value is %ld.\n" ,
                         previous_date_in_ticks ) ;
      return ;
   }


   /* Set xdate's fields with the year, month, day, and hour information
      which represents the previous datetime. */
   xdate.year = pDatetime->tm_year + 1900 ;
   xdate.month = pDatetime->tm_mon + 1 ;
   xdate.day = pDatetime->tm_mday ;
   xdate.hour = pDatetime->tm_hour ;

   /*--------------------------------------------------------------*/
   /*     set date strings                                         */
   /*--------------------------------------------------------------*/

   memset ( xdate.cdate , '\0' , 11 ) ;
   strftime ( xdate.cdate , 11 , "%Y%m%d%H" ,  pDatetime ) ;

   memset ( xdate.ldate , '\0' , 20 ) ;
   strftime ( xdate.ldate , 20 , "%b %d %Hz" , pDatetime ) ;

   memset ( xdate.lldate , '\0' , 20 ) ;
   strftime ( xdate.lldate , 20 , "%b %d %Y %Hz" , pDatetime ) ;

   memset ( xdate.dttm , '\0' , 22 ) ;
   strftime ( xdate.dttm , 22 , "%Y-%m-%d %H:00:00" , pDatetime ) ;

   return ;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/find_dates.c,v $";
 static char rcs_id2[] = "$Id: find_dates.c,v 1.15 2007/02/22 16:05:42 lawrence Exp $";}
/*  ===================================================  */

}

/********************************************* END GetEarlierDate_RFCW **********/
