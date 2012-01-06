/*
---------------------------------------------------------------------
																	   
	title:    metar_ob_time													   
																	   
	purpose:  Main function used to construct observation date/time. 

    execution:The function is called by shef_it_metar. 

	Author:   David G. Brandon, CBRFC, Salt Lake City, UT			   
																	   
	Version:  1.0  FEB 18, 95  DGB										   
				   Original											   
              1.1  MAY  1, 96  DGB
                   Include a small tolerance (time interval) for
                   determing observations times.  Some obs arrive
                   with observation times a few minutes in the
                   future (as compared to the system clock).  This
                   could result in slight differences in system
                   clock times.  A tolerance of 60 minutes was
                   chosen.  Changed the method that times are
                   computed.
              1.2  JUN 8 96
                   Changed the way that the observation day for the 
                   METAR ob was determined.  The day is computed from
                   the system time.  If the system time is greater
                   than the ob time, the system day is used.  If
                   the system time is less than the ob time, the 
                   system day is decremented.  The specific day 
                   in the METAR ob, if provided, is not used.  However,
                   a check is made between the computed system day and
                   the day provided in the METAR ob.  If different, 
                   a WARNING is printed.  The major difference is that
                   the day in the METAR ob is not explicitly used any
                   longer.
              1.3  JUL 12 96
                   Check for bad observation time.
              1.4  DEC 16 96 DGB
                   Declare funtion ouptime() as a long (required for
                   msdos version);
	      1.5  JAN 10 00 DGB
                   Changed the way that observation times are
		   computed.  There are two cases based on:
		   (1)  the day is included in the observation
		   (2)  the day not included in the observation
		   
		   If the day is not included, the system year,
		   month and day are used along with the observation
		   time.
		   
		   If the day is included, the system year and month
		   are used.  If the day in the ob is > than the 
		   system day, the month is decremented ( and the 
		   year if necessary).  If the day in the ob is 
		   equal to the system day, then if the ob time is
		   greater than the system time, a warning is 
		   printed.
		   	           
	      1.6  Dec 01 00 DGB
	           Return (0) at bottom of function...some compilers
		   such gcc caused a dump at runtime.
	      1.7  Sep 16 01 DGB
	           Change <metar.h> to "metar.h"
																	   
---------------------------------------------------------------------
*/ 

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include "mtr.h"
#include "global_external.h"
#include "metar.h"
extern void check_dtg();
extern int VERBOSE, DEBUG;
extern int AO2, AO2A, RAMOS, AWOS, AMOS, OTHER, SA, SP, RS, SM, REMARKS; 

long ouptime(int,int,int,int,int,int);              /* dgb:12/16/96 */

void prtDMETR( Decoded_METAR * );

int metar_ob_time( Decoded_METAR *Mptr )
{ 

  struct tm tmm;
  int TIME_TOLERANCE = 100;

  long time_seconds;

  static short int iday, imonth, iyear, ihour, imin, isec, 
                     ob_time, system_time;


    /* for testing - faking in system date/time 
    buffer_.idate[0] = 2000;
    buffer_.idate[1]  = 1;
    buffer_.idate[2] = 31;
    buffer_.idate[3] = 23;
    buffer_.idate[4] = 58;
    */    

    iyear  = buffer_.idate[0];
    imonth = buffer_.idate[1];
    iday   = buffer_.idate[2];
    ihour  = buffer_.idate[3];
    imin   = buffer_.idate[4];
    isec   = 0;

    /* check to see if the -x switch is on for overriding date/time */
    check_dtg( );    

    if ( Mptr->ob_date == MAXINT )                          /* dgb:01/10/00 */
    {

       system_time = ihour*100 + imin;
       if ( Mptr->ob_hour > 100 || Mptr->ob_minute > 100 )
            return(1);                                      /* dgb:07/12/96 */

       ob_time     = Mptr->ob_hour*100 + Mptr->ob_minute;

       time_seconds = ouptime(iyear,imonth,iday,ihour,imin,isec);
       tmm=*gmtime(&time_seconds);

       iyear = tmm.tm_year + 1900;
       imonth= tmm.tm_mon+ 1;
       iday  = tmm.tm_mday;
       ihour = tmm.tm_hour;
       imin  = tmm.tm_min;


       /* compare obs and system time to see if day should be decremented */
       if ( ob_time < system_time )
       {
          if ( (system_time - ob_time) > 2300 )
                iday++;
       }
       else
       {
          if ( (ob_time - system_time) >= TIME_TOLERANCE )  
               iday--;
       }

       time_seconds = ouptime(iyear,imonth,iday,ihour,imin,isec);
       tmm = *gmtime(&time_seconds);

       iyear = tmm.tm_year + 1900;
       imonth= tmm.tm_mon + 1;
       iday  = tmm.tm_mday;
       ihour = tmm.tm_hour;
       imin  = tmm.tm_min;
  
       buffer_.idate1[0] = iyear;
       buffer_.idate1[1] = imonth;
       buffer_.idate1[2] = iday;

       /* parse off obs time hr and min */
       buffer_.idate1[3]  = Mptr->ob_hour;
       buffer_.idate1[4]  = Mptr->ob_minute;

       /* repack adjusted values */
       buffer_.idate1[1] = imonth;
       buffer_.idate1[2] = iday;
       buffer_.idate1[0] = iyear;

       /* check the computed day from the system time against the 
       day in the METAR ob (if one is present).  All METAR obs
       do not send the day.  If they are different, print an
       error message.
       */

       if ( Mptr->ob_date != MAXINT )               /* dgb:06/08/96 */
       {
          if ( iday != Mptr->ob_date )
          {
             fprintf(luns_.icher,"\nWARNING:observation day=%d computed from system is different from",iday);
             fprintf(luns_.icher,"\n        day = %d provided in METAR ob", Mptr->ob_date);
             if ( VERBOSE )
             {
                fprintf(stdout,"\nWARNING:observation day=%d computed from system is different from",iday);
                fprintf(stdout,"\n        day = %d provided in METAR ob",Mptr->ob_date);
             }
          }
       }
    }                                                       /* dgb:01/10/00 */
    else                                                    /* dgb:01/10/00 */
    {                                                       /* dgb:01/10/00 */

       /* parse off obs day, hour, and minute */
       buffer_.idate1[0] = buffer_.idate[0]; /* year   */   /* dgb:01/10/00 */
       buffer_.idate1[1] = buffer_.idate[1]; /* month  */   /* dgb:01/10/00 */
       buffer_.idate1[2] = Mptr->ob_date;    /* day    */   /* dgb:01/10/00 */
       buffer_.idate1[3] = Mptr->ob_hour;    /* hour   */   /* dgb:01/10/00 */
       buffer_.idate1[4] = Mptr->ob_minute;  /* minute */   /* dgb:01/10/00 */

       system_time = ihour*100 + imin;                      /* dgb:01/10/00 */
       if ( Mptr->ob_hour > 100 || Mptr->ob_minute > 100 )  /* dgb:01/10/00 */
            return(1);                                      /* dgb:07/12/96 */

       ob_time     = Mptr->ob_hour*100 + Mptr->ob_minute;   /* dgb:01/10/00 */

       if ( Mptr->ob_date > buffer_.idate[2] )              /* dgb:01/10/00 */
       {                                                    /* dgb:01/10/00 */
          buffer_.idate1[1] = buffer_.idate1[1] - 1;        /* dgb:01/10/00 */
	  if ( buffer_.idate1[1] == 0 )                     /* dgb:01/10/00 */
	  {                                                 /* dgb:01/10/00 */
	     buffer_.idate1[1] = 12;                        /* dgb:01/10/00 */
	     buffer_.idate1[0] = buffer_.idate1[0] - 1;     /* dgb:01/10/00 */
	  }                                                 /* dgb:01/10/00 */
       }                                                    /* dgb:01/10/00 */
       else                                                 /* dgb:01/10/00 */
       if ( Mptr->ob_date == buffer_.idate[2] )             /* dgb:01/10/00 */
       {                                                    /* dgb:01/10/00 */ 
          if ( ob_time > system_time )                      /* dgb:01/10/00 */
	  {                                                 /* dgb:01/10/00 */
             fprintf(luns_.icher,
	     "\nWARNING:observation time is greater than the system time for the same day"); /* dgb:01/10/00 */
             fprintf(luns_.icher,
	     "\n        observation time = %04d system time = %04d day = %d", 
	       ob_time, system_time, Mptr->ob_date);        /* dgb:01/10/00 */
             if ( VERBOSE )                                 /* dgb:01/10/00 */
             {                                              /* dgb:01/10/00 */
                fprintf(stdout,
		"\nWARNING:observation time is greater than the system time for the same day"); /* dgb:01/10/00 */
                fprintf(stdout,
		"\n        observation time = %04d system time = %04d day = %d", 
	             ob_time, system_time, Mptr->ob_date);   /* dgb:01/10/00 */

             }                                              /* dgb:01/10/00 */
	  }                                                 /* dgb:01/10/00 */
       }                                                    /* dgb:01/10/00 */  
    }                                                       /* dgb:01/10/00 */
    return(0);                                            /* dgb:12/01/00 */
}
