/* ================================================================== */
/* pgm: UDATLD.. Return a daylight Saving Time flag for either the    */
/*                current date/time or a specified date/time          */
/*                                                                    */
/* use:     CALL UDATLD2(JDATE)                                       */
/*  NOTE: First array element is used to signal use current date/time */
/*                                                                    */
/*  Assume it's called from FORTRAN so this is the FORTRAN view:      */
/*                                                                    */
/*  in/out:JDATE(7) ..... date-time array as follows: - INT           */
/*  in/out:              JDATE(1) ... 4-digit year number [or 0]      */
/*  in/out:              JDATE(2) ... julian day number (1-366)       */
/*  in/out:              JDATE(3) ... month number (1-12)             */
/*  in/out:              JDATE(4) ... day number (1-31)               */
/*  in/out:              JDATE(5) ... hours-minutes as 100*hr + min   */
/*  in/out:              JDATE(6) ... seconds as 100*sec              */
/*  out:                 JDATE(7) ... is_dst (0=no, 1=yes)            */
/* ================================================================== */

#include <stdio.h>
#include <time.h>
#define TIME_LENGTH 100

void udatld(int time_out[6])
{
struct  tm      *time_pointer;
time_t          tp;
char    time_string [TIME_LENGTH];


    if ( time_out[0] != 0)      /*  time is supplied  */
    { 
      time_pointer = localtime(&tp);  /* initialize struct */

      time_pointer->tm_year = time_out[0] - 1900; /* for now assume > 1900 */
      time_pointer->tm_yday = time_out[1] - 1;
      time_pointer->tm_mon  = time_out[2] - 1;
      time_pointer->tm_mday = time_out[3];
      time_pointer->tm_hour = time_out[4]/100;
      time_pointer->tm_min = time_out[4]%100;

      time_pointer->tm_sec = time_out[5]/100;
      time_pointer->tm_isdst = -1;
    
      tp = mktime(time_pointer);
    }  
    else   /* use current time */
    {
    	time(&tp);
    }

    time_pointer = localtime(&tp);
/*
    strftime ( time_string, TIME_LENGTH, "%Y-%m-%d %H:%M:%S", time_pointer );
    printf ( "\nIn udatld2: Output time:\t%s\n", time_string );
*/    
    time_out[0] = (time_pointer->tm_year < 1900) ? time_pointer->tm_year + 1900
                                                 : time_pointer->tm_year;
    time_out[1] = time_pointer->tm_yday + 1;
    time_out[2] = time_pointer->tm_mon + 1;
    time_out[3] = time_pointer->tm_mday;
    time_out[4] = time_pointer->tm_hour * 100 + time_pointer->tm_min;
    time_out[5] = (time_pointer->tm_sec < 59) ? time_pointer->tm_sec * 100
                                              : 5900;
    time_out[6] = time_pointer->tm_isdst;
    		      

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/udatld.c,v $";
 static char rcs_id2[] = "$Id: udatld.c,v 1.1 2006/05/12 17:24:28 jgofus Exp $";}
/*  ===================================================  */

}
