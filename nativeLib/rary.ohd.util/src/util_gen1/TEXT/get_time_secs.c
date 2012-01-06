/* routine to convert input year, month and day to seconds */

#include <stdio.h>
#include <string.h>
#include <time.h>

time_t get_time_secs (int year, int month, int day)
{
 struct tm utm;
 int 	udate, utime;
 time_t utimet;

   udate=year*10000+month*100+day;

   utm.tm_sec=0;                      /* seconds after the minute - [0,61] */
   utm.tm_min=0;                      /* minutes after the hour - [0,59] */
   utm.tm_hour=0;                     /* hours - [0,23] */
   utm.tm_mday=(udate%100);           /* day of month - [1,31] */
   utm.tm_mon=((udate%10000)/100)-1;  /* month of year - [0,11] */
   utm.tm_year=(udate/10000)-1900;    /* years since 1900 */
   utm.tm_wday=0;                     /* days since Sunday - [0,6] */
   utm.tm_yday=0;                     /* days since January 1 - [0,365] */
   utm.tm_isdst=-1;                   /* daylight savings time flag */

   utimet=mktime(&utm);

   return(utimet);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/get_time_secs.c,v $";
 static char rcs_id2[] = "$Id: get_time_secs.c,v 1.2 2001/02/08 19:27:59 dws Exp $";}
/*  ===================================================  */

}
