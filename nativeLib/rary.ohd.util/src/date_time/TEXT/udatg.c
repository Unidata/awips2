/* ================================================================== */
/* pgm: UDATG .. Get system date-time as integers (grennwich time)    */
/*                                                                    */
/* use:     CALL UDATG(JDATE)                                        */
/*                                                                    */
/*  in: JDATE(6) ..... date-time array as follows: - INT              */
/*  in:                  JDATE(1) ... 4-digit year number             */
/*  in:                  JDATE(2) ... julian day number (1-366)       */
/*  in:                  JDATE(3) ... month number (1-12)             */
/*  in:                  JDATE(4) ... day number (1-31)               */
/*  in:                  JDATE(5) ... hours-minutes as 100*hr + min   */
/*  in:                  JDATE(6) ... seconds as 100*sec              */
/* ================================================================== */

#include <stdio.h>
#include <time.h>

void udatg(int time_out[6])
{
struct  tm      *time_pointer;
time_t          tp;

    time(&tp);
    time_pointer = gmtime(&tp);

    time_out[0] = (time_pointer->tm_year < 1900) ? time_pointer->tm_year + 1900
                                                 : time_pointer->tm_year;
    time_out[1] = time_pointer->tm_yday + 1;
    time_out[2] = time_pointer->tm_mon + 1;
    time_out[3] = time_pointer->tm_mday;
    time_out[4] = time_pointer->tm_hour * 100 + time_pointer->tm_min;
    time_out[5] = (time_pointer->tm_sec < 59) ? time_pointer->tm_sec * 100
                                              : 5900;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/date_time/RCS/udatg.c,v $";
 static char rcs_id2[] = "$Id: udatg.c,v 1.2 1998/07/06 14:36:55 dws Exp $";}
/*  ===================================================  */

}
