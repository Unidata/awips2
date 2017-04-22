/*-------------------------------------------------------------------

   title:    smtime                                                 
                                                                     
   purpose:  To construct observation time for SM.                        
                                                                     
   Author:   David G. Brandon, CBRFC, Salt Lake City, UT             
                                                                     
   Version:  
         1.0   MAR 1994 DGB
               original version 
         1.1   FEB 20, 1996 DGB
               Compare wmo hour and ob hour to determine if 
               year/month/day should be adjusted.
         1.2   MAR 6, 96 DGB
               Use system date/time to time stamp observation.
         1.3   JAN 10 00 DGB
	       Add check_dtg for testing of date/times.
 ---------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "mtr.h"
#include "global_external.h"
extern void check_dtg();
extern int DEBUG, VERBOSE;

void smtime()
{
	static short int iday, imonth, iyear, ihour, imin;
	void shleap();


    /* most sm reports do not include the date/time in the ob */
    /* use system date/time */

    check_dtg();                             /* dgb:01/10/00 */
    iyear  = buffer_.idate[0];
    imonth = buffer_.idate[1];
    iday   = buffer_.idate[2];
    ihour  = buffer_.idate[3];
    imin   = buffer_.idate[4];

    /*  used for testing .. comment in and out
    iyear = 1996;
    imonth = 7;
    iday   = 28;
    ihour  = 12;
    imin   = 29;
    */    

    buffer_.ibuf[5] = ihour*100 + imin;   /* place hr and min in buffer */
    buffer_.idate1[0] = iyear;
    buffer_.idate1[1] = imonth;
    buffer_.idate1[2] = iday;
    buffer_.idate1[3] = ihour;
    buffer_.idate1[4] = imin;
	return;

} /* end of function */
